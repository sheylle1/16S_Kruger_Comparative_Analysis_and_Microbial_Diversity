# Alpha Diversity Analysis with Complete Statistical Tests for All Tables
# Generates Tables S3, S4, S5, S6 for manuscript

library(tidyverse)
library(ggpubr)
library(rstatix)
library(FSA)

# Load data
entropy_data <- readxl::read_excel("data/raw/Entropydata.xlsx", sheet = "alpha-diversity")

# Define outlier samples (matching your PERMANOVA outliers)
outliers <- c("K058700-R", "K058796-R", "K058824-R", "K058810-R", "K058694-R")

# Clean data and set factor levels
entropy_clean <- entropy_data %>%
  filter(!Sample_id %in% outliers) %>%
  mutate(Diet_type = factor(Diet_type, levels = c("Carnivore", "Non-Ruminant", "Ruminant")))

# Get total sample count (n=89 as in your tables)
total_n <- nrow(entropy_clean)
cat("Total samples:", total_n, "\n")

# ============================================================================
# TABLE S3: Kruskal-Wallis Tests Comparing Alpha Diversity Among Dietary Groups
# ============================================================================

# Function to run Kruskal-Wallis and format results
run_kruskal <- function(data, metric, metric_name) {
  test <- kruskal.test(reformulate("Diet_type", metric), data = data)
  result <- data.frame(
    Metric = metric_name,
    n = nrow(data),
    statistic = round(test$statistic, 8),
    df = test$parameter,
    p = formatC(test$p.value, format = "e", digits = 2),
    method = "Kruskal-Wallis"
  )
  return(result)
}

# Run for all three metrics
table_s3 <- bind_rows(
  run_kruskal(entropy_clean, "shannon_entropy", "Shannon_entropy"),
  run_kruskal(entropy_clean, "faith_pd", "Faith_PD"),
  run_kruskal(entropy_clean, "observed_features", "Observed_features")
)

print(table_s3)
write.csv(table_s3, "outputs/tables/Table_S3_Kruskal_Wallis.csv", row.names = FALSE)

# ============================================================================
# TABLE S4: Pairwise Dunn's Tests for Alpha Diversity Among Dietary Groups
# ============================================================================

# Function to run Dunn test and format results
run_dunn_table <- function(data, metric, metric_name) {
  dunn_result <- dunnTest(reformulate("Diet_type", metric), 
                          data = data, 
                          method = "bh")
  
  dunn_df <- dunn_result$res
  colnames(dunn_df) <- c("Comparison", "Z", "P.unadj", "P.adj")
  
  # Extract group names and sample sizes
  dunn_df$Group1 <- sapply(strsplit(dunn_df$Comparison, " - "), `[`, 1)
  dunn_df$Group2 <- sapply(strsplit(dunn_df$Comparison, " - "), `[`, 2)
  
  # Add sample sizes
  n1 <- sapply(dunn_df$Group1, function(g) sum(data$Diet_type == g, na.rm = TRUE))
  n2 <- sapply(dunn_df$Group2, function(g) sum(data$Diet_type == g, na.rm = TRUE))
  
  # Add significance symbols
  dunn_df$p.adj.signif <- case_when(
    dunn_df$P.adj < 0.0001 ~ "****",
    dunn_df$P.adj < 0.001 ~ "***",
    dunn_df$P.adj < 0.01 ~ "**",
    dunn_df$P.adj < 0.05 ~ "*",
    TRUE ~ "ns"
  )
  
  result <- data.frame(
    Metric = metric_name,
    Group1 = dunn_df$Group1,
    Group2 = dunn_df$Group2,
    n1 = n1,
    n2 = n2,
    estimate = round(dunn_df$Z, 3),
    statistic = round(dunn_df$Z, 3),
    p = dunn_df$P.unadj,
    p.adj = dunn_df$P.adj,
    p.adj.signif = dunn_df$p.adj.signif,
    method = "Dunn Test"
  )
  
  return(result)
}

# Run for all three metrics
table_s4 <- bind_rows(
  run_dunn_table(entropy_clean, "shannon_entropy", "Shannon entropy"),
  run_dunn_table(entropy_clean, "faith_pd", "Faith PD"),
  run_dunn_table(entropy_clean, "observed_features", "Observed features")
)

print(table_s4)
write.csv(table_s4, "outputs/tables/Table_S4_Dunn_Tests.csv", row.names = FALSE)

# ============================================================================
# TABLE S5: Pairwise Comparisons of Shannon Index Among Host Species
# ============================================================================

cat("\n\n========== TABLE S5: SHANNON - SPECIES PAIRWISE COMPARISONS ==========\n")

# Get all species with their sample sizes
species_counts <- entropy_clean %>%
  group_by(Common.name_Species) %>%
  summarise(N = n(), .groups = "drop") %>%
  arrange(desc(N))

cat("\nSpecies with sample sizes:\n")
print(species_counts)

# Function to run pairwise Wilcoxon tests for all species combinations
run_species_pairwise <- function(data, metric, metric_name) {
  
  # Get all unique species
  species_list <- unique(data$Common.name_Species)
  species_list <- species_list[!is.na(species_list)]
  
  # Create all pairwise combinations
  pairs <- combn(species_list, 2, simplify = FALSE)
  
  results <- list()
  
  for(pair in pairs) {
    g1 <- pair[1]
    g2 <- pair[2]
    
    # Subset data for these two species
    sub_data <- data %>% filter(Common.name_Species %in% c(g1, g2))
    
    # Get sample sizes
    n1 <- sum(sub_data$Common.name_Species == g1)
    n2 <- sum(sub_data$Common.name_Species == g2)
    
    # Run Wilcoxon rank-sum test
    wilcox_result <- wilcox.test(reformulate("Common.name_Species", metric), 
                                 data = sub_data)
    
    # Calculate H statistic (equivalent to Z)
    # For Wilcoxon, we can use the U statistic normalized
    u_stat <- wilcox_result$statistic
    n_total <- n1 + n2
    z_score <- (u_stat - (n1 * n2 / 2)) / sqrt(n1 * n2 * (n_total + 1) / 12)
    
    results[[length(results) + 1]] <- data.frame(
      Group1 = g1,
      Group2 = g2,
      n1 = n1,
      n2 = n2,
      H = round(abs(as.numeric(z_score)), 3),
      p_value = wilcox_result$p.value,
      stringsAsFactors = FALSE
    )
  }
  
  # Combine all results
  all_results <- bind_rows(results)
  
  # Add BH adjusted q-values
  all_results <- all_results %>%
    mutate(
      q_value = p.adjust(p_value, method = "BH"),
      Symbol = case_when(
        q_value < 0.0001 ~ "****",
        q_value < 0.001 ~ "***",
        q_value < 0.01 ~ "**",
        q_value < 0.05 ~ "*",
        TRUE ~ "NS"
      )
    ) %>%
    arrange(q_value)
  
  return(all_results)
}

# Run for Shannon index
table_s5 <- run_species_pairwise(entropy_clean, "shannon_entropy", "Shannon Index")

# Add n values to species names in the output
table_s5_formatted <- table_s5 %>%
  mutate(
    Group1 = paste0(Group1, " (n=", n1, ")"),
    Group2 = paste0(Group2, " (n=", n2, ")")
  ) %>%
  select(Group1, Group2, H, p_value, q_value, Symbol)

print(head(table_s5_formatted, 20))
write.csv(table_s5_formatted, "outputs/tables/Table_S5_Shannon_Species_Pairwise.csv", row.names = FALSE)

# ============================================================================
# TABLE S6: Pairwise Comparisons of Faith PD Among Host Species
# ============================================================================

cat("\n\n========== TABLE S6: FAITH PD - SPECIES PAIRWISE COMPARISONS ==========\n")

# Run for Faith PD
table_s6 <- run_species_pairwise(entropy_clean, "faith_pd", "Faith PD")

# Format with sample sizes
table_s6_formatted <- table_s6 %>%
  mutate(
    Group1 = paste0(Group1, " (n=", n1, ")"),
    Group2 = paste0(Group2, " (n=", n2, ")")
  ) %>%
  select(Group1, Group2, H, p_value, q_value, Symbol)

print(head(table_s6_formatted, 20))
write.csv(table_s6_formatted, "outputs/tables/Table_S6_FaithPD_Species_Pairwise.csv", row.names = FALSE)


# ============================================================================
# OPTIONAL: Format tables for Word/Excel with better formatting
# ============================================================================

# Format Table S3 for easy copying
cat("\n\n========== TABLE S3 (Copy-paste ready) ==========\n")
cat("\nMetric\tn\tstatistic\tdf\tp\tmethod")
for(i in 1:nrow(table_s3)) {
  cat(sprintf("\n%s\t%d\t%.8f\t%d\t%s\t%s",
              table_s3$Metric[i], table_s3$n[i], table_s3$statistic[i],
              table_s3$df[i], table_s3$p[i], table_s3$method[i]))
}

# Format Table S4 for easy copying
cat("\n\n========== TABLE S4 (Copy-paste ready) ==========\n")
cat("\nMetric\tgroup1\tgroup2\tn1\tn2\testimate\tstatistic\tp\tp.adj\tp.adj.signif\tmethod")
for(i in 1:nrow(table_s4)) {
  cat(sprintf("\n%s\t%s\t%s\t%d\t%d\t%.3f\t%.3f\t%.2e\t%.2e\t%s\t%s",
              table_s4$Metric[i], table_s4$Group1[i], table_s4$Group2[i],
              table_s4$n1[i], table_s4$n2[i], table_s4$estimate[i],
              table_s4$statistic[i], table_s4$p[i], table_s4$p.adj[i],
              table_s4$p.adj.signif[i], table_s4$method[i]))
}

# ============================================================================
# VERIFICATION: Check sample sizes match your tables
# ============================================================================

cat("\n\n========== SAMPLE SIZE VERIFICATION ==========\n")
cat("\nTotal samples in analysis:", nrow(entropy_clean))
cat("\n\nSamples by diet:")
print(table(entropy_clean$Diet_type))

cat("\n\nSamples by species:")
species_n <- entropy_clean %>%
  group_by(Common.name_Species) %>%
  summarise(N = n()) %>%
  arrange(desc(N))
print(species_n)


