library(tidyverse)
library(RColorBrewer)
library(stringr)

# Load data
data <- read.csv("data/raw/level-6.csv", check.names = FALSE, stringsAsFactors = FALSE)

# Remove outliers (matching your analysis)
outliers <- c("K058700-R", "K058796-R", "K058824-R", "K058810-R", "K058694-R")
data <- data[!data$index %in% outliers, ]

# Extract abundance and metadata
tax_cols <- grep("^d__", names(data), value = TRUE)
abundance <- data[, tax_cols]
sample_ids <- data$index
diet_type <- data$Diet_type
species_name <- data$Common.name_Species
animals <- data$Animal

# Extract family names
family_names <- sapply(tax_cols, function(col) {
  parts <- unlist(strsplit(col, ";"))
  f_part <- parts[grep("^f__", parts)]
  if (length(f_part) == 0) return("Unclassified")
  family <- sub("^f__", "", f_part)
  family <- unlist(strsplit(family, ";"))[1]
  ifelse(family == "", "Unclassified", family)
})

# Aggregate to family level
abundance_t <- t(abundance)
abundance_family <- rowsum(abundance_t, group = family_names, reorder = FALSE)
abundance_family <- t(abundance_family)
rownames(abundance_family) <- sample_ids

# Remove "Unclassified"
abundance_family_df <- as.data.frame(abundance_family)
if ("Unclassified" %in% colnames(abundance_family_df)) {
  abundance_family_df$Unclassified <- NULL
}

# Normalize to relative abundance
abundance_family_rel <- as.data.frame(abundance_family_df / rowSums(abundance_family_df) * 100)

# Add metadata
abundance_family_rel$Sample <- rownames(abundance_family_rel)
abundance_family_rel$Diet_type <- diet_type
abundance_family_rel$Species <- species_name
abundance_family_rel$Animal <- animals

# ============================================================================
# TABLE S7: Average Relative Abundances by Dietary Group
# ============================================================================

# Get family columns (exclude metadata)
family_cols <- names(abundance_family_rel)[!names(abundance_family_rel) %in% c("Sample", "Diet_type", "Species", "Animal")]

# Calculate mean abundance for each family by diet group
table_s7 <- abundance_family_rel %>%
  group_by(Diet_type) %>%
  summarise(across(all_of(family_cols), ~ mean(., na.rm = TRUE))) %>%
  pivot_longer(cols = -Diet_type, names_to = "Family", values_to = "Average_Relative_Abundance") %>%
  filter(Average_Relative_Abundance > 1) %>%  # Keep only families with >1% abundance
  arrange(Diet_type, desc(Average_Relative_Abundance)) %>%
  mutate(Average_Relative_Abundance = round(Average_Relative_Abundance, 6))

# Format for display
table_s7_display <- table_s7 %>%
  select(Diet_type, Family, Average_Relative_Abundance)

# Save to CSV
write.csv(table_s7_display, "outputs/tables/Table_S7_Family_Abundance_By_Diet.csv", row.names = FALSE)

# Calculate with Mean ± SD for Table S7
table_s7_with_sd <- abundance_family_rel %>%
  group_by(Diet_type) %>%
  summarise(across(all_of(family_cols), list(Mean = ~mean(., na.rm = TRUE), 
                                              SD = ~sd(., na.rm = TRUE)))) %>%
  pivot_longer(cols = -Diet_type, names_to = "Variable", values_to = "Value") %>%
  mutate(
    Family = gsub("_Mean|_SD", "", Variable),
    Statistic = ifelse(grepl("_Mean", Variable), "Mean", "SD")
  ) %>%
  select(-Variable) %>%
  pivot_wider(names_from = Statistic, values_from = Value, values_fn = list) %>%
  unnest(cols = c(Mean, SD)) %>%
  filter(Mean > 1) %>%
  mutate(Mean_SD = paste0(round(Mean, 2), " ± ", round(SD, 2))) %>%
  arrange(Diet_type, desc(Mean)) %>%
  select(Diet_type, Family, Mean_SD, Mean, SD)

write.csv(table_s7_with_sd, "outputs/tables/Table_S7_Family_Abundance_By_Diet_with_SD.csv", row.names = FALSE)

# Print formatted table for manuscript (copy-paste ready)
cat("\n\n========== TABLE S7 (Copy-paste ready) ==========\n")
cat("\nDiet type\tFamily\tAverage Relative Abundance (%)")
for(i in 1:nrow(table_s7_display)) {
  cat(sprintf("\n%s\t%s\t%.6f", 
              table_s7_display$Diet_type[i], 
              table_s7_display$Family[i], 
              table_s7_display$Average_Relative_Abundance[i]))
}

# Get species with their diet types
species_diet <- data %>%
  select(Common.name_Species, Diet_type) %>%
  distinct()

# Calculate mean abundance for each family by species
table_s8 <- abundance_family_rel %>%
  group_by(Species) %>%
  summarise(across(all_of(family_cols), ~ mean(., na.rm = TRUE))) %>%
  pivot_longer(cols = -Species, names_to = "Family", values_to = "Average_Relative_Abundance") %>%
  filter(Average_Relative_Abundance > 1) %>%  # Keep only families with >1% abundance
  arrange(Species, desc(Average_Relative_Abundance)) %>%
  mutate(Average_Relative_Abundance = round(Average_Relative_Abundance, 6))

# Add diet type information
table_s8 <- table_s8 %>%
  left_join(species_diet, by = c("Species" = "Common.name_Species"))

# Reorder columns
table_s8_display <- table_s8 %>%
  select(Species, Family, Average_Relative_Abundance) %>%
  arrange(Species, desc(Average_Relative_Abundance))

print(head(table_s8_display, 30))

# Save to CSV
write.csv(table_s8_display, "outputs/tables/Table_S8_Family_Abundance_By_Species.csv", row.names = FALSE)

# Calculate with Mean ± SD for Table S8
table_s8_with_sd <- abundance_family_rel %>%
  group_by(Species) %>%
  summarise(across(all_of(family_cols), list(Mean = ~mean(., na.rm = TRUE), 
                                              SD = ~sd(., na.rm = TRUE)))) %>%
  pivot_longer(cols = -Species, names_to = "Variable", values_to = "Value") %>%
  mutate(
    Family = gsub("_Mean|_SD", "", Variable),
    Statistic = ifelse(grepl("_Mean", Variable), "Mean", "SD")
  ) %>%
  select(-Variable) %>%
  pivot_wider(names_from = Statistic, values_from = Value, values_fn = list) %>%
  unnest(cols = c(Mean, SD)) %>%
  filter(Mean > 1) %>%
  mutate(Mean_SD = paste0(round(Mean, 2), " ± ", round(SD, 2))) %>%
  arrange(Species, desc(Mean)) %>%
  select(Species, Family, Mean_SD, Mean, SD)

write.csv(table_s8_with_sd, "outputs/tables/Table_S8_Family_Abundance_By_Species_with_SD.csv", row.names = FALSE)

# Calculate with confidence intervals for top families
table_s7_with_ci <- abundance_family_rel %>%
  group_by(Diet_type) %>%
  summarise(across(all_of(family_cols), list(
    Mean = ~mean(., na.rm = TRUE),
    SD = ~sd(., na.rm = TRUE),
    N = ~n(),
    CI_lower = ~mean(., na.rm = TRUE) - 1.96 * sd(., na.rm = TRUE) / sqrt(n()),
    CI_upper = ~mean(., na.rm = TRUE) + 1.96 * sd(., na.rm = TRUE) / sqrt(n())
  ))) %>%
  pivot_longer(cols = -Diet_type, names_to = "Variable", values_to = "Value") %>%
  mutate(
    Family = gsub("_Mean|_SD|_N|_CI_lower|_CI_upper", "", Variable),
    Statistic = case_when(
      grepl("_Mean", Variable) ~ "Mean",
      grepl("_SD", Variable) ~ "SD",
      grepl("_N", Variable) ~ "N",
      grepl("_CI_lower", Variable) ~ "CI_lower",
      grepl("_CI_upper", Variable) ~ "CI_upper",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-Variable) %>%
  pivot_wider(names_from = Statistic, values_from = Value, values_fn = list) %>%
  unnest(cols = c(Mean, SD, N, CI_lower, CI_upper)) %>%
  filter(Mean > 1) %>%
  arrange(Diet_type, desc(Mean))

write.csv(table_s7_with_ci, "outputs/tables/Table_S7_Family_Abundance_With_CI.csv", row.names = FALSE)

