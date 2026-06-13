
library(ggplot2); library(dplyr); library(tidyr)
library(tibble); library(readr); library(stringr)
library(scales); library(pheatmap); library(RColorBrewer); library(vegan)

# ── 1. File paths ─────────────────────────────────────────────────────────────
pathway_file <- "D:/01_Work/02_ActiveProjects/Project_16S_Kruger/Manuscript_analysis/data/raw/path_abun_unstrat.tsv"
diet_file    <- "D:/01_Work/02_ActiveProjects/Project_16S_Kruger/Manuscript_analysis/data/raw/diets.csv"

# Outliers to remove
OUTLIERS <- c("K058700-R", "K058796-R", "K058824-R", "K058810-R", "K058694-R")

# ── 2. Load data ──────────────────────────────────────────────────────────────
pwy <- read.table(pathway_file, header = TRUE, sep = "\t",
                  row.names = 1, check.names = FALSE, quote = "")
pwy <- pwy[, !colnames(pwy) %in% OUTLIERS]

diets <- read_csv(diet_file, show_col_types = FALSE)
colnames(diets)[1] <- "Sample"
diets <- diets %>% filter(Sample %in% colnames(pwy))
diets$diet_type <- factor(diets$diet_type,
                          levels = c("Carnivore","Non-Ruminant","Ruminant"),
                          labels = c("Carnivores","Non-Ruminants","Ruminants"))

sample_counts <- table(diets$diet_type)

desc_file <- "metacyc_pathways_info.txt.gz"
if (!file.exists(desc_file)) {
  download.file(
    "https://raw.githubusercontent.com/picrust/picrust2/master/picrust2/default_files/description_mapfiles/metacyc_pathways_info.txt.gz",
    destfile = desc_file, mode = "wb"
  )
}
desc_map <- read.table(desc_file, header = FALSE, sep = "\t",
                       quote = "", stringsAsFactors = FALSE, comment.char = "")
colnames(desc_map) <- c("pathway_id","description")

# 2. Top-level categories
cat_file <- "metacyc_top_level.tsv"
if (!file.exists(cat_file)) {
  cat("  Downloading MetaCyc top-level category map...\n")
  download.file(
    "https://raw.githubusercontent.com/Jiung-Wen/picrust_mapping/master/metacyc_pathways_info_prokaryotes_top_level.tsv",
    destfile = cat_file, mode = "wb"
  )
}

cat_map <- read.table(cat_file, header = FALSE, sep = "\t",
                      quote = "", stringsAsFactors = FALSE, comment.char = "")

if (ncol(cat_map) >= 2) {
  colnames(cat_map)[1:2] <- c("pathway_id", "top_level_category")
  cat(sprintf(" MetaCyc top-level categories: %d entries\n", nrow(cat_map)))
} else {
  stop("ERROR: Category map file is corrupted or has the wrong format.")
}

pwy_annotated <- pwy %>%
  rownames_to_column("pathway_id") %>%
  left_join(desc_map, by = "pathway_id") %>%
  left_join(cat_map, by = "pathway_id") %>%
  mutate(
    description = ifelse(is.na(description), pathway_id, description),
    top_level_category = ifelse(is.na(top_level_category), "Unclassified", top_level_category),
    top_level_category = recode(top_level_category, "Glykan Pathways" = "Glycan Pathways")
  )

sample_cols <- colnames(pwy)
pwy_relab <- as.matrix(sweep(pwy[, sample_cols], 2, 
                              colSums(pwy[, sample_cols]), "/"))

create_group_heatmap <- function(data, metadata, group1_samples, group2_samples, 
                                  group1_name, group2_name, n_top = 15,
                                  filename = "heatmap.png") {

  all_samples <- c(group1_samples, group2_samples)
  group_mat <- data[, all_samples, drop = FALSE]
  
  # Calculate t-statistic for each pathway
  pathway_names <- rownames(group_mat)
  t_stats <- numeric(length(pathway_names))
  names(t_stats) <- pathway_names
  
  for (i in seq_along(pathway_names)) {
    pathway <- pathway_names[i]
    vals1 <- as.numeric(group_mat[pathway, group1_samples, drop = TRUE])
    vals2 <- as.numeric(group_mat[pathway, group2_samples, drop = TRUE])
    
    # Remove NAs and infinite values
    vals1 <- vals1[is.finite(vals1) & !is.na(vals1)]
    vals2 <- vals2[is.finite(vals2) & !is.na(vals2)]
    
    if(length(vals1) >= 2 & length(vals2) >= 2) {
      # Use t-test
      t_res <- tryCatch({
        t.test(vals1, vals2, var.equal = FALSE)
      }, error = function(e) {
        return(NULL)
      })
      if(!is.null(t_res)) {
        t_stats[i] <- abs(t_res$statistic)
      } else {
        t_stats[i] <- 0
      }
    } else {
      t_stats[i] <- 0
    }
  }
  
  # Get top N pathways (excluding zero variance)
  valid_pathways <- t_stats[is.finite(t_stats) & t_stats > 0]
  if(length(valid_pathways) == 0) {
    cat(" Warning: No variable pathways found!\n")
    return(NULL)
  }
  
  top_pathways <- names(sort(valid_pathways, decreasing = TRUE)[1:min(n_top, length(valid_pathways))])
  
  # Ensure we have valid pathways
  if(length(top_pathways) == 0) {
    cat(" Warning: No top pathways selected!\n")
    return(NULL)
  }
  
  # Create matrix - ensure pathways exist in group_mat
  top_pathways_valid <- top_pathways[top_pathways %in% rownames(group_mat)]
  if(length(top_pathways_valid) == 0) {
    cat(" Warning: No valid pathway names found!\n")
    return(NULL)
  }
  
  heatmap_mat <- group_mat[top_pathways_valid, , drop = FALSE]
  
  # Scale the matrix (handle single row case)
  if(nrow(heatmap_mat) == 1) {
    heatmap_scaled <- heatmap_mat
  } else {
    heatmap_scaled <- t(scale(t(heatmap_mat)))
  }
  
  # Add readable row names
  row_descriptions <- pwy_annotated$description[match(rownames(heatmap_scaled), pwy_annotated$pathway_id)]
  row_descriptions[is.na(row_descriptions)] <- rownames(heatmap_scaled)[is.na(row_descriptions)]
  rownames(heatmap_scaled) <- str_trunc(row_descriptions, 70, side = "right")
  
  # Order samples - put group1 first, then group2
  sample_order <- c(group1_samples, group2_samples)
  heatmap_scaled <- heatmap_scaled[, sample_order, drop = FALSE]
  
  # Create annotation
  annotation_df <- data.frame(
    Group = c(rep(group1_name, length(group1_samples)), 
              rep(group2_name, length(group2_samples))),
    row.names = sample_order
  )
  
  # Define colors
  group_colors <- c(setNames("#9370db", group1_name), 
                    setNames("#d88fd8", group2_name))
  
  # For Black vs White rhino, use specific colors
  if(group1_name == "Black rhinoceros" && group2_name == "White rhinoceros") {
    group_colors <- c("Black rhinoceros" = "#9370db", "White rhinoceros" = "#d88fd8")
  }
  if(group1_name == "Other Carnivores" && group2_name == "Spotted Hyaena") {
    group_colors <- c("Other Carnivores" = "#FF5F2B", "Spotted Hyaena" = "#FFD580")
  }
  
  # Create heatmap
  pheatmap(
    mat = heatmap_scaled,
    scale = "none",  # Already scaled
    color = colorRampPalette(c("#313695", "white", "#a50026"))(100),
    annotation_col = annotation_df,
    annotation_colors = list(Group = group_colors),
    annotation_names_col = TRUE,
    cluster_cols = FALSE,  # Keep groups separate
    cluster_rows = TRUE,
    show_rownames = TRUE,
    show_colnames = FALSE,
    fontsize_row = 8,
    fontsize = 10,
    border_color = NA,
    cellwidth = 7,
    cellheight = 10,
    gaps_col = length(group1_samples),  # Gap between groups
    filename = filename,
    width = 10,
    height = max(6, nrow(heatmap_scaled) * 0.3),  # Dynamic height
    dpi = 300
  )
  
  # Return the top pathways for reporting
  result_df <- data.frame(
    Rank = 1:length(top_pathways_valid),
    Pathway_ID = top_pathways_valid,
    Description = pwy_annotated$description[match(top_pathways_valid, pwy_annotated$pathway_id)],
    T_statistic = round(t_stats[top_pathways_valid], 3)
  )

  return(result_df)
}

# ── PANEL D: Black vs White Rhinoceros (15 most variable pathways) ─────────
# Get rhinoceros samples
black_rhino_samples <- diets %>%
  filter(Species == "Black rhinoceros") %>%
  pull(Sample)

white_rhino_samples <- diets %>%
  filter(Species == "White rhinoceros") %>%
  pull(Sample)

cat("  Black rhinoceros samples:", length(black_rhino_samples), "\n")
cat("  White rhinoceros samples:", length(white_rhino_samples), "\n")

if(length(black_rhino_samples) >= 2 & length(white_rhino_samples) >= 2) {
  rhino_results <- create_group_heatmap(
    data = pwy_relab,
    metadata = diets,
    group1_samples = black_rhino_samples,
    group2_samples = white_rhino_samples,
    group1_name = "Black rhinoceros",
    group2_name = "White rhinoceros",
    n_top = 15,
    filename = "outputs/figures/Figure8_B.png"
  )

} else {
  cat(" Not enough rhinoceros samples for heatmap\n")
}

# ── PANEL E: Spotted Hyaena vs Other Carnivores (15 most variable pathways) ─

# Get carnivore samples
hyaena_samples <- diets %>%
  filter(Species == "Spotted Hyaena") %>%
  pull(Sample)

other_carn_samples <- diets %>%
  filter(diet_type == "Carnivores", Species != "Spotted Hyaena") %>%
  pull(Sample)


if(length(hyaena_samples) >= 2 & length(other_carn_samples) >= 2) {
  carn_results <- create_group_heatmap(
    data = pwy_relab,
    metadata = diets,
    group1_samples = other_carn_samples,
    group2_samples = hyaena_samples,
    group1_name = "Other Carnivores",
    group2_name = "Spotted Hyaena",
    n_top = 15,
    filename = "outputs/figures/Figure8_C.png"
  )
  
} else {
  cat(" Not enough carnivore samples for heatmap\n")
}

# ── 6. PANEL A: Stacked bar (Official MetaCyc Categories) ─────────────────────

# Aggregate by category
cat_df <- pwy_annotated %>%
  select(top_level_category, all_of(sample_cols)) %>%
  group_by(top_level_category) %>%
  summarise(across(everything(), sum), .groups = "drop") %>%
  column_to_rownames("top_level_category")

rel <- sweep(cat_df, 2, colSums(cat_df), FUN = "/")

# Define category order
available_cats <- rownames(cat_df)
CAT_ORDER <- c("Biosynthesis", "Degradation/Utilization/Assimilation",
               "Generation of Precursor Metabolite and Energy", "Superpathways",
               "Glycan Pathways", "Macromolecule Modification", "Amino Acid Metabolism",
               "Lipid Metabolism", "Nucleotide Metabolism", "Cofactor/Vitamin Metabolism",
               "Other Metabolic Pathways", "Other Pathways")
CAT_ORDER <- CAT_ORDER[CAT_ORDER %in% available_cats]
CAT_ORDER <- c(CAT_ORDER, setdiff(available_cats, CAT_ORDER))

# Colors
n_cats <- length(CAT_ORDER)
if (n_cats <= 12) {
  CAT_COLORS <- setNames(brewer.pal(n_cats, "Set3"), CAT_ORDER)
} else {
  CAT_COLORS <- setNames(colorRampPalette(brewer.pal(12, "Set3"))(n_cats), CAT_ORDER)
}

common_colors <- c(
  "Biosynthesis" = "#d62728",
  "Degradation/Utilization/Assimilation" = "#1f77b4",
  "Generation of Precursor Metabolite and Energy" = "#2ca02c",
  "Superpathways" = "#ff7f0e"
)
for (cat in names(common_colors)) {
  if (cat %in% CAT_ORDER) {
    CAT_COLORS[cat] <- common_colors[cat]
  }
}

DIET_ORDER <- c("Carnivores","Non-Ruminants","Ruminants")

rel_long <- rel %>%
  rownames_to_column("category") %>%
  pivot_longer(-category, names_to = "Sample", values_to = "RelAbund") %>%
  left_join(diets, by = "Sample") %>%
  mutate(
    category = factor(category, levels = rev(CAT_ORDER)),
    diet_type = factor(diet_type, levels = DIET_ORDER)
  )

diet_mean <- rel_long %>%
  group_by(diet_type, category) %>%
  summarise(RelAbund = mean(RelAbund, na.rm = TRUE), .groups = "drop")

n_counts <- diets %>% count(diet_type)

pA <- ggplot(diet_mean,
             aes(x = diet_type, y = RelAbund,
                 fill = factor(category, levels = rev(CAT_ORDER)))) +
  geom_bar(stat = "identity", width = 0.6, colour = "white", linewidth = 0.5) +
  geom_text(data = n_counts,
            aes(x = diet_type, y = 1.03, label = paste0("n=", n)),
            size = 4, colour = "grey40", inherit.aes = FALSE) +
  scale_fill_manual(values = CAT_COLORS, name = " Category") +
  scale_y_continuous(labels = label_percent(accuracy = 1),
                     expand = c(0, 0), limits = c(0, 1)) +
  labs(x = NULL, y = "Relative Abundance", title = "") +
  theme_classic(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.text = element_text(color = "black"),
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
  )

ggsave("outputs/figures/FigureS8.png", pA,
       width = 8, height = 5, dpi = 300, bg = "white")

# ── 7. PANEL B: Top 15 pathways by abundance ─────────────────────────────────

pathway_means <- rowMeans(pwy_relab, na.rm = TRUE)
top15_ids <- names(sort(pathway_means, decreasing = TRUE)[1:15])

pwy_top15 <- pwy_annotated %>% filter(pathway_id %in% top15_ids)
pwy_top15$RowLabel <- str_trunc(pwy_top15$description, 60, side = "right")

heatmap_matrix <- as.matrix(pwy_relab[top15_ids, sample_cols])
rownames(heatmap_matrix) <- pwy_top15$RowLabel

sample_order <- diets %>% arrange(diet_type, Sample) %>% pull(Sample)
heatmap_matrix <- heatmap_matrix[, sample_order]

annotation_col <- data.frame(
  Diet = diets$diet_type[match(colnames(heatmap_matrix), diets$Sample)],
  row.names = colnames(heatmap_matrix)
)

diet_colors <- c("Carnivores" = "#ff5e2b", "Ruminants" = "#800080", "Non-Ruminants" = "#2E8B57")
ann_colors <- list(Diet = diet_colors)
heat_colors <- colorRampPalette(c("#313695", "white", "#a50026"))(100)

# ── 8. PANEL C: Species heatmap with Diet + Species annotations ───────────────

# Calculate F-statistic for each pathway (between diet groups)
f_stats <- sapply(rownames(pwy_relab), function(pathway) {
  pathway_data <- as.numeric(pwy_relab[pathway, sample_cols])
  pathway_df <- data.frame(
    abundance = pathway_data,
    diet = diets$diet_type[match(sample_cols, diets$Sample)]
  )
  pathway_df <- na.omit(pathway_df)
  if(length(unique(pathway_df$diet)) >= 2) {
    anova_res <- summary(aov(abundance ~ diet, data = pathway_df))
    return(anova_res[[1]]$`F value`[1])
  } else {
    return(0)
  }
})

top30_variable <- names(sort(f_stats, decreasing = TRUE)[1:30])

heatmap_species_matrix <- as.matrix(pwy_relab[top30_variable, sample_cols])
rownames(heatmap_species_matrix) <- str_trunc(
  pwy_annotated$description[match(rownames(heatmap_species_matrix), 
                                   pwy_annotated$pathway_id)], 83)

sample_order_species <- diets %>%
  arrange(diet_type, Species, Sample) %>%
  pull(Sample)

heatmap_species_matrix <- heatmap_species_matrix[, sample_order_species]

annotation_col_two <- data.frame(
  Diet = diets$diet_type[match(colnames(heatmap_species_matrix), diets$Sample)],
  Species = diets$Species[match(colnames(heatmap_species_matrix), diets$Sample)],
  row.names = colnames(heatmap_species_matrix)
)

diet_colors_species <- c("Carnivores" = "#ff5e2b", "Non-Ruminants" = "#800080", "Ruminants" = "#2E8B57")

species_colors <- c(
  "African wild dog" = "#ff5e2b",
  "Lion" = "#fba300",
  "Spotted Hyaena" = "#ffd580",
  "Warthog" = "#800080",
  "African elephant" = "#9e19f6",
  "Black rhinoceros" = "#9370db",
  "White rhinoceros" = "#d88fd8",
  "Impala" = "#90ee90",
  "African buffalo" = "#228b22"
)

ann_colors_two <- list(Diet = diet_colors_species, Species = species_colors)
diet_gaps <- cumsum(table(diets$diet_type[match(sample_order_species, diets$Sample)]))
diet_gaps <- diet_gaps[1:length(diet_gaps)-1]

pheatmap(
  mat = heatmap_species_matrix,
  scale = "row",
  color = heat_colors,
  annotation_col = annotation_col_two,
  annotation_colors = ann_colors_two,
  annotation_names_col = TRUE,
  cluster_cols = TRUE,
  cluster_rows = TRUE,
  show_rownames = TRUE,
  show_colnames = FALSE,
  fontsize_row = 8,
  fontsize = 10,
  border_color = NA,
  cellwidth = 5,
  cellheight = 6,
  gaps_col = diet_gaps,
  filename = "outputs/figures/Figure8_A.png",
  width = 14,
  height = 10,
  dpi = 300
)


top_variable_pathways <- data.frame(
  Rank = 1:30,
  Pathway_ID = top30_variable,
  Description = pwy_annotated$description[match(top30_variable, pwy_annotated$pathway_id)],
  F_statistic = round(f_stats[top30_variable], 3)
)

write.csv(top_variable_pathways, "outputs/tables/Top30_Variable_Pathways_ANOVA.csv", row.names = FALSE)

top30_scaled <- t(scale(t(pwy_relab[top30_variable, sample_cols])))

# Create data frame with z-scores and metadata
zscore_df <- as.data.frame(top30_scaled)
zscore_df$Pathway_ID <- rownames(zscore_df)
zscore_df$Description <- pwy_annotated$description[match(zscore_df$Pathway_ID, pwy_annotated$pathway_id)]

# Pivot to long format
zscore_long <- zscore_df %>%
  pivot_longer(cols = -c(Pathway_ID, Description), 
               names_to = "Sample", values_to = "Z_score") %>%
  left_join(diets, by = "Sample")

# Calculate mean z-score per diet for each pathway
zscore_summary <- zscore_long %>%
  group_by(Description, diet_type) %>%
  summarise(
    Mean_Z = mean(Z_score, na.rm = TRUE),
    SD_Z = sd(Z_score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Determine which diet has highest enrichment for each pathway
  group_by(Description) %>%
  mutate(
    Max_Z = max(Mean_Z),
    Min_Z = min(Mean_Z),
    Enriched_in = diet_type[which.max(Mean_Z)],
    Depleted_in = diet_type[which.min(Mean_Z)],
    Enrichment_score = Max_Z - Min_Z
  ) %>%
  ungroup() %>%
  arrange(desc(Enrichment_score))

# Add ranking
zscore_summary$Rank <- 1:nrow(zscore_summary)

# Reorganize columns
zscore_summary <- zscore_summary %>%
  select(Rank, Description, diet_type, Mean_Z, SD_Z, Enriched_in, Depleted_in, Enrichment_score)

# Save to CSV
write.csv(zscore_summary, "outputs/tables/TableS10.csv", row.names = FALSE)

# === For Panel D: Black vs White Rhinoceros - Detailed statistics ===

if(exists("rhino_results") && !is.null(rhino_results)) {

  # Get the top 15 pathway IDs from rhino_results
  rhino_top_pathways <- rhino_results$Pathway_ID
  
  # Extract raw abundances for these pathways
  rhino_abundance <- pwy_relab[rhino_top_pathways, c(black_rhino_samples, white_rhino_samples)]
  
  # Calculate statistics for each group
  rhino_stats <- data.frame()
  
  for(i in 1:nrow(rhino_abundance)) {
    pathway_id <- rownames(rhino_abundance)[i]
    description <- pwy_annotated$description[match(pathway_id, pwy_annotated$pathway_id)]
    
    # Black rhino stats
    black_vals <- as.numeric(rhino_abundance[i, black_rhino_samples])
    black_vals <- black_vals[is.finite(black_vals) & !is.na(black_vals)]
    
    # White rhino stats
    white_vals <- as.numeric(rhino_abundance[i, white_rhino_samples])
    white_vals <- white_vals[is.finite(white_vals) & !is.na(white_vals)]
    
    # Calculate statistics
    black_mean <- mean(black_vals, na.rm = TRUE)
    black_sd <- sd(black_vals, na.rm = TRUE)
    white_mean <- mean(white_vals, na.rm = TRUE)
    white_sd <- sd(white_vals, na.rm = TRUE)
    
    # Calculate fold change (relative to white rhino)
    fold_change <- black_mean / white_mean
    log2_fc <- log2(fold_change)
    
    # T-test p-value
    t_test <- t.test(black_vals, white_vals, var.equal = FALSE)
    
    rhino_stats <- rbind(rhino_stats, data.frame(
      Rank = i,
      Pathway_ID = pathway_id,
      Description = description,
      Black_Rhino_Mean = round(black_mean, 6),
      Black_Rhino_SD = round(black_sd, 6),
      White_Rhino_Mean = round(white_mean, 6),
      White_Rhino_SD = round(white_sd, 6),
      Fold_Change_Black_vs_White = round(fold_change, 3),
      Log2_Fold_Change = round(log2_fc, 3),
      T_statistic = round(rhino_results$T_statistic[i], 3),
      P_value = signif(t_test$p.value, 3)
    ))
  }
  
  # Add enrichment direction
  rhino_stats$Enriched_in <- ifelse(rhino_stats$Log2_Fold_Change > 0, 
                                     "Black rhinoceros", "White rhinoceros")
  
  write.csv(rhino_stats, "outputs/tables/TableS11.csv", row.names = FALSE)
}

if(exists("carn_results") && !is.null(carn_results)) {

  # Get the top 15 pathway IDs from carn_results
  carn_top_pathways <- carn_results$Pathway_ID
  # Extract raw abundances for these pathways
  carn_abundance <- pwy_relab[carn_top_pathways, c(other_carn_samples, hyaena_samples)]
  
  # Calculate statistics for each group
  carn_stats <- data.frame()
  
  for(i in 1:nrow(carn_abundance)) {
    pathway_id <- rownames(carn_abundance)[i]
    description <- pwy_annotated$description[match(pathway_id, pwy_annotated$pathway_id)]
    
    # Other carnivores stats
    other_vals <- as.numeric(carn_abundance[i, other_carn_samples])
    other_vals <- other_vals[is.finite(other_vals) & !is.na(other_vals)]
    
    # Spotted hyaena stats
    hyaena_vals <- as.numeric(carn_abundance[i, hyaena_samples])
    hyaena_vals <- hyaena_vals[is.finite(hyaena_vals) & !is.na(hyaena_vals)]
    
    # Calculate statistics
    other_mean <- mean(other_vals, na.rm = TRUE)
    other_sd <- sd(other_vals, na.rm = TRUE)
    hyaena_mean <- mean(hyaena_vals, na.rm = TRUE)
    hyaena_sd <- sd(hyaena_vals, na.rm = TRUE)
    
    # Calculate fold change (hyaena vs other carnivores)
    fold_change <- hyaena_mean / other_mean
    log2_fc <- log2(fold_change)
    
    # T-test p-value
    t_test <- t.test(hyaena_vals, other_vals, var.equal = FALSE)
    
    carn_stats <- rbind(carn_stats, data.frame(
      Rank = i,
      Pathway_ID = pathway_id,
      Description = description,
      Other_Carnivores_Mean = round(other_mean, 6),
      Other_Carnivores_SD = round(other_sd, 6),
      Spotted_Hyaena_Mean = round(hyaena_mean, 6),
      Spotted_Hyaena_SD = round(hyaena_sd, 6),
      Fold_Change_Hyaena_vs_Other = round(fold_change, 3),
      Log2_Fold_Change = round(log2_fc, 3),
      T_statistic = round(carn_results$T_statistic[i], 3),
      P_value = signif(t_test$p.value, 3)
    ))
  }
  
  # Add enrichment direction
  carn_stats$Enriched_in <- ifelse(carn_stats$Log2_Fold_Change > 0, 
                                    "Spotted Hyaena", "Other Carnivores")
  
  write.csv(carn_stats, "outputs/tables/TableS12.csv", row.names = FALSE)
}

