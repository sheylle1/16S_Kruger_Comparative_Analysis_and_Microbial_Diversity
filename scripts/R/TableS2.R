library(tidyverse)
library(vegan)

# ── 1. Load data and distance matrices ────────────────────────────────────────

# Load raw data
raw <- read.csv("D:/01_Work/02_ActiveProjects/Project_16S_Kruger/Manuscript_analysis/scripts/data_new.csv", 
                check.names = FALSE, stringsAsFactors = FALSE)

# Load weighted and unweighted UniFrac distance matrices
wuf_raw <- read.table("D:/01_Work/02_ActiveProjects/Project_16S_Kruger/Manuscript_analysis/scripts/weighted_unifrac_distance_matrix.tsv",
                      header = TRUE, row.names = 1, sep = "\t", check.names = FALSE)
uwuf_raw <- read.table("D:/01_Work/02_ActiveProjects/Project_16S_Kruger/Manuscript_analysis/scripts/unweighted_unifrac_distance_matrix.tsv",
                       header = TRUE, row.names = 1, sep = "\t", check.names = FALSE)

# Get rarefied sample IDs
qiime_ids <- rownames(wuf_raw)

# Filter samples (no outliers removed)
df_rarefied <- raw[raw$index %in% qiime_ids, ]
df_clean <- df_rarefied  # No outlier removal

cat("Total samples:", nrow(df_clean), "\n")
print(table(df_clean$Diet_type))

# ── 2. Load distance matrices and filter to clean samples ─────────────────────

load_qdist <- function(filename, keep_ids) {
  mat <- read.table(filename, header = TRUE, row.names = 1,
                    sep = "\t", check.names = FALSE)
  ids <- intersect(keep_ids, rownames(mat))
  as.dist(as.matrix(mat[ids, ids]))
}

keep_ids <- df_clean$index
wuf_dist <- load_qdist("D:/01_Work/02_ActiveProjects/Project_16S_Kruger/Manuscript_analysis/scripts/weighted_unifrac_distance_matrix.tsv", keep_ids)
uwuf_dist <- load_qdist("D:/01_Work/02_ActiveProjects/Project_16S_Kruger/Manuscript_analysis/scripts/unweighted_unifrac_distance_matrix.tsv", keep_ids)

# Metadata
meta_ord <- df_clean[match(labels(wuf_dist), df_clean$index), ]

cat("\nDistance matrices loaded:")
cat("\n  Weighted UniFrac:", attr(wuf_dist, "Size"), "samples")
cat("\n  Unweighted UniFrac:", attr(uwuf_dist, "Size"), "samples\n")

# ── 3. Pairwise PERMANOVA function ───────────────────────────────────────────

run_pairwise_permanova <- function(dist_obj, meta, group_col,
                                   nperm = 9999, dist_label = "") {
  groups <- unique(meta[[group_col]])
  pairs <- combn(groups, 2, simplify = FALSE)
  results <- list()
  
  for (pair in pairs) {
    g1 <- pair[1]
    g2 <- pair[2]
    idx <- meta$index[meta[[group_col]] %in% c(g1, g2)]
    sub_dist <- as.dist(as.matrix(dist_obj)[idx, idx])
    sub_meta <- meta[meta$index %in% idx, , drop = FALSE]
    
    sub_meta$perm_group <- sub_meta[[group_col]]
    perm <- adonis2(sub_dist ~ perm_group,
                    data = sub_meta, permutations = nperm, by = "terms")
    
    results[[paste(g1, "vs", g2)]] <- data.frame(
      Group1 = g1,
      Group2 = g2,
      Sample_Size = length(idx),
      Permutations = nperm,
      Pseudo_F = round(perm$F[1], 2),
      R2 = round(perm$R2[1], 3),
      p_value = perm$`Pr(>F)`[1],
      Distance = dist_label,
      stringsAsFactors = FALSE
    )
  }
  
  res <- do.call(rbind, results)
  res$q_value_BH <- round(p.adjust(res$p_value, method = "BH"), 6)
  res$p_value_formatted <- ifelse(res$p_value < 0.0001, "0.0001", 
                                   as.character(round(res$p_value, 4)))
  res$q_value_formatted <- ifelse(res$q_value_BH < 0.0001, "0.0001",
                                   as.character(round(res$q_value_BH, 6)))
  rownames(res) <- NULL
  res
}

# ── 4. Run pairwise PERMANOVA for dietary groups ─────────────────────────────

# Weighted UniFrac
pw_diet_wuf <- run_pairwise_permanova(wuf_dist, meta_ord, "Diet_type",
                                      nperm = 9999, dist_label = "Weighted UniFrac")

# Unweighted UniFrac
pw_diet_uwuf <- run_pairwise_permanova(uwuf_dist, meta_ord, "Diet_type",
                                       nperm = 9999, dist_label = "Unweighted UniFrac")

# ── 5. Create Table S2 formatted for manuscript ──────────────────────────────

# Combine both distance metrics
table_s2 <- rbind(pw_diet_wuf, pw_diet_uwuf)

# Format for manuscript display
table_s2_formatted <- table_s2 %>%
  select(Distance, Group1, Group2, Sample_Size, Permutations, 
         Pseudo_F, p_value_formatted, q_value_formatted, R2) %>%
  arrange(Distance, desc(Pseudo_F))

# Rename columns for clean output
colnames(table_s2_formatted) <- c("UniFrac Metric", "Group 1", "Group 2", 
                                   "Sample Size", "Permutations", "Pseudo-F", 
                                   "p-value", "q-value", "R²")


# Save as CSV
write.csv(table_s2_formatted, "outputs/tables/Table_S2.csv", row.names = FALSE)

