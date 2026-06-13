library(tidyverse)
library(vegan)
library(ggplot2)
library(patchwork)
library(scales)
library(phyloseq)
library(microbiomeMarker)

raw <- read.csv("D:/01_Work/02_ActiveProjects/Project_16S_Kruger/Manuscript_analysis/scripts/data_new.csv", check.names = FALSE, stringsAsFactors = FALSE)

genus_cols <- grep("^d__Bacteria", names(raw), value = TRUE)
cat("Raw samples:", nrow(raw), "| Genus columns:", length(genus_cols), "\n")


make_genus_label <- function(col) {
  parts  <- strsplit(col, ";")[[1]]
  cl     <- function(s) sub("^[a-z]__", "", s)
  ranks  <- sapply(parts, cl)
  genus  <- if (length(ranks) >= 6) ranks[6] else ""
  family <- if (length(ranks) >= 5) ranks[5] else ""
  order  <- if (length(ranks) >= 4) ranks[4] else ""
  class_ <- if (length(ranks) >= 3) ranks[3] else ""
  phylum <- if (length(ranks) >= 2) ranks[2] else ""
  blank  <- function(s) s %in% c("", "_", "__")
  
  if (!blank(genus) && !genus %in% c("uncultured", "Incertae_Sedis")) {
    return(genus)
  } else if (genus == "uncultured") {
    ref <- if (!blank(family) && family != "uncultured") family
    else if (!blank(order) && order != "uncultured") order
    else phylum
    return(paste0(ref, " (uncultured)"))
  } else if (genus == "Incertae_Sedis") {
    return(paste0(family, " Incertae Sedis"))
  } else {
    if (!blank(family) && family != "uncultured") return(paste0(family, " (unclassified)"))
    else if (!blank(order) && order != "uncultured") return(paste0(order, " (unclassified)"))
    else if (!blank(class_) && class_ != "uncultured") return(paste0(class_, " (unclassified)"))
    else return(paste0(phylum, " (unclassified)"))
  }
}

genus_labels <- sapply(genus_cols, make_genus_label, USE.NAMES = FALSE)

# Make unique by appending phylum to any duplicates
dup_idx <- which(duplicated(genus_labels) | duplicated(genus_labels, fromLast = TRUE))
for (i in dup_idx) {
  ph <- sub("^p__", "", strsplit(genus_cols[i], ";")[[1]][2])
  genus_labels[i] <- paste0(genus_labels[i], " (", ph, ")")
}
stopifnot(length(unique(genus_labels)) == length(genus_labels))
cat("", length(genus_labels), "unique genus labels\n")


wuf_raw   <- read.table("D:/01_Work/02_ActiveProjects/Project_16S_Kruger/Manuscript_analysis/scripts/weighted_unifrac_distance_matrix.tsv",
                        header = TRUE, row.names = 1, sep = "\t", check.names = FALSE)
qiime_ids <- rownames(wuf_raw)   # 89 rarefaction-filtered samples

#outlier_ids <- c("K058683-R", "K058706-R", "K058789-R", "K058689-R", "K058814-R", "K058785-R")
outlier_ids <- c("")                 

df_rarefied <- raw[raw$index %in% qiime_ids, ]
df_clean    <- df_rarefied[!df_rarefied$index %in% outlier_ids, ]
cat("Samples after rarefaction:", nrow(df_rarefied),
    "| After outlier removal:", nrow(df_clean), "\n")
print(table(df_clean$Common.name_Species, df_clean$Diet_type))

write.csv(df_clean[, c("index","Common.name_Species","Animal","Diet_type")],
          "outlier_removal_record.csv", row.names = FALSE)


# ── 4. PHYLOSEQ OBJECT ────────────────────────────────────────────────────────
otu_mat <- t(as.matrix(df_clean[, genus_cols]))
colnames(otu_mat) <- df_clean$index
rownames(otu_mat) <- genus_labels
storage.mode(otu_mat) <- "integer"

parse_tax <- function(col) {
  parts <- strsplit(col, ";")[[1]]
  rnks  <- c("Kingdom","Phylum","Class","Order","Family","Genus")
  out   <- setNames(rep(NA_character_, 6), rnks)
  for (i in seq_along(parts)) out[rnks[i]] <- sub("^[a-z]__", "", parts[i])
  out
}
tax_mat           <- do.call(rbind, lapply(genus_cols, parse_tax))
rownames(tax_mat) <- genus_labels

meta_df           <- df_clean[, c("index","Common.name_Species","Animal","Diet_type","Sex","Age.class")]
rownames(meta_df) <- meta_df$index

ps <- phyloseq(
  otu_table(otu_mat, taxa_are_rows = TRUE),
  tax_table(tax_mat),
  sample_data(meta_df)
)
cat("\nPhyloseq object:\n"); print(ps)


extract_lefse <- function(lefse_obj) {
  
  # Pull raw slot data (avoids class contamination from as.data.frame)
  mt  <- microbiomeMarker::marker_table(lefse_obj)
  raw <- as.matrix(mt)   # coerce to plain matrix first
  
  df  <- data.frame(
    feature      = raw[, "feature"],
    enrich_group = raw[, "enrich_group"],
    ef_lda       = as.numeric(raw[, "ef_lda"]),
    pvalue       = as.numeric(raw[, "pvalue"]),
    stringsAsFactors = FALSE
  )
  # Confirm it is a plain data.frame with no exotic classes
  class(df) <- "data.frame"
  
  # Clean the feature column: "k__|p__|...|g__Genus" → "Genus"
  clean_name <- function(n) {
    n <- gsub("_+$", "", n)                              # trailing underscores
    n <- gsub("^_+", "", n)                              # leading underscores
    n <- gsub("_", " ", n)                               # remaining underscores → spaces
    trimws(n)
  }
  
  rank_prefix <- function(p) {
    switch(p, "g__" = "g: ", "f__" = "f: ", "o__" = "o: ", "c__" = "c: ", "p__" = "p: ", "")
  }
  
  df$feature_clean <- sapply(df$feature, function(x) {
    parts  <- strsplit(x, "\\|")[[1]]
    last   <- tail(parts, 1)
    prefix <- substr(last, 1, 3)
    name   <- sub("^[a-z]__", "", last)
    paste0(rank_prefix(prefix), clean_name(name))
  }, USE.NAMES = FALSE)
  
  # Significance stars
  df$stars <- cut(df$pvalue,
                  breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                  labels = c("***", "**", "*", ""))
  df$stars <- as.character(df$stars)
  
  df
}


plot_lefse <- function(res_df, enrich_col_1, enrich_col_2,
                       col1, col2, title_text, top_n = 30) {
  
  # Ensure res_df is a plain data frame before any dplyr work
  stopifnot(is.data.frame(res_df), !inherits(res_df, "marker_table"))
  
  plot_df <- res_df[order(-res_df$ef_lda), ]          # sort by LDA descending
  plot_df <- head(plot_df, top_n)                      # top N taxa
  
  plot_df$lda_signed <- plot_df$ef_lda 

  plot_df <- plot_df[grepl("\\|g__|\\|f__", plot_df$feature), ]
  plot_df <- plot_df[!duplicated(plot_df$feature_clean), ]

  plot_df <- plot_df[order(plot_df$enrich_group, -plot_df$ef_lda), ]  # group first, then LDA descending within group
  plot_df$feature_clean <- make.unique(as.character(plot_df$feature_clean))
  plot_df$feature_clean <- factor(plot_df$feature_clean,
                                  levels = plot_df$feature_clean)
  plot_df <- plot_df[order(rev(plot_df$enrich_group), -plot_df$ef_lda), ]
  
  
  nudge <- max(abs(plot_df$lda_signed)) * 0.03
  star_df <- plot_df[plot_df$stars != "", ]
  
  ggplot(plot_df, aes(x = lda_signed, y = feature_clean,
                      fill = enrich_group)) +
    geom_col(width = 0.8, colour = "black", linewidth = 0.25) +
    # Stars at bar tips
    geom_text(
      data = star_df,
      aes(x     = lda_signed + ifelse(lda_signed >= 0, nudge, -nudge),
          y     = feature_clean,
          label = stars),
      inherit.aes = FALSE,
      #hjust    = ifelse(star_df$lda_signed >= 0, 0, 1),
      hjust = 0,
      size     = 3.2,
      colour   = "grey20",
      fontface = "bold"
    ) +
    geom_vline(xintercept = 0, linewidth = 0.6, colour = "black") +
    scale_fill_manual(
      values = setNames(c(col1, col2), c(enrich_col_1, enrich_col_2)),
      name   = "Enriched in"
    ) +
    scale_x_continuous(
      labels = function(x) abs(round(x, 1)),
      expand = expansion(mult = c(0.0, 0.10))
    ) +
    labs(
      title    = title_text,
      subtitle = "*** p<0.001  ** p<0.01  * p<0.05",
      x        = "LDA Score (log10)",
      y        = NULL
    ) +
    theme_bw(base_size = 11) +
    theme(
      plot.title         = element_text(face = "bold", size = 13),
      plot.subtitle      = element_text(size = 8.5, colour = "grey45"),
      legend.position    = "bottom",
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text.y = element_text(size = 9, face = "italic")
    )
}

ps_rhino <- subset_samples(ps,
                           Common.name_Species %in% c("Black rhinoceros", "White rhinoceros"))
ps_rhino <- prune_taxa(taxa_sums(ps_rhino) > 0, ps_rhino)

lefse_rhino <- run_lefse(ps_rhino,
                         group = "Common.name_Species", norm = "CPM",
                         kw_cutoff = 0.05, wilcoxon_cutoff = 0.05,
                         lda_cutoff = 2, multigrp_strat = FALSE)

res_rhino <- extract_lefse(lefse_rhino)   # plain data.frame, guaranteed

write.csv(res_rhino, "LEfSe_BlackRhino_vs_WhiteRhino_results.csv", row.names = FALSE)

p_rhino <- plot_lefse(res_rhino,
                      enrich_col_1 = "Black rhinoceros", enrich_col_2 = "White rhinoceros",
                      col1 = "#9370DB", col2 = "#D88FD8",
                      title_text = "")

p_rhino 
ggsave("outputs/figures/FigureS9_2.png", p_rhino,
       width = 6, height = 6,
       dpi = 300, bg = "white")

ps_carn <- subset_samples(ps, Diet_type == "Carnivore")
sample_data(ps_carn)$Hyaena_group <- ifelse(
  sample_data(ps_carn)$Common.name_Species == "Spotted Hyaena",
  "Spotted Hyaena", "Other Carnivores")
ps_carn <- prune_taxa(taxa_sums(ps_carn) > 0, ps_carn)

lefse_hyaena <- run_lefse(ps_carn,
                          group = "Hyaena_group", norm = "CPM",
                          kw_cutoff = 0.05, wilcoxon_cutoff = 0.05,
                          lda_cutoff = 2, multigrp_strat = FALSE)

res_hyaena <- extract_lefse(lefse_hyaena)

write.csv(res_hyaena, "LEfSe_Hyaena_vs_OtherCarnivores_results.csv", row.names = FALSE)

p_hyaena <- plot_lefse(res_hyaena,
                       enrich_col_1 = "Spotted Hyaena", enrich_col_2 = "Other Carnivores",
                       col1 = "#FFD580", col2 = "#FF5F2B",
                       title_text = "")
p_hyaena
ggsave("outputs/figures/FigureS9_1.png", p_hyaena,
       width = 6, height = 6,
       dpi = 300, bg = "white")


