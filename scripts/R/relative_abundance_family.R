# Relative Abundance Analysis - Family Level

library(tidyverse)
library(RColorBrewer)

# Load data
data <- read.csv("data/level-6.csv", check.names = FALSE, stringsAsFactors = FALSE)

# Remove outliers
outliers <- c("K058683-R", "K058706-R", "K058789-R", "K058689-R", 
              "K058814-R", "K058785-R", "K058700-R", "K058796-R", 
              "K058824-R", "K058810-R", "K058694-R")
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

# Get top families
overall_top <- colSums(abundance_family_rel[, 1:(ncol(abundance_family_rel)-4)])
top_families <- names(sort(overall_top, decreasing = TRUE)[1:20])

# Calculate 'Other' category
top_data <- abundance_family_rel[, top_families, drop = FALSE]
other <- 100 - rowSums(top_data)
final_data <- cbind(top_data, Other = other)

# Add metadata
final_data$Sample <- abundance_family_rel$Sample
final_data$Diet_type <- factor(abundance_family_rel$Diet_type, 
                               levels = c("Carnivore", "Non-Ruminant", "Ruminant"))
final_data$Species <- abundance_family_rel$Species
final_data$Animal <- abundance_family_rel$Animal

# Sort samples
final_data <- final_data %>%
  arrange(Diet_type, Species) %>%
  mutate(Sample = factor(Sample, levels = Sample))

# Pivot to long format
long_data <- final_data %>%
  pivot_longer(cols = -c(Sample, Diet_type, Species, Animal), 
               names_to = "Family", values_to = "Abundance") %>%
  mutate(Family = factor(Family, levels = c(top_families, "Other")))

# Define color scheme
bright_families <- c(
  "Peptostreptococcaceae", "Clostridiaceae", "Enterobacteriaceae", "Bacteroidaceae",
  "Rikenellaceae", "Acidaminococcaceae", "Spirochaetaceae", "Anaerovoracaceae", "Paludibacteraceae",
  "Prevotellaceae", "Oscillospiraceae", "Fibrobacteraceae", "Christensenellaceae", "Ruminococcaceae", "uncultered"
)

n_bright <- sum(top_families %in% bright_families)
n_subtle <- length(top_families) - n_bright

bright_palette <- colorRampPalette(brewer.pal(8, "Set1"))(n_bright)
subtle_palette <- colorRampPalette(c("#CDFADB", "#F0C1E1", "#A5B68D", "#F4DEB3", 
                                     "#C5D3E8", "#CBE2B5", "#E8C5E5", "#D6DAC8"))(n_subtle)

# Assign colors
family_colors <- c()
bright_idx <- 1
subtle_idx <- 1

for (fam in top_families) {
  if (fam %in% bright_families) {
    family_colors[fam] <- bright_palette[bright_idx]
    bright_idx <- bright_idx + 1
  } else {
    family_colors[fam] <- subtle_palette[subtle_idx]
    subtle_idx <- subtle_idx + 1
  }
}

family_colors["Other"] <- "darkgrey"
family_colors["Clostridiaceae"] <- "darkgreen"

# Create plot
abundance_plot <- ggplot(long_data, aes(x = Sample, y = Abundance, fill = Family)) +
  geom_col(color = "black", size = 0.1) +
  facet_grid(. ~ Diet_type, scales = "free_x", space = "free_x") +
  scale_fill_manual(values = family_colors) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    legend.position = "right",
    strip.background = element_blank(),
    strip.text = element_text(size = 18, face = "bold")
  ) +
  labs(y = "Relative Abundance (%)", fill = "Family") +
  guides(fill = guide_legend(ncol = 1))

# Save plot and color mapping
ggsave("output/figures/Relative_abundance_family.png", 
       plot = abundance_plot, width = 20, height = 10, dpi = 600)
