# Relative Abundance Analysis - Family Level

library(tidyverse)
library(RColorBrewer)
library(stringr)

# Load data
data <- read.csv("/data/raw/level-6.csv", check.names = FALSE, stringsAsFactors = FALSE)

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

## Carinvores 

c_species <- c("African wild dog", "Lion", "Spotted Hyaena")

c_data <- long_data %>%
  filter(Species %in% c_species) %>%
  mutate(Species = factor(Species, levels = c_species))

c_data <- c_data %>%
  mutate(Animal_num = as.numeric(str_extract(Animal, "\\d+$"))) %>%
  arrange(Species, Animal_num) %>%
  mutate(Animal = factor(Animal, levels = unique(Animal))) %>%
  select(-Animal_num)

#Define custom facet labels
species_labels <- c(
  "African wild dog" = "African wild dog",
  "Lion" = "Lion",
  "Spotted Hyaena" = "Spotted hyaena"
)

c <- ggplot(c_data, aes(x = Animal, y = Abundance, fill = Family)) +
  geom_col(color = "black", size = 0.2) +
  facet_wrap(~ Species,
             scales = "free_x",
             nrow = 1,
             labeller = labeller(Species = species_labels)) +
  scale_fill_manual(values = family_colors) +
  theme_bw(base_size = 16) +  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16),
    strip.text = element_text(face = "bold", size = 18),
    legend.title = element_text(face = "bold", size = 16),
    legend.text = element_text(size = 14),
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    strip.background = element_blank(),
    legend.position = "none"
  ) +
  labs(
    x = "",
    y = "Relative Abundance (%)",
    fill = "Bacterial Family"
  )
c
ggsave("output/figures/microbiome_C_Family.png", width = 15, height = 6, dpi = 600)

## Non-Ruminants

nr_species <- c("Warthog", "African elephant", "Black rhinoceros", "White rhinoceros")

nr_data <- long_data %>%
  filter(Species %in% nr_species) %>%
  mutate(Species = factor(Species, levels = nr_species))

nr_data <- nr_data %>%
  mutate(Animal_num = as.numeric(str_extract(Animal, "\\d+$"))) %>%
  arrange(Species, Animal_num) %>%
  mutate(Animal = factor(Animal, levels = unique(Animal))) %>%
  select(-Animal_num)

# ==== Final plot ====
nr <- ggplot(nr_data, aes(x = Animal, y = Abundance, fill = Family)) +
  geom_col(color = "black", size = 0.2) +
  facet_wrap(~ Species, scales = "free_x", nrow = 1) +
  scale_fill_manual(values = family_colors) +
  theme_bw(base_size = 16) +  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16),
    strip.text = element_text(face = "bold", size = 18),
    legend.title = element_text(face = "bold", size = 16),
    legend.text = element_text(size = 14),
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    strip.background = element_blank(),
    legend.position = "none"
  ) +
  labs(
    x = "",
    y = "Relative Abundance (%)",
    fill = "Bacterial Family"
  )
nr
ggsave("output/figures/microbiome_NR_Family.png", width = 10, height = 6, dpi = 600)

## Ruminants

r_species <- c("African buffalo", "Impala")

r_data <- long_data %>%
  filter(Species %in% r_species) %>%
  mutate(Species = factor(Species, levels = r_species))

r_data <- r_data %>%
  mutate(Animal_num = as.numeric(str_extract(Animal, "\\d+$"))) %>%
  arrange(Species, Animal_num) %>%
  mutate(Animal = factor(Animal, levels = unique(Animal))) %>%
  select(-Animal_num)

r <- ggplot(r_data, aes(x = Animal, y = Abundance, fill = Family)) +
  geom_col(color = "black", size = 0.2) +
  facet_wrap(~ Species, scales = "free_x", nrow = 1) +
  scale_fill_manual(values = family_colors) +
  theme_bw(base_size = 16) +  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16),
    strip.text = element_text(face = "bold", size = 18),
    legend.title = element_text(face = "bold", size = 16),
    legend.text = element_text(size = 14),
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    strip.background = element_blank(),
    legend.position = "none"
  ) +
  labs(
    x = "",
    y = "Relative Abundance (%)",
    fill = "Bacterial Family"
  )
r
ggsave("output/figures/microbiome_R_Family.png", width = 10, height = 6, dpi = 600)


library(ggplot2)
library(patchwork)
library(cowplot)

# Extract shared legend from one plot
legend_plot <- c + theme(legend.position = "bottom")
shared_legend <- cowplot::get_legend(
  legend_plot + theme(legend.box.margin = ggplot2::margin(0, 0, 0, 0))
)

# Wrap plots to remove individual legends
c_no_legend  <- c  + theme(legend.position = "none")
nr_no_legend <- nr + theme(legend.position = "none")
r_no_legend  <- r  + theme(legend.position = "none")

#  Wrap each plot in a fixed-width container (approximate relative widths)
# We'll use `plot_spacer()` on sides to center narrower plots
max_width <- 20   # width of the widest plot (nr)
center_plot <- function(plot, target_width, max_width){
  left_pad <- (max_width - target_width)/2
  right_pad <- max_width - target_width - left_pad
  wrap_plots(
    plot_spacer(),  # left padding
    plot,
    plot_spacer(),  # right padding
    ncol = 3,
    widths = c(left_pad, target_width, right_pad)
  )
}

c_centered  <- center_plot(c_no_legend, 15, max_width)
nr_centered <- center_plot(nr_no_legend, 20, max_width)
r_centered  <- center_plot(r_no_legend, 10, max_width)

# Stack the three plots vertically
combined_plots <- c_centered / nr_centered / r_centered

# Add the shared legend below
final_combined_plot <- combined_plots / wrap_elements(shared_legend) +
  plot_layout(heights = c(1, 1, 1, 0.15)) +
  plot_annotation(
    theme = theme(plot.margin = ggplot2::margin(10, 10, 10, 10, unit = "pt"))
  )

final_combined_plot
# Save
for(ext in c("png","pdf","tiff")){
  ggsave(
    filename = paste0("Figure_7.", ext),
    plot = final_combined_plot,
    width = max_width,
    height = 18,
    dpi = 600
  )
}
