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

# Extract genus names
genus_names <- sapply(tax_cols, function(col) {
  parts <- unlist(strsplit(col, ";"))
  f_part <- parts[grep("^g__", parts)]
  if (length(f_part) == 0) return("Unclassified")
  genus <- sub("^g__", "", f_part)
  genus <- unlist(strsplit(genus, ";"))[1]
  ifelse(genus == "", "Unclassified", genus)
})

# Aggregate to genus level
abundance_t <- t(abundance)
abundance_genus <- rowsum(abundance_t, group = genus_names, reorder = FALSE)
abundance_genus <- t(abundance_genus)
rownames(abundance_genus) <- sample_ids

# Remove "Unclassified"
abundance_genus_df <- as.data.frame(abundance_genus)
if ("Unclassified" %in% colnames(abundance_genus_df)) {
  abundance_genus_df$Unclassified <- NULL
}

# Normalize to relative abundance
abundance_genus_rel <- as.data.frame(abundance_genus_df / rowSums(abundance_genus_df) * 100)

# Add metadata
abundance_genus_rel$Sample <- rownames(abundance_genus_rel)
abundance_genus_rel$Diet_type <- diet_type
abundance_genus_rel$Species <- species_name
abundance_genus_rel$Animal <- animals

# Get top genera
overall_top <- colSums(abundance_genus_rel[, 1:(ncol(abundance_genus_rel)-4)])
top_genera <- names(sort(overall_top, decreasing = TRUE)[1:30])

top_genera

# Calculate 'Other' category
top_data <- abundance_genus_rel[, top_genera, drop = FALSE]
other <- 100 - rowSums(top_data)
final_data <- cbind(top_data, Other = other)

# Add metadata
final_data$Sample <- abundance_genus_rel$Sample
final_data$Diet_type <- factor(abundance_genus_rel$Diet_type, levels = c("Carnivore", "Non-Ruminant", "Ruminant"))
final_data$Species <- abundance_genus_rel$Species
final_data$Animal <- abundance_genus_rel$Animal

# Sort Samples within group by Species
final_data <- final_data %>%
  arrange(Diet_type, Species) %>%
  mutate(Sample = factor(Sample, levels = Sample))  # Preserve sample order

# Pivot to long format
long_data <- final_data %>%
  pivot_longer(cols = -c(Sample, Diet_type, Species, Animal), names_to = "genus", values_to = "Abundance") %>%
  mutate(genus = factor(genus, levels = c(top_genera, "Other")))


genus_colors <- read.csv("/data/raw/family_colors_genus.csv")
genus_colors_v <- setNames(genus_colors$Color, genus_colors$Family)

ggplot(long_data, aes(x = Sample, y = Abundance, fill = genus)) +
  geom_col(color = "black", size = 0.1) +
  facet_grid(. ~ Diet_type, scales = "free_x", space = "free_x") +
  scale_fill_manual(values = genus_colors_v) +
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
  labs(y = "Relative Abundance (%)", fill = "Genera") +
  guides(fill = guide_legend(ncol = 1))


ggsave("output/figures/Genus.png", width = 20, height = 10, dpi = 600)

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

c <- ggplot(c_data, aes(x = Animal, y = Abundance, fill = genus)) +
  geom_col(color = "black", size = 0.2) +
  facet_wrap(~ Species,
             scales = "free_x",
             nrow = 1,
             labeller = labeller(Species = species_labels)) +
  scale_fill_manual(values = genus_colors_v) +
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
    fill = "Bacterial Genus"
  )
c

ggsave("output/figures/microbiome_C_genus.png", width = 15, height = 6, dpi = 600)

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
nr <- ggplot(nr_data, aes(x = Animal, y = Abundance, fill = genus)) +
  geom_col(color = "black", size = 0.2) +
  facet_wrap(~ Species, scales = "free_x", nrow = 1) +
  scale_fill_manual(values = genus_colors_v) +
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
    fill = "Bacterial Genus"
  )
nr
ggsave("output/figures/microbiome_NR_genus.png", width = 20, height = 6, dpi = 600)

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

r <- ggplot(r_data, aes(x = Animal, y = Abundance, fill = genus)) +
  geom_col(color = "black", size = 0.2) +
  facet_wrap(~ Species, scales = "free_x", nrow = 1) +
  scale_fill_manual(values = genus_colors_v) +
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
    fill = "Bacterial Genus"
  )
r
ggsave("output/figures/microbiome_R_genus.png", width = 10, height = 6, dpi = 600)


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

legend_spacer <- plot_spacer()

#  Stack plots with legend spacer
final_combined_plot <- (c_centered / nr_centered / r_centered / legend_spacer / wrap_elements(shared_legend)) +
  plot_layout(heights = c(1, 1, 1, 0.05, 0.2)) +  # increase last number for more space for legend
  plot_annotation(
    theme = theme(plot.margin = ggplot2::margin(10, 10, 10, 10, unit = "pt"))
  )

final_combined_plot
# Save
for(ext in c("png","pdf","tiff")){
  ggsave(
    filename = paste0("Figure_8.", ext),
    plot = final_combined_plot,
    width = max_width,
    height = 18,
    dpi = 600
  )
}
