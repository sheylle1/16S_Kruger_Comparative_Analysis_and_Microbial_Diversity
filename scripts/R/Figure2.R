# Alpha Diversity Analysis
# Generates boxplots for Shannon, Faith PD, and Observed Features

library(tidyverse)
library(ggpubr)
library(rstatix)

# Load data
entropy_data <- readxl::read_excel("data/raw/Entropydata.xlsx", sheet = "alpha-diversity")
outliers <- c("") 

# Clean data and set factor levels
entropy_clean <- entropy_data %>%
  filter(!Sample_id %in% outliers) %>%
  mutate(Diet_type = factor(Diet_type, levels = c("Carnivore", "Non-Ruminant", "Ruminant")))

# Total samples
total_samples <- nrow(entropy_clean)
# Samples by diet
diet_counts <- table(entropy_clean$Diet_type)
cat("\n\nSamples by diet type:")
print(diet_counts)

# Custom color palette
diet_palette <- c("Carnivore" = "#ff5e2b", "Ruminant" = "#2E8B57", "Non-Ruminant" = "#800080")

# Function to create alpha diversity plot with significance
create_alpha_plot <- function(data, y_var, y_label, y_breaks, y_min, y_max) {

  stat_test <- data %>%
    pairwise_wilcox_test(as.formula(paste(y_var, "~ Diet_type"))) %>%
    add_xy_position(x = "Diet_type", step.increase = 0.1)
  
  # Add significance column
  stat_test <- stat_test %>%
    mutate(p.signif = case_when( p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", TRUE ~ "ns"))
  
  # Create plot
  p <- ggplot(data, aes(x = Diet_type, y = .data[[y_var]], fill = Diet_type)) +
    geom_boxplot(color = "black", size = 0.8, outlier.shape = NA) +
    geom_jitter(shape = 21, color = "black", fill = NA, width = 0.2, size = 2, stroke = 0.4) +
    scale_fill_manual(values = diet_palette) +
    scale_y_continuous(breaks = y_breaks, limits = c(y_min, y_max)) +
    labs(x = NULL, y = y_label) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text( size = 16),
      axis.title.y = element_text(face = "bold", size = 18),
      legend.position = "none",
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)
    )
  
  # Add significance annotations only for significant comparisons
  stat_test_sig <- stat_test %>% filter(p.signif != "ns")
  if(nrow(stat_test_sig) > 0) {
    # Calculate y positions for significance bars (spaced out above the max value)
    y_positions <- seq(y_max * 0.93, y_max * 0.99, length.out = nrow(stat_test_sig))
    p <- p + stat_pvalue_manual(stat_test_sig, label = "p.signif",  tip.length = 0.02, y.position = y_positions)
  }
  
  return(p)
}

shannon_plot <- create_alpha_plot(entropy_clean, "shannon_entropy", "Shannon Entropy Index", 
                                   seq(4.5, 8, 0.5), y_min = 4.5, y_max = 8.5)

faith_max <- max(entropy_clean$faith_pd, na.rm = TRUE)
faith_plot <- create_alpha_plot(entropy_clean, "faith_pd", "Faith's Phylogenetic Diversity", 
                                 seq(0, ceiling(faith_max), 2), y_min = 0, y_max = faith_max + 3)

observed_max <- max(entropy_clean$observed_features, na.rm = TRUE)
observed_plot <- create_alpha_plot(entropy_clean, "observed_features", "Observed Features", 
                                    seq(0, 300, 25), y_min = 0, y_max = 325)

ggsave("outputs/figures/Fig2_A.png", plot = shannon_plot, width = 8, height = 6, dpi = 300)
ggsave("outputs/figures/Fig2_C.png", plot = faith_plot, width = 8, height = 6, dpi = 300)
ggsave("outputs/figures/Fig2_extra.png", plot = observed_plot, width = 8, height = 6, dpi = 300)

# Species-level alpha diversity with custom order
animal_colors <- c(
  "African buffalo" = "#228b22",
  "African elephant" = "#9e19f6",
  "African wild dog" = "#ff5e2b",
  "Black rhinoceros" = "#9370db",
  "Impala" = "#90ee90",
  "Lion" = "#fba300",
  "Spotted Hyaena" = "#ffd580",
  "Warthog" = "#800080",
  "White rhinoceros" = "#d88fd8"
)

species_order <- c("African wild dog", "Lion", "Spotted Hyaena",
                   "Warthog", "African elephant", "Black rhinoceros", "White rhinoceros",
                   "African buffalo", "Impala")

# Set factor levels with custom order
entropy_clean_species <- entropy_clean %>%
  mutate(Common.name_Species = factor(Common.name_Species, levels = species_order))
# Create label mapper for species
species_labels <- c(
  "African wild dog" = "African wild dog",
  "Lion" = "Lion",
  "Spotted Hyaena" = "Spotted hyaena",  # Changed display name only
  "Warthog" = "Warthog",
  "African elephant" = "African elephant",
  "Black rhinoceros" = "Black rhinoceros",
  "White rhinoceros" = "White rhinoceros",
  "African buffalo" = "African buffalo",
  "Impala" = "Impala"
)

# Calculate max y value for species plot (Faith PD)
y_max_species <- max(entropy_clean_species$faith_pd, na.rm = TRUE)

# Create species-level plot with Faith PD
species_plot <- ggplot(entropy_clean_species, 
                       aes(x = Common.name_Species, y = faith_pd, fill = Common.name_Species)) +  
  geom_boxplot(color = "black", size = 0.7) +
  geom_jitter(shape = 21, color = "black", fill = NA, width = 0.2, size = 3, stroke = 0.4) +
  scale_fill_manual(values = animal_colors) +  
  scale_x_discrete(labels = species_labels) + 
  scale_y_continuous(breaks = seq(0, ceiling(y_max_species), by = 2), 
                     limits = c(0, y_max_species + 3)) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size = 16),
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold", size = 18),
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  ) +
  labs(y = "Faith's Phylogenetic Diversity") +
  geom_vline(xintercept = c(3.5, 7.5), linetype = "dashed", color = "gray30", size = 0.8)

# Save species plot
ggsave("outputs/figures/Fig2_D.png", plot = species_plot, width = 12, height = 6, dpi = 300)

# Create Shannon species plot with updated y-axis (min 4.5, max 8.5)
shannon_species_max <- max(entropy_clean_species$shannon_entropy, na.rm = TRUE)
shannon_species_plot <- ggplot(entropy_clean_species, 
                               aes(x = Common.name_Species, y = shannon_entropy, fill = Common.name_Species)) +  
  geom_boxplot(color = "black", size = 0.7) +
  geom_jitter(shape = 21, color = "black", fill = NA, width = 0.2, size = 3, stroke = 0.4) +
  scale_fill_manual(values = animal_colors) +  
  scale_x_discrete(labels = species_labels) +  # This changes how labels appear on plot
  scale_y_continuous(breaks = seq(4.5, 8, 0.5), limits = c(4.5, 8.5)) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size = 16),
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold", size = 18),
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  ) +
  labs(y = "Shannon Entropy Index") +
  geom_vline(xintercept = c(3.5, 7.5), linetype = "dashed", color = "gray30", size = 0.8)

ggsave("outputs/figures/Fig2_B.png", plot = shannon_species_plot, width = 12, height = 6, dpi = 300)

library(patchwork)
library(cowplot)

# Load the individual plots (or recreate them)
# Since you already created the plots above, we'll use them directly

# Add panel letters to each plot using cowplot
# Panel A: Shannon by diet
panel_A <- shannon_plot + 
  labs(tag = "A") +
  theme(plot.tag = element_text(size = 24, face = "bold", family = "Arial"),
        plot.tag.position = c(0.02, 0.98))  # Top-left corner

# Panel B: Shannon by species  
panel_B <- shannon_species_plot + 
  labs(tag = "B") +
  theme(plot.tag = element_text(size = 24, face = "bold", family = "Arial"),
        plot.tag.position = c(0.02, 0.98))

# Panel C: Faith PD by diet
panel_C <- faith_plot + 
  labs(tag = "C") +
  theme(plot.tag = element_text(size = 24, face = "bold", family = "Arial"),
        plot.tag.position = c(0.02, 0.98))

# Panel D: Faith PD by species
panel_D <- species_plot + 
  labs(tag = "D") +
  theme(plot.tag = element_text(size = 24, face = "bold", family = "Arial"),
        plot.tag.position = c(0.02, 0.98))

combined_figure <- (panel_A + panel_B + plot_layout(widths = c(1, 2))) /
                   (panel_C + panel_D + plot_layout(widths = c(1, 2)))

# Add a title for the entire figure (optional)
combined_figure <- combined_figure +
  plot_annotation(
    title = NULL,  # Add title if desired
    theme = theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))
  )

# Display the combined figure
print(combined_figure)

# Save the combined figure
ggsave("outputs/Final/Figure2.png", 
       plot = combined_figure, 
       width = 22, 
       height = 12, 
       dpi = 300,
       bg = "white")

# Also save as PDF for publication quality
ggsave("outputs/Final/Figure2.pdf", 
       plot = combined_figure, 
       width = 22, 
       height = 12, 
       dpi = 300,
       bg = "white")

