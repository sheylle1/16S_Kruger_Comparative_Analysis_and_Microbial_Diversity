# Alpha Diversity Analysis
# Generates boxplots for Shannon, Faith PD, and Observed Features

library(tidyverse)
library(ggpubr)

# Load data
entropy_data <- readxl::read_excel("data/Entropydata.xlsx", sheet = "alpha-diversity")

# Define outlier samples
outliers <- c("K058689-R", "K058785-R", "K058814-R", "K058789-R", 
              "K058706-R", "K058683-R")

# Clean data and set factor levels
entropy_clean <- entropy_data %>%
  filter(!Sample_id %in% outliers) %>%
  mutate(Diet_type = factor(Diet_type, levels = c("Carnivore", "Non-Ruminant", "Ruminant")))

# Custom color palette
diet_palette <- c("Carnivore" = "#ff5e2b", 
                  "Ruminant" = "#800080", 
                  "Non-Ruminant" = "#2E8B57")

# Function to create alpha diversity plot
create_alpha_plot <- function(data, y_var, y_label, y_breaks) {
  ggplot(data, aes(x = Diet_type, y = .data[[y_var]], fill = Diet_type)) +
    geom_boxplot(color = "black", size = 0.8, outlier.shape = NA) +
    geom_jitter(shape = 21, color = "black", fill = NA, width = 0.2, size = 2, stroke = 0.4) +
    scale_fill_manual(values = diet_palette) +
    scale_y_continuous(breaks = y_breaks) +
    labs(x = NULL, y = y_label) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.y = element_text(face = "bold"),
      legend.position = "none",
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)
    )
}

# Create plots
shannon_plot <- create_alpha_plot(entropy_clean, "shannon_entropy", "Shannon Entropy Index", seq(0, 6, 0.5))
faith_plot <- create_alpha_plot(entropy_clean, "faith_pd", "Faith PD", seq(0, 20, 2))
observed_plot <- create_alpha_plot(entropy_clean, "observed_features", "Observed Features", seq(0, 300, 25))

# Save plots
ggsave("output/figures/Shannon_diversity.png", plot = shannon_plot, width = 8, height = 6, dpi = 300)
ggsave("output/figures/Faith_pd.png", plot = faith_plot, width = 8, height = 6, dpi = 300)
ggsave("output/figures/Observed_features.png", plot = observed_plot, width = 8, height = 6, dpi = 300)

# Species-level alpha diversity
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

# Create species-level plot
species_plot <- ggplot(entropy_clean, 
       aes(x = Common.name_Species, y = shannon_entropy, fill = Common.name_Species)) +  
  geom_boxplot(color = "black", size = 0.7) +
  geom_jitter(shape = 21, color = "black", fill = NA, width = 0.2, size = 3, stroke = 0.4) +
  scale_fill_manual(values = animal_colors) +  
  scale_y_continuous(breaks = seq(0, 6, 0.5)) +
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

ggsave("output/figures/Shannon_species.png", plot = species_plot, width = 12, height = 6, dpi = 300)