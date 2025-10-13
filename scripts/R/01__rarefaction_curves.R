# Rarefaction Curve Analysis
# Generates Supplementary Figure 1

library(tidyverse)
library(viridis)

# Load data
df <- read_csv("data/observed_features.csv")

# Define outlier samples
outliers <- c("K058689-R", "K058785-R", "K058814-R", "K058789-R", 
              "K058706-R", "K058683-R", "K058700-R", "K058796-R", 
              "K058824-R", "K058810-R", "K058694-R")

# Process data for diet-type rarefaction curves
df_long <- df %>%
  select(`sample-id`, Diet_type, starts_with("depth")) %>%
  pivot_longer(
    cols = starts_with("depth"),
    names_to = "depth_iter",
    values_to = "observed_features"
  ) %>%
  mutate(
    sequencing_depth = as.numeric(str_extract(depth_iter, "(?<=depth-)[0-9]+"))
  )

# Average observed features per sample and depth
df_summary <- df_long %>%
  group_by(`sample-id`, Diet_type, sequencing_depth) %>%
  summarise(mean_observed = mean(observed_features, na.rm = TRUE), .groups = "drop")

# Prepare outlier endpoints
endpoints <- df_summary %>%
  filter(`sample-id` %in% outliers, !is.na(mean_observed)) %>%
  group_by(`sample-id`) %>%
  slice_max(order_by = sequencing_depth, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(outlier_status = "Outlier",
         sequencing_depth = if_else(`sample-id` == "K058814-R", 4445, sequencing_depth))

# Add dummy column for consistent shape mapping
df_summary <- df_summary %>% mutate(outlier_status = NA_character_)

# Combine for plotting
df_plot <- bind_rows(df_summary, endpoints)

# Create rarefaction curve plot
rarefaction_plot <- ggplot(df_plot, aes(x = sequencing_depth, y = mean_observed, group = `sample-id`)) +
  geom_line(aes(color = factor(Diet_type)), size = 1, alpha = 0.85) +
  geom_point(
    data = filter(df_plot, outlier_status == "Outlier"),
    aes(shape = outlier_status),
    color = "red", size = 2
  ) +
  scale_color_viridis_d(option = "D", direction = 1, name = "Diet Type") +
  scale_shape_manual(name = "Outlier Status", values = c("Outlier" = 16)) +
  scale_x_continuous(
    breaks = seq(0, 11000, 2000),
    limits = c(0, 11000),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    breaks = seq(0, 280, 40),
    limits = c(0, 280),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    title = "Alpha Rarefaction Curves",
    x = "Sequencing Depth",
    y = "Observed Features"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    legend.position = "right",
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank()
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 3)),
    shape = guide_legend(override.aes = list(size = 3, color = "red"))
  )

# Save plot
ggsave("output/figures/Supp_Fig1_rarefaction_curves.png", 
       plot = rarefaction_plot, 
       width = 8, height = 6, dpi = 300)