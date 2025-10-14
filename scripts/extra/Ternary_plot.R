# Ternary Plot for Kruger Wild Mammal Data

library(ggtern)
library(dplyr)
library(readr)

install.packages("ggtern")


library(tidyverse)

# Read data
df <- read.csv("D:/CERI/Carla_16S/Manuscript_analysis/data/raw/level-6.csv", row.names = 1, check.names = FALSE)

outliers <- c("K058683-R", "K058706-R", "K058789-R", "K058689-R", "K058814-R", "K058785-R")
Out <-  c("K058700-R", "K058796-R", "K058824-R", "K058810-R", "K058694-R")

# Pivot longer (only taxonomy columns, not metadata)
# Pivot longer (only taxonomy columns, not metadata)
df_long <- df %>%
  rownames_to_column("sample_id") %>%
  pivot_longer(
    cols = starts_with("d__Bacteria"),
    names_to = "species",
    values_to = "abundance"
  ) %>%
  # Keep only rows where abundance > 0
  filter(abundance > 0) %>%
  # Keep only rows that contain "g__"
  filter(grepl("g__", species)) %>%
  # Extract only genus after "g__"
  mutate(species = sub(".*g__", "", species)) %>%
  # Remove any empty genus just in case
  filter(species != "") %>%
  # Add Diet_type column
  mutate(taxonomic_group = df$Diet_type[match(sample_id, rownames(df))]) %>%
  select(sample_id, species, abundance, taxonomic_group)


# View result
head(df_long)

df_long <- df_long[!(df_long$sample_id %in% outliers), ]
df_long <- df_long[!(df_long$sample_id %in% Out), ]

# Aggregate by genus and taxonomic group
genus_summary <- df_long %>%
  group_by(species, taxonomic_group) %>%
  summarise(total_abundance = sum(abundance), .groups = "drop") %>%
  pivot_wider(
    names_from = taxonomic_group,
    values_from = total_abundance,
    values_fill = 0
  ) %>%
  # Standardize column names
  rename(
    Non_Ruminant = `Non-Ruminant`
  ) %>%
  mutate(
    total = Ruminant + Non_Ruminant + Carnivore,
    Ruminant_prop     = Ruminant / total,
    Non_Ruminant_prop = Non_Ruminant / total,
    Carnivore_prop    = Carnivore / total
  )

# Count number of samples each genus occurs in
genus_counts <- df_long %>%
  group_by(species) %>%
  summarise(sample_count = n_distinct(sample_id), .groups = "drop")

# Merge with top_genera
top_genera <- top_genera %>%
  left_join(genus_counts, by = "species")

# Ternary plot with size reflecting number of samples
ternary_plot <- ggtern(top_genera, 
                       aes(x = Ruminant_prop,
                           y = Non_Ruminant_prop,
                           z = Carnivore_prop,
                           color = species)) +
  geom_point(size=6,alpha = 0.8) +
  theme_rgbw() +
  labs(
    x = "Ruminant",
    y = "Non-Ruminant",
    z = "Carnivore",
    color = "Genus"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "right",
    legend.text = element_text(size = 8)
  ) +
  scale_size_continuous(range = c(3, 8)) 

print(ternary_plot)

