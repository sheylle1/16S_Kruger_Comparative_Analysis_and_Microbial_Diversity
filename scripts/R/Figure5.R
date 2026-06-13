library(pheatmap)
library(viridis)
library(dplyr)
library(tidyr)
library(gridExtra)
library(tibble)
library(ggplot2)

all_data <- read.csv("D:/Downloads/Data_heatmap.csv", stringsAsFactors = FALSE)

species_data <- all_data %>%
  select(Animal, Species) %>%
  filter(!is.na(Species) & Species > 0) %>%  
  group_by(Animal) %>%
  summarise(Species = sum(Species), .groups = "drop")

# Get animal groups
animal_groups_df <- all_data %>%
  select(Animal, Group) %>%
  distinct() %>%
  filter(!is.na(Group))

# Ensure we only include animals that have species data
animal_groups_df <- animal_groups_df %>%
  filter(Animal %in% species_data$Animal)

# Order animal_groups consistently with species_data
animal_groups <- animal_groups_df %>%
  arrange(match(Animal, species_data$Animal)) %>%
  column_to_rownames("Animal") %>%
  as.data.frame()

group_colors <- list(
  Group = c("Carnivore" = "#ff5e2b", 
            "Ruminant" = "#800080", 
            "Non-Ruminant" = "#2E8B57")
)

# Modify animal names to include species count
col_names <- paste0(species_data$Animal)
names(col_names) <- species_data$Animal

# Rename columns in animal_groups to match the display names
rownames(animal_groups) <- col_names[rownames(animal_groups)]

family_matrix <- all_data %>%
  filter(!is.na(Reads) & Reads > 0) %>%
  group_by(Family, Animal) %>%
  summarise(Reads = sum(Reads), .groups = 'drop') %>%
  filter(Animal %in% species_data$Animal) %>%
  pivot_wider(names_from = Animal, values_from = Reads, values_fill = 0) %>%
  column_to_rownames("Family") %>%
  as.matrix()

colnames(family_matrix) <- col_names[colnames(family_matrix)]

genus_matrix <- all_data %>%
  filter(!is.na(Reads) & Reads > 0, !is.na(Genus)) %>%
  group_by(Genus, Animal) %>%
  summarise(Reads = sum(Reads), .groups = 'drop') %>%
  filter(Animal %in% species_data$Animal) %>%
  pivot_wider(names_from = Animal, values_from = Reads, values_fill = 0) %>%
  column_to_rownames("Genus") %>%
  as.matrix()

colnames(genus_matrix) <- col_names[colnames(genus_matrix)]

family_matrix <- family_matrix[rowSums(family_matrix) > 0, ]
genus_matrix <- genus_matrix[rowSums(genus_matrix) > 0, ]

pheatmap_grob <- function(mat, title) {
  
  # First create heatmap to get clustering
  p_temp <- pheatmap(mat,
                     color = viridis(100),
                     cluster_rows = TRUE,
                     cluster_cols = FALSE,
                     annotation_col = animal_groups,
                     annotation_colors = group_colors,
                     main = title,
                     fontsize = 12,
                     angle_col = 45,
                     display_numbers = FALSE,
                     border_color = "grey60",
                     cellwidth = 40,
                     cellheight = 30,
                     silent = TRUE)
  
  # Reorder matrix according to clustering
  if (!is.null(p_temp$tree_row)) {
    mat <- mat[p_temp$tree_row$order, , drop = FALSE]
  }
  
  # Create matrix for number colors based on values
  number_colors <- matrix("white", nrow = nrow(mat), ncol = ncol(mat))
  number_colors[mat > 1000] <- "black"
  
  # Create final heatmap with reordered matrix
  p <- pheatmap(mat,
                color = viridis(100),
                cluster_rows = TRUE,  # Already clustered
                cluster_cols = FALSE,
                annotation_col = animal_groups,
                annotation_colors = group_colors,
                main = title,
                fontsize = 12,
                angle_col = 45,
                display_numbers = TRUE,
                number_color = number_colors,
                border_color = "grey60",
                cellwidth = 40,
                cellheight = 30,
                silent = TRUE)
  
  return(p$gtable)
}

# Create grobs
grob_family <- pheatmap_grob(family_matrix, "Family Level")
grob_genus <- pheatmap_grob(genus_matrix, "Genus Level")

# Display plots
grid::grid.newpage()
grid::grid.draw(grob_family)

grid::grid.newpage()
grid::grid.draw(grob_genus)

# Save individual heatmaps
for (ext in c("png", "tiff")) {
  ggsave(
    filename = paste0("outputs/figures/Figure5_extra.", ext),
    plot = grob_family,
    width = 11,
    height = 8,
    dpi = 600,
    limitsize = FALSE
  )
  
  ggsave(
    filename = paste0("outputs/Final/Figure5.", ext),
    plot = grob_genus,
    width = 11,
    height = 8,
    dpi = 600,
    limitsize = FALSE
  )
}

# Save matrices
write.csv(family_matrix, "family_matrix_processed.csv")
write.csv(genus_matrix, "genus_matrix_processed.csv")