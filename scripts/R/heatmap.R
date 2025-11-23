# Load required libraries
library(pheatmap)
library(viridis)
library(dplyr)
library(tidyr)
library(gridExtra)
library(tibble)

# Original animal data
animal_data <- data.frame(
  Family = c("Pasteurellaceae", "Campylobacteraceae", "Campylobacteraceae", "Campylobacteraceae",
             "Campylobacteraceae", "Campylobacteraceae", "Campylobacteraceae", "Campylobacteraceae", "Campylobacteraceae",
             "Coxiellaceae", "Enterobacteriaceae Salmonella", "Enterobacteriaceae Salmonella", "Enterobacteriaceae Salmonella",
             "Enterobacteriaceae Salmonella", "Enterobacteriaceae Salmonella", "Enterobacteriaceae Salmonella", 
             "Enterobacteriaceae Salmonella", "Enterobacteriaceae Salmonella", "Enterobacteriaceae Salmonella",
             "Leptospiraceae", "Staphylococcaceae", "Staphylococcaceae"),
  Animal = c("Lion", "African wild dog", "Spotted hyaena", "Lion", "African elephant", "White rhinoceros", 
             "Warthog", "African buffalo", "Impala", "Warthog", "African wild dog", "Lion", "Spotted hyaena",
             "African elephant", "Black rhinoceros", "White rhinoceros", "Warthog", "African buffalo", "Impala",
             "African elephant", "White rhinoceros", "Spotted hyaena"),
  Reads = c(12, 148, 76, 47, 84, 10, 40, 7, 57, 8, 706, 1338, 206, 53, 231, 164, 22, 327, 516, 14, 182, 63)
)

genus_info <- data.frame(
  Family = c("Pasteurellaceae", "Campylobacteraceae", "Coxiellaceae", 
             "Enterobacteriaceae Salmonella", "Leptospiraceae", "Staphylococcaceae"),
  Genus = c("Pasteurella  haemolytica", "Campylobacter jejuni", "Coxiella burnetil", "Escherichia-Shigella", "Leptospira spp.", "Staphylococcus/Macrococcus")
)

animal_groups <- data.frame(
  Group = c(rep("Carnivore", 3), rep("Non-ruminant", 4), rep("Ruminant", 2)),
  row.names = c("Lion", "Spotted hyaena", "African wild dog", 
                "African elephant", "White rhinoceros", "Black rhinoceros", "Warthog",
                "African buffalo", "Impala")
)

group_colors <- list(
  Group = c("Carnivore" = "#ff5e2b", 
            "Ruminant" = "#800080", 
            "Non-ruminant" = "#2E8B57")
)

# Species counts to add in brackets
species_data <- data.frame(
  Animal = c("Lion","African wild dog","Spotted hyaena","African elephant",
             "White rhinoceros","Black rhinoceros","Warthog","African buffalo","Impala"),
  Species = c(2,2,3,4,2,1,2,1,2) # Fill in according to your data
)

# Modify animal names to include species count
col_names <- paste0(species_data$Animal, " (", species_data$Species, ")")
names(col_names) <- species_data$Animal  # mapping

# FAMILY MATRIX
family_matrix <- animal_data %>%
  group_by(Family, Animal) %>%
  summarise(Reads = sum(Reads), .groups = 'drop') %>%
  pivot_wider(names_from = Animal, values_from = Reads, values_fill = 0) %>%
  column_to_rownames("Family") %>%
  as.matrix() %>%
  .[, species_data$Animal]  # order columns

# GENUS MATRIX
genus_matrix <- animal_data %>%
  left_join(genus_info, by = "Family") %>%
  group_by(Genus, Animal) %>%
  summarise(Reads = sum(Reads), .groups = 'drop') %>%
  pivot_wider(names_from = Animal, values_from = Reads, values_fill = 0) %>%
  column_to_rownames("Genus") %>%
  as.matrix() %>%
  .[, species_data$Animal]  # order columns

# Function to generate a pheatmap and return as a grob
pheatmap_grob <- function(mat, title){
  # Create the heatmap first without numbers to get the row order
  p_temp <- pheatmap(mat,
                     color = viridis(100),
                     cluster_rows = TRUE,
                     cluster_cols = FALSE,
                     annotation_col = animal_groups,
                     annotation_colors = group_colors,
                     main = title,
                     fontsize = 12,
                     angle_col = 45,
                     display_numbers = FALSE,  # No numbers initially
                     border_color = "grey60",
                     cellwidth = 40,
                     cellheight = 30,
                     silent = TRUE)  # silent to prevent plotting
  
  # Get the reordered matrix after clustering
  mat_reordered <- mat[p_temp$tree_row$order, , drop = FALSE]
  
  # Create number colors based on the reordered matrix
  number_colors <- ifelse(mat_reordered > 1000, "black", "white")
  
  # Now create the final plot with numbers
  p <- pheatmap(mat,
                color = viridis(100),
                cluster_rows = TRUE,
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
                cellheight = 30)
  return(p$gtable)
}


# Create grobs for side-by-side display
grob_family <- pheatmap_grob(family_matrix, "")

for (ext in c("png", "pdf", "tiff")) {
  ggsave(
    filename = paste0("Figure_6(family).", ext),
    plot = last_plot(),
    width = 10,
    height = 8,
    dpi = 600
  )
}

grob_genus  <- pheatmap_grob(genus_matrix,  "")

library(ggplot2)

for (ext in c("png", "pdf", "tiff")) {
  ggsave(
    filename = paste0("Figure_6.", ext),
    plot = grob_genus,
    width = 11,
    height = 5,
    dpi = 600
  )
}


# Arrange side by side
pdf("Family_and_Genus_Heatmaps.pdf", width=16, height=8)  # Save as PDF
grid.arrange(grob_family, grob_genus, ncol=2)
dev.off()


