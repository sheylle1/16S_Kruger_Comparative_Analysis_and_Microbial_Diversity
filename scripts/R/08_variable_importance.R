# Variable Importance Analysis for Microbiome Data
-
library(tidyverse)
library(caret)
library(randomForest)
library(viridis)
library(ggpubr)
library(patchwork)


data <- read.csv("D:/data/level-6.csv", check.names = FALSE)

# Remove outliers
outliers <- c("K058683-R", "K058706-R", "K058789-R", "K058689-R", 
              "K058814-R", "K058785-R", "K058700-R", "K058796-R", 
              "K058824-R", "K058810-R", "K058694-R")
data <- data[!data$index %in% outliers, ]

# Prepare metadata
metadata <- data.frame(
  SampleID = data$index,
  Diet_type = factor(make.names(data$Diet_type)),
  stringsAsFactors = FALSE
)

# Extract abundance data
abundance_table <- data[, grep("d__Bacteria", colnames(data))]
rownames(abundance_table) <- metadata$SampleID

# Extract family names from taxonomy strings
extract_family <- function(tax_string) {
  if (grepl("f__", tax_string)) {
    family <- gsub(".*;f__([^;]+).*", "\\1", tax_string)
  } else {
    family <- NA
  }
  return(family)
}

family_names <- sapply(colnames(abundance_table), extract_family)

# Aggregate to family level
abundance_table_family <- rowsum(t(abundance_table), group = family_names, na.rm = TRUE)
abundance_table_family <- t(abundance_table_family)
abundance_table_family <- abundance_table_family[, !is.na(colnames(abundance_table_family))]

# CLR transformation
clr_transform <- function(x) {
  x <- x + 1
  log(x / exp(mean(log(x))))
}

abundance_table_clr <- t(apply(abundance_table_family, 1, clr_transform))

# Combine with metadata
rf_data <- cbind(Diet_type = metadata$Diet_type, as.data.frame(abundance_table_clr))
rf_data$Diet_type <- as.factor(rf_data$Diet_type)

#RF Classification
set.seed(123)
train_control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  savePredictions = "final"
)

rf_model <- train(
  Diet_type ~ .,
  data = rf_data,
  method = "rf",
  trControl = train_control,
  ntree = 500,
  importance = TRUE,
  metric = "Accuracy"
)

print(rf_model)

# Variable Importance
diet_classes <- levels(rf_data$Diet_type)
importance_list <- list()

for (diet in diet_classes) {
  rf_data_binary <- rf_data
  rf_data_binary$Diet_binary <- ifelse(rf_data_binary$Diet_type == diet, diet, paste0("Not_", diet))
  rf_data_binary$Diet_binary <- factor(rf_data_binary$Diet_binary)
  
  # Separate predictors & outcome
  x <- rf_data_binary %>% select(-Diet_type, -Diet_binary)
  y <- rf_data_binary$Diet_binary
  
  rf_bin <- randomForest(x = x, y = y, ntree = 500, importance = TRUE)
  
  imp <- importance(rf_bin, type = 1, scale = TRUE)
  imp_df <- data.frame(
    Taxa = rownames(imp),
    Importance = imp[, 1],
    Diet = diet
  )
  
  importance_list[[diet]] <- imp_df
}

importance_long <- bind_rows(importance_list)

# Identify Diet with Highest Abundance 
clr_df <- as.data.frame(abundance_table_clr)
clr_df$Diet_type <- rf_data$Diet_type

clr_long <- clr_df %>%
  pivot_longer(cols = -Diet_type, names_to = "Taxa", values_to = "CLR")

mean_abund <- clr_long %>%
  group_by(Taxa, Diet_type) %>%
  summarise(MeanCLR = mean(CLR), .groups = "drop")

winner_diet <- mean_abund %>%
  group_by(Taxa) %>%
  slice_max(order_by = MeanCLR, n = 1) %>%
  select(Taxa, Winner = Diet_type)

# Merge with importance data
importance_long <- importance_long %>%
  left_join(winner_diet, by = "Taxa")

# Clean Labels and Filter Taxa
# Fix the recode syntax - use proper dplyr recode format
importance_long <- importance_long %>%
  filter(!grepl("^d_Bacteria;p_", Taxa)) %>%
  mutate(
    Diet = case_when(
      Diet == "Non.Ruminant" ~ "Non Ruminant",
      Diet == "Ruminant" ~ "Ruminant",
      TRUE ~ Diet  # Keep original value if no match
    ),
    Winner = case_when(
      Winner == "Non.Ruminant" ~ "Non Ruminant",
      Winner == "Ruminant" ~ "Ruminant",
      TRUE ~ Winner
    )
  )

# Get Top 20 Taxa and Set Order 
# Get top 20 taxa overall by maximum importance
top_taxa <- importance_long %>%
  group_by(Taxa) %>%
  summarise(MaxImportance = max(Importance), .groups = "drop") %>%
  slice_max(order_by = MaxImportance, n = 20, with_ties = FALSE) %>%
  pull(Taxa)

# Set the order for plotting (from highest to lowest importance)
taxa_order <- importance_long %>%
  filter(Taxa %in% top_taxa) %>%
  group_by(Taxa) %>%
  summarise(MaxImportance = max(Importance), .groups = "drop") %>%
  arrange(desc(MaxImportance)) %>%
  pull(Taxa)

# Convert to factors with the correct order
importance_long$Taxa <- factor(importance_long$Taxa, levels = taxa_order)

# Also prepare CLR data for the distribution plot
clr_long_top <- clr_long %>%
  filter(Taxa %in% top_taxa) %>%
  mutate(
    Taxa = factor(Taxa, levels = taxa_order),
    Diet_type = case_when(
      Diet_type == "Non.Ruminant" ~ "Non Ruminant",
      Diet_type == "Ruminant" ~ "Ruminant",
      TRUE ~ Diet_type
    )
  ) %>%
  # Ensure CLR is numeric
  mutate(CLR = as.numeric(CLR))

# Fix Diet names in importance_long for consistency using the same approach
importance_long$Diet <- case_when(
  importance_long$Diet == "Non.Ruminant" ~ "Non Ruminant",
  TRUE ~ importance_long$Diet
)

# Ensure Importance is numeric
importance_long$Importance <- as.numeric(importance_long$Importance)

# Create Background Stripes Data

# Ensure taxa are characters
importance_long_char <- importance_long %>%
  filter(Taxa %in% top_taxa) %>%
  mutate(Taxa_char = as.character(Taxa))

clr_long_top_char <- clr_long_top %>%
  mutate(Taxa_char = as.character(Taxa))

# Option B: Based on mean importance (better visual logic)
taxa_order <- clr_long_top_char %>%
  group_by(Taxa_char) %>%
  summarise(mean_clr = mean(CLR, na.rm = TRUE)) %>%
  arrange(mean_clr) %>%
  pull(Taxa_char)

# Create proper ALTERNATING background stripes data frame
bg_stripes <- data.frame(
  Taxa_char = taxa_order
) %>%
  # Create alternating stripes based on row position
  mutate(
    stripe = (row_number() %% 2 == 1)  # TRUE for odd rows, FALSE for even rows
  )

#Create Plots with Matching Y-Axis 
importance_long_char$Taxa_char <- factor(importance_long_char$Taxa_char,
                                         levels = taxa_order)
clr_long_top_char$Taxa_char <- factor(clr_long_top_char$Taxa_char,
                                      levels = taxa_order)



cat("Testing y-axis consistency:\n")
cat("Number of unique taxa in importance data:", nlevels(importance_long_char$Taxa_char), "\n")
cat("Number of unique taxa in CLR data:", nlevels(clr_long_top_char$Taxa_char), "\n")
cat("Number of unique taxa in bg_stripes:", nlevels(bg_stripes$Taxa_char), "\n")

cat("\nTaxa order (should be identical):\n")
print(levels(importance_long_char$Taxa_char))
cat("\n")

# ---- Plot 1: Variable importance ----
custom_plot1 <- ggplot() +
  # Background stripes using geom_tile
  geom_tile(
    data = bg_stripes %>% filter(stripe == TRUE),
    aes(y = Taxa_char, x = 0, height = 0.9, width = Inf),
    fill = "grey90", alpha = 0.6
  ) +
  geom_col(
    data = importance_long_char,
    aes(x = Importance, y = Taxa_char, fill = Diet),
    position = position_dodge(0.8),
    width = 0.7
  ) +
  scale_fill_manual(
    values = c(
      "Carnivore" = "#ff5e2b",
      "Non Ruminant" = "#800080",
      "Ruminant" = "#2E8B57"
    )
  ) +
  labs(x = "Mean Decrease Accuracy", y = "Taxonomic Family") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(face = "italic", size = 10),
    axis.title = element_text(face = "bold"),
    legend.position = "none"
  )

# ---- Plot 2: CLR abundance ----
custom_plot2 <- ggplot() +
  # Background stripes using geom_tile
  geom_tile(
    data = bg_stripes %>% filter(stripe == TRUE),
    aes(y = Taxa_char, x = 0, height = 0.9, width = Inf),
    fill = "grey90", alpha = 0.6
  ) +
  geom_boxplot(
    data = clr_long_top_char,
    aes(x = CLR, y = Taxa_char, fill = Diet_type),
    outlier.size = 0.8,
    width = 0.7
  ) +
  scale_fill_manual(
    values = c(
      "Carnivore" = "#ff5e2b",
      "Non Ruminant" = "#800080",
      "Ruminant" = "#2E8B57"
    )
  ) +
  labs(x = "CLR Abundance", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title = element_text(face = "bold"),
    legend.position = "none"
  )

# Test individual plots
cat("Testing plot 1...\n")
print(custom_plot1)

cat("Testing plot 2...\n")
print(custom_plot2)

dummy_legend_data <- data.frame(
  Diet = c("Carnivore", "Non Ruminant", "Ruminant"),
  Importance = c(1, 1, 1),
  Taxa_char = factor("Example", levels = levels(importance_long_char$Taxa_char))
)

legend_plot <- ggplot(importance_long_char,
                      aes(x = Importance, y = Taxa_char, fill = Diet)) +
  geom_col(position = position_dodge(0.8)) +
  scale_fill_manual(
    values = c(
      "Carnivore" = "#ff5e2b",
      "Non Ruminant" = "#800080", 
      "Ruminant" = "#2E8B57"
    ),
    name = "Diet Type",
    # Explicitly define all levels to ensure they appear in legend
    breaks = c("Carnivore", "Non Ruminant", "Ruminant"),
    labels = c("Carnivore", "Non Ruminant", "Ruminant")
  ) +
  theme(legend.position = "bottom")

plot_legend <- cowplot::get_legend(legend_plot)

# Combine plots with legend
final_combined_plot <- (custom_plot1 + custom_plot2) / 
  plot_legend +
  plot_layout(heights = c(10, 1))

# Print final plot
print(final_combined_plot)

# Save final combined plot
ggsave("variable_importance_combined_final_with_strips.png",
       plot = final_combined_plot,
       width = 16, height = 10, dpi = 300)
