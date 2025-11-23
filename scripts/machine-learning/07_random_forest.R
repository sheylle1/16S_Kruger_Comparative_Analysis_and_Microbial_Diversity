# Random Forest Classification Analysis

library(tidyverse)
library(caret)
library(patchwork)

# Function to generate confusion matrix plot
make_confusion_plot <- function(data, remove_outliers = TRUE, plot_title = "") {
  
  if (remove_outliers) {
    outliers <- c("K058683-R", "K058706-R", "K058789-R", "K058689-R", "K058814-R", "K058785-R")
    data <- data[!(data$index %in% outliers), ]
  }
  
  # Prepare metadata
  metadata <- data.frame(
    SampleID = data$index,
    Diet_type = factor(make.names(data$Diet_type)),
    stringsAsFactors = FALSE
  )
  
  # Extract abundance data
  abundance_table <- data[, grep("d__Bacteria", colnames(data))]
  rownames(abundance_table) <- metadata$SampleID
  
  # Extract family names
  extract_family <- function(tax_string) {
    if (grepl("g__", tax_string)) {
      gsub(".*;g__([^;]+).*", "\\1", tax_string)
    } else if (grepl("f__", tax_string)) {
      gsub(".*;f__([^;]+).*", "\\1", tax_string)
    } else {
      NA
    }
  }
  
  family_names <- sapply(colnames(abundance_table), extract_family)
  abundance_table_family <- rowsum(t(abundance_table), group = family_names, na.rm = TRUE)
  abundance_table_family <- t(abundance_table_family)
  abundance_table_family <- abundance_table_family[, !is.na(colnames(abundance_table_family))]
  
  # CLR transformation
  clr_transform <- function(x) {
    x <- x + 1
    log(x / exp(mean(log(x))))
  }
  
  abundance_table_clr <- t(apply(abundance_table_family, 1, clr_transform))
  
  # Prepare data for random forest
  rf_data <- cbind(Diet_type = metadata$Diet_type, as.data.frame(abundance_table_clr))
  rf_data$Diet_type <- as.factor(rf_data$Diet_type)
  
  # Train random forest model
  set.seed(123)
  rf_model <- train(
    Diet_type ~ ., data = rf_data,
    method = "rf",
    trControl = trainControl(method = "cv", number = 5, classProbs = TRUE, savePredictions = "final"),
    ntree = 500, importance = TRUE, metric = "Accuracy"
  )
  
  # Get predictions
  cv_predictions <- rf_model$pred
  cv_predictions$obs <- factor(cv_predictions$obs, levels = levels(rf_data$Diet_type))
  cv_predictions$pred <- factor(cv_predictions$pred, levels = levels(rf_data$Diet_type))
  
  # Create confusion matrix
  conf_matrix_cv <- confusionMatrix(cv_predictions$pred, cv_predictions$obs)
  cm_df <- as.data.frame(conf_matrix_cv$table)
  
  # Recode for nice labels
  cm_df <- cm_df %>%
    mutate(
      Reference = recode(Reference, "Non.Ruminant" = "Non Ruminants", "Ruminant" = "Ruminants"),
      Prediction = recode(Prediction, "Non.Ruminant" = "Non Ruminants", "Ruminant" = "Ruminants")
    )
  
  # Create plot
  ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_tile(color = "white", linewidth = 0.6) +
    geom_text(aes(label = Freq),
              color = ifelse(cm_df$Freq > max(cm_df$Freq) * 0.5, "black", "white"),
              size = 5, fontface = "bold") +
    scale_fill_viridis_c(name = "Count") +
    labs(x = "Expected Diet", y = "Predicted Diet", title = plot_title) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
      axis.text.y = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      legend.title = element_text(face = "bold")
    )
}

# Load data
data <- read.csv("D:/CERI/Carla_16S/Manuscript_analysis/data/raw/level-6.csv", check.names = FALSE)

# Remove additional outliers
additional_outliers <- c("K058700-R", "K058796-R", "K058824-R", "K058810-R", "K058694-R")
data <- data[!(data$index %in% additional_outliers), ]

# Create plots
plot_all <- make_confusion_plot(data, remove_outliers = FALSE, plot_title = "A")
plot_no_outliers <- make_confusion_plot(data, remove_outliers = TRUE, plot_title = "B")

# Combine plots
final_plot <- plot_all + plot_no_outliers + plot_layout(ncol = 2)

final_plot

for (ext in c("png", "pdf", "tiff")) {
  ggsave(
    filename = paste0("Figure_S2.", ext),
    plot = final_plot,
    width = 16,
    height = 6,
    dpi = 600
  )
}
# Save combined plot
ggsave("output/figures/Supplementary_confusion_matrix.png",
       plot = final_plot,
       width = 16, height = 6, units = "in", dpi = 600)