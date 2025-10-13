# Variable Importance Analysis for Microbiome Data
# This script performs random forest classification and identifies important taxa

# -------------------- 0. Load libraries --------------------
library(tidyverse)
library(caret)
library(randomForest)
library(viridis)

# -------------------- 1. Load and preprocess data --------------------
data <- read.csv("data/level-6.csv", check.names = FALSE)

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

# -------------------- 2. Random Forest Classification --------------------
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

# -------------------- 3. Diet-specific Variable Importance --------------------
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

# -------------------- 4. Identify Diet with Highest Abundance --------------------
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

# -------------------- 5. Clean Labels and Filter Taxa --------------------
importance_long <- importance_long %>%
  filter(!grepl("^d_Bacteria;p_", Taxa)) %>%
  mutate(
    Diet = recode(Diet,
                  "Non.Ruminant" = "Non Ruminant",
                  "Ruminant" = "Ruminant"),
    Winner = recode(Winner,
                    "Non.Ruminant" = "Non Ruminant",
                    "Ruminant" = "Ruminant")
  )

# -------------------- 6. Plot Top 20 Important Taxa per Diet --------------------
top_taxa <- importance_long %>%
  group_by(Diet) %>%
  slice_max(order_by = Importance, n = 20, with_ties = FALSE) %>%
  ungroup()

# Create plot
importance_plot <- ggplot(
  top_taxa,
  aes(x = tidytext::reorder_within(Taxa, Importance, Diet),
      y = Importance,
      fill = Winner)
) +
  geom_col(width = 0.7, show.legend = TRUE) +
  facet_wrap(~ Diet, scales = "free_y") +
  coord_flip() +
  tidytext::scale_x_reordered() +
  scale_fill_manual(
    values = c(
      "Carnivore" = "#ff5e2b",
      "Non Ruminant" = "#800080",
      "Ruminant" = "#2E8B57"
    )
  ) +
  labs(
    x = "Taxonomic Family",
    y = "Mean Decrease Accuracy",
    fill = "Highest CLR Abundance"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 9, face = "italic"),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(size = 13, face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

# Save plot
ggsave("output/figures/Variable_importance.png", 
       plot = importance_plot, 
       width = 12, height = 10, dpi = 300)

