# Kruger Wild Mammal Functional Analysis Visualization
# Load required libraries

library(ggplot2)
library(dplyr)
library(reshape2)
library(tibble)

# Function to process PICRUSt2 pathway output for Kruger data
process_kruger_pathways <- function(pathway_file) {
  
  # Check if file exists
  if (!file.exists(pathway_file)) {
    stop(paste("File not found:", pathway_file))
  }
  
  cat("Reading PICRUSt2 pathway data...\n")
  
  # Read PICRUSt2 pathway abundance file
  pathways <- read.table(pathway_file, header = TRUE, sep = "\t", 
                         row.names = 1, check.names = FALSE, quote = "")
  
  cat("Found", nrow(pathways), "pathways across", ncol(pathways), "samples\n")
  
  # Map MetaCyc/KEGG pathways to major functional categories
  # Enhanced mapping for mammalian gut microbiome
  pathway_to_category <- data.frame(
    pathway = rownames(pathways),
    category = case_when(
      # Metabolism (most abundant in gut microbiomes)
      grepl("biosynthesis|degradation|metabolism|glycolysis|TCA|pentose|fatty.acid|amino.acid|carbohydrate|lipid|energy|fermentation", 
            rownames(pathways), ignore.case = TRUE) ~ "Metabolism",
      grepl("PWY|ARGSYNBSUB|TRPSYN|LEUSYN|VALSYN|ILVSYN|glucose|lactose|starch|cellulose", 
            rownames(pathways), ignore.case = TRUE) ~ "Metabolism",
      grepl("^\\d+\\.|butyrate|propanoate|acetate|SCFA|short.chain", 
            rownames(pathways), ignore.case = TRUE) ~ "Metabolism",
      
      # Genetic Information Processing
      grepl("ribosome|translation|transcription|replication|repair|recombination|RNA|DNA", 
            rownames(pathways), ignore.case = TRUE) ~ "Genetic Information Processing",
      grepl("RNA.polymerase|DNA.repair|homologous.recombination|protein.folding", 
            rownames(pathways), ignore.case = TRUE) ~ "Genetic Information Processing",
      
      # Environmental Information Processing  
      grepl("transport|ABC.transport|PTS|two.component|signal|membrane|ion|nutrient", 
            rownames(pathways), ignore.case = TRUE) ~ "Environmental Information Processing",
      grepl("phosphotransferase|permease|transporter|channel|pump", 
            rownames(pathways), ignore.case = TRUE) ~ "Environmental Information Processing",
      
      # Cellular Processes
      grepl("cell.cycle|cell.wall|peptidoglycan|flagellar|chemotaxis|sporulation|motility|adhesion", 
            rownames(pathways), ignore.case = TRUE) ~ "Cellular Processes",
      grepl("biofilm|quorum.sensing|cell.division|cell.envelope", 
            rownames(pathways), ignore.case = TRUE) ~ "Cellular Processes",
      
      # Human/Animal Diseases (relevant for mammalian hosts)
      grepl("pathogen|virulence|antibiotic|resistance|toxin|disease|infection", 
            rownames(pathways), ignore.case = TRUE) ~ "Human Diseases",
      
      # Organismal Systems (host-related)
      grepl("immune|nervous|endocrine|digestive|circulatory|vitamin|cofactor", 
            rownames(pathways), ignore.case = TRUE) ~ "Organismal Systems",
      
      # Unclassified/Others
      grepl("hypothetical|unknown|uncharacterized|predicted", 
            rownames(pathways), ignore.case = TRUE) ~ "Unclassified",
      
      # Default to Others for everything else
      TRUE ~ "Others"
    ),
    stringsAsFactors = FALSE
  )
  
  # Show category distribution
  cat("\nPathway category distribution:\n")
  print(table(pathway_to_category$category))
  
  # Aggregate by category
  category_abundance <- pathways %>%
    rownames_to_column("pathway") %>%
    left_join(pathway_to_category, by = "pathway") %>%
    group_by(category) %>%
    summarise(across(where(is.numeric), sum), .groups = 'drop') %>%
    column_to_rownames("category")
  
  # Convert to relative abundance
  category_rel <- sweep(category_abundance, 2, colSums(category_abundance), FUN = "/")
  
  # Convert to long format for plotting
  plot_data <- category_rel %>%
    rownames_to_column("Category") %>%
    reshape2::melt(id.vars = "Category", 
                   variable.name = "Sample", 
                   value.name = "RelativeAbundance")
  
  # Clean up sample names if needed
  plot_data$Sample <- gsub("\\.", "_", plot_data$Sample)
  
  cat("Data processing complete!\n")
  return(plot_data)
}

# Enhanced plotting function: group samples into 3 bars (one per diet_type)
create_kruger_functional_plot_diet <- function(data, diet_file) {
  # Read diet CSV
  if (!file.exists(diet_file)) {
    stop(paste("Diet CSV file not found:", diet_file))
  }
  
  cat("Reading diet data...\n")
  diets <- readr::read_csv(diet_file, show_col_types = FALSE)
  colnames(diets) <- c("Sample", "diet_type")
  
  # Join diet data with plot data
  data <- left_join(data, diets, by = "Sample")
  
  if (any(is.na(data$diet_type))) {
    warning("Some samples in the pathway data do not have matching diet information.")
  }
  
  # Aggregate relative abundance by diet_type and Category
  diet_summary <- data %>%
    group_by(diet_type, Category) %>%
    summarise(RelativeAbundance = mean(RelativeAbundance, na.rm = TRUE), .groups = "drop")
  
  # Define colors
  colors <- c(
    "Metabolism" = "#d62728",
    "Genetic Information Processing" = "#1f77b4",
    "Environmental Information Processing" = "#2ca02c",
    "Cellular Processes" = "#ff7f0e",
    "Unclassified" = "#9467bd",
    "Human Diseases" = "#8c564b",
    "Organismal Systems" = "#e377c2",
    "Others" = "#17becf"
  )
  
  # Order categories
  diet_summary$Category <- factor(diet_summary$Category, levels = names(colors))
  
  # Order diet_type on x-axis (optional: custom order)
  diet_summary$diet_type <- factor(diet_summary$diet_type, levels = c("Carnivore", "Non-Ruminant", "Ruminant"))
  
  # Create plot
  p <- ggplot(diet_summary, aes(x = diet_type, y = RelativeAbundance, fill = Category)) +
    geom_bar(stat = "identity", width = 0.6, color = "white") +
    scale_fill_manual(values = colors) +
    scale_y_continuous(limits = c(0, 1),
                       breaks = seq(0, 1, 0.25),
                       labels = c("0", "0.25", "0.5", "0.75", "1")) +
    geom_hline(yintercept = 0.75, linetype = "dotted", color = "gray50", alpha = 0.7) +
    labs(
      x = "Diet Type",
      y = "Relative Abundance",
      fill = NULL,
      title = "Functional Prediction of Kruger Wild Mammal Gut Microbiomes",
      subtitle = "Averaged by diet type"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 10),
      axis.title.y = element_text(size = 12, margin = ggplot2::margin(r = 10)),
      plot.title    = element_text(size = 14, margin = ggplot2::margin(b = 5)),
      plot.subtitle = element_text(size = 11, margin = ggplot2::margin(b = 5)),
      legend.position = "right",
      legend.text = element_text(size = 9),
      legend.key.size = unit(0.8, "cm"),
    )
  
  return(p)
}

# Updated main execution function
main_analysis <- function(
    pathway_file = "D:/CERI/Carla_16S/Manuscript_analysis/data/raw/path_abun_unstrat.tsv",
    diet_file = "D:/CERI/Carla_16S/Manuscript_analysis/data/raw/diets.csv"
) {
  
  cat("Starting Kruger mammal functional analysis (by diet)...\n")
  
  # Process the data
  plot_data <- process_kruger_pathways(pathway_file)
  
  # Create the plot with 3 bars for diet types
  functional_plot <- create_kruger_functional_plot_diet(plot_data, diet_file)
  
  # Display plot
  print(functional_plot)
  
  # Save plot
  ggsave("kruger_functional_abundance_by_diet_3bars.png", functional_plot, 
         width = 10, height = 7, dpi = 300)
  ggsave("kruger_functional_abundance_by_diet_3bars.pdf", functional_plot, 
         width = 10, height = 7)
  
  cat("\nPlot saved as 'kruger_functional_abundance_by_diet_3bars.png' and '.pdf'\n")
  
  return(list(
    plot_data = plot_data,
    plot = functional_plot
  ))
}

results <- main_analysis()
