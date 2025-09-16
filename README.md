# Manuscript Data Analysis

This repository contains scripts for analyzing and visualizing data for the manuscript.

## Directory Structure

- `data/`: Input data files
- `output/figures/`: Generated visualizations
- `01_rarefaction_curves.R`: Rarefaction curve analysis
- `02_alpha_diversity.R`: Alpha diversity analysis
- `03_relative_abundance_family.R`: Family-level relative abundance
- `04_relative_abundance_speciefamily.R`: Family-level relative abundance on a specie levevl
- `05_relative_abundance_speciegenus.R`: Genus-level relative abundance on a specie level
- `06_random_forest.R`: Random forest classification
- `07_variable_importance.R`: Variable importance analysis

## Data Requirements

Place the following files in the `data/` directory:
- `observed_features.csv`
- `Entropydata.xlsx`
- `level-6.csv`

## Running the Analysis

1. Install required packages: `install.packages(c("tidyverse", "ggpubr", "caret", "viridis", "patchwork", "RColorBrewer"))`
2. Run scripts
3. Output will be saved to the `output/` directory

## Contact

For questions about this analysis, please contact 24791849@sun.ac.za.