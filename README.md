# Comparative Analysis of Gut Microbiota Composition and Diversity in Wild Mammals from the Kruger National Park, South Africa

## Overview

This repository contains data, code, and analysis workflows for a comparative study of gut microbiota in wild mammals from Kruger National Park, South Africa. The repository supports reproducible research and facilitates peer review of the submitted manuscript.

**Note:** Materials in this repository may be updated upon final publication.

---

## Repository Structure

```
├── data/
│   ├── raw/                    # Raw input data
│   ├── processed/              # Processed data outputs
│   ├── qiime2_raw/             # Raw inputs for qiime 2
│   ├── intermediate/           # Intermediate analysis outputs
│   └── metadata/               # Sample metadata
├── scripts/
│   ├── qiime2/                 # QIIME2 workflow and pipeline
│   ├── R/                      # R analysis scripts
│   ├── machine-learning/       # Random Forest and ML analyses
│   └── extra/                  # Additional analyses
├── output/
│   ├── extra/                  # Additional analyses
│   └── figures/                # Generated figures
└── docs/
    ├── supplementary/          # Supplementary figures
    └── main/                   # Final manuscript figures

```

---

## Data Description

### Raw Data

Raw sequencing data are hosted externally due to file size constraints. Accession numbers and download instructions are provided in `data/README.md`.

### Metadata

Sample metadata, including host species, sample types, and collection information, is stored in:
- `data/metadata/metadata.xlsx`

This metadata file is used throughout all analyses.

### Intermediate Data

To facilitate reproducibility without reprocessing raw data, intermediate outputs are provided in `data/intermediate/`:
- Demultiplexed reads
- Filtered feature tables
- Phylogenetic trees
- core-metrics-results
- exported metrics

### Final figures

The figures within the manuscript are stored in `doc/main` and supplementary material in `docs/supplementary`

---

## Analysis Workflows and Figure Generation

### Figure 1: Beta Diversity Analyses

**Methods:** Core diversity analyses performed in QIIME2

**Input files:**
- `data/processed/qiime2/beta-diversity/` (QIIME2 visualizations, emperor-settings.json)

**Scripts:**
- `scripts/qiime2/` (QIIME2 workflow and pipeline)

**Output:**
- Beta diversity plots and Emperor visualizations

---

### Figure 2: Alpha Diversity Analyses

**Methods:** Generated in R using QIIME2 output

**Scripts:**
- `scripts/R/plot_diversity.R`

**Metrics analyzed:**
- Shannon diversity
- Faith's Phylogenetic Diversity (PD)
- UniFrac distances

**Output:**
- `output/figures/alpha-diversity/`
- `docs/main/Manuscript_figures.pdf`

---

### Figures 3 & 4: Taxonomic Composition by Diet Group

**Methods:** QIIME2 taxonomic classification with R aggregation and visualization

**Input files:**
- `data/processed/taxonomic/` (QIIME2 taxonomy output)
- `data/raw/level-6.csv`

**Scripts:**
- `scripts/R/` (taxonomy plotting scripts)

**Analyses:**
- Top 20 family-level composition
- Top 30 genus-level composition

**Output:**
- `output/figures/tax_by_group/`
- `docs/main/Manuscript_figures.pdf`

---

### Figures 5 & 6: Taxonomic Composition by Species

**Methods:** QIIME2 taxonomic classification with R aggregation and visualization

**Input files:**
- `data/processed/taxonomic/` (QIIME2 taxonomy output)

**Scripts:**
- `scripts/R/` (taxonomy plotting scripts)

**Analyses:**
- Top 20 family-level composition
- Top 30 genus-level composition

**Output:**
- `output/figures/tax_by_species/`
- `docs/main/Manuscript_figures.pdf`

---

### Figures 7 & 8: Variable Importance (Machine Learning)

**Methods:** Random Forest classification and feature importance analysis in R

**Scripts:**
- `scripts/machine-learning/` (RF classification, confusion matrices, variable importance)

**Output:**
- `output/figures/machine-learning/`
- `docs/main/Manuscript_figures.pdf`

---

### Additional Analyses

Supplementary analyses are included to provide extended insights:

**Types of analyses:**
- Functional analyses
- Association analyses
- Disease detection
- Extended importance analyses
- Dunn-tests

**Scripts:**
- `scripts/extra/`

**Output:**
- `output/figures/extra/`

---

## Software Requirements

### QIIME2

All QIIME2 analyses were performed using:
- **Conda environment:** `v24.10.amp`

### R

All R analyses were performed using:
- **R version:** 4.3.3

### Installation

Detailed installation instructions for required packages and dependencies are provided in the respective script directories.

---

## Reproducibility

To reproduce the analyses:

1. Download raw sequencing data using instructions in `data/README.md`
2. Ensure QIIME2 (v24.10.amp) and R (4.3.3) are installed
3. Run QIIME2 workflows in `scripts/qiime2/`
4. Execute R scripts in `scripts/R/` and `scripts/machine-learning/`
5. Review outputs in `output/figures/` and `docs/main/`

Alternatively, use intermediate data in `data/intermediate/` to skip raw data processing steps.

---

## Citation

If you use this data or code, please cite the associated manuscript (citation details will be added upon publication).

---

## Contact

For questions or issues regarding this repository, please contact the corresponding author listed in the manuscript.

---
