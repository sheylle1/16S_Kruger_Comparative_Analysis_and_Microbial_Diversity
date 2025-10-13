# Comparative Analysis of Gut Microbiota Composition and Diversity in Wild Mammals from the Kruger National Park, South Africa

Description of the data and file structure This data and code repository is currently provided to facilitate the peer-review process for a submitted manuscript. Materials may change upon final publication.

Results related to Figures 1 (Beta diversity analyses) were generated from QIIME2 core diversity analyses.

Input files for these analyses (qiime visualisations, emperor-settings.json) are stored in the folder data/processed/qiime2/beta-diversity.

Scripts used to generate Random Forest(RF) classification and confusion matrices are located in scripts/machine-learning.

The qiime workflow and pipeline can be found in scripts/qiime2. 

Results for Figure 2 (Alpha diversity analyses) were generated in R using output from Qiime2 pipeline. 

R scripts for plotting diversity metrics (e.g., Shannon, Faith’s PD, UniFrac) are found in scripts/R/plot_diversity.R, and the resulting figures are saved in output/figures/alpha-diversity and docs/main/Manuscript_figures.pdf.

Results related to Figure 3 and 4(Taxonomic composition by Diet Group) were generated from QIIME2 taxonomic classification followed by data aggregation in R.

Taxonomy output from qiime 2 are stored in data/processed/taxonomic and inputs for R are stored in data/raw/level-6.csv .

The top 20 family-level and top 30 genus-level composition plots were produced using scripts/R , with results in output/figures/tax_by_group. Final figures saved in docs/main/Manuscript_figures.pdf

Results related to Figure 5 and 6(Taxonomic composition by Species) were generated from QIIME2 taxonomic classification followed by data aggregation in R.

The top 20 family-level and top 30 genus-level composition plots were produced using scripts/R , with results in output/figures/tax_by_species. Final figures saved in docs/main/Manuscript_figures.pdf

Results related to Figure 7 and 8(Variable Importance) were generated  in R. Scripts are stored in scripts/machine-learning and resulting figures are in output/figures/machine-learning

Raw sequencing data are hosted externally due to file size constraints. Accession numbers and download instructions will be provided in data/README.md.

Metadata containing host species, sample types, and collection information is stored in data/metadata/metadata.xlsx, and is used throughout the analyses.

Intermediate outputs, including demultiplexed reads, filtered feature tables, and phylogenetic trees, are stored in data/intermediate/. These allow reviewers to reproduce downstream analyses without repeating the full raw data processing.

Additional analyses, such as Functional analyses, Association analyses, diease detection, and further importance analysis are included in scripts/extra with figure results in output/figures/extra. 

All qiime 2 analyses were performed within the conda environment v24.10.amp. And all R analyses done in version 4.3.3. 

