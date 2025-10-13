#!/bin/bash
# QIIME2 16S Processing Pipeline
# Make sure QIIME2 is activated before running this script
# Example: conda activate qiime2-2024.2

set -e  # Stop on error

echo "=== Step 1: Importing Paired-End Reads ==="
qiime tools import \
  --type 'SampleData[PairedEndSequencesWithQuality]' \
  --input-format PairedEndFastqManifestPhred33V2 \
  --input-path Newmanifest.tsv \
  --output-path demux.qza

echo "=== Step 2: Summarizing Demultiplexed Data ==="
qiime demux summarize \
  --i-data demux.qza \
  --o-visualization demux.qzv

echo "=== Step 3: Denoising with DADA2 ==="
qiime dada2 denoise-paired \
  --i-demultiplexed-seqs demux.qza \
  --p-trunc-len-f 280 \
  --p-trunc-len-r 265 \
  --p-trim-left-f 5 \
  --p-trim-left-r 5 \
  --o-representative-sequences rep-seqs.qza \
  --o-table table.qza \
  --o-denoising-stats denoising-stats.qza

echo "=== Step 4: Summarizing Denoising Stats ==="
qiime metadata tabulate \
  --m-input-file denoising-stats.qza \
  --o-visualization denoising-stats.qzv

echo "=== Step 5: Phylogenetic Tree Construction ==="
qiime phylogeny align-to-tree-mafft-fasttree \
  --i-sequences rep-seqs.qza \
  --o-alignment aligned-rep-seqs.qza \
  --o-masked-alignment masked-aligned-rep-seqs.qza \
  --o-tree unrooted-tree.qza \
  --o-rooted-tree rooted-tree.qza

echo "=== Step 6: Core Diversity Metrics ==="
qiime diversity core-metrics-phylogenetic \
  --i-phylogeny rooted-tree.qza \
  --i-table table.qza \
  --p-sampling-depth 2000 \
  --m-metadata-file metadata.tsv \
  --output-dir core-metrics-results

echo "=== Step 7: Alpha Diversity Significance ==="
qiime diversity alpha-group-significance \
  --i-alpha-diversity core-metrics-results/faith_pd_vector.qza \
  --m-metadata-file metadata.tsv \
  --o-visualization faith-pd-group-significance.qzv

echo "=== Step 8: Beta Diversity Significance ==="
qiime diversity beta-group-significance \
  --i-distance-matrix core-metrics-results/unweighted_unifrac_distance_matrix.qza \
  --m-metadata-file metadata.tsv \
  --m-metadata-column Farming_Type \
  --o-visualization unweighted-unifrac-group-significance.qzv \
  --p-pairwise

echo "=== Step 9: Taxonomy Assignment ==="
qiime feature-classifier classify-sklearn \
  --i-classifier silva-138-99-nb-classifier.qza \
  --i-reads rep-seqs.qza \
  --o-classification taxonomy.qza

echo "=== Step 10: Summarizing Taxonomy ==="
qiime metadata tabulate \
  --m-input-file taxonomy.qza \
  --o-visualization taxonomy.qzv

echo "=== Step 11: Taxonomic Barplots ==="
qiime taxa barplot \
  --i-table table.qza \
  --i-taxonomy taxonomy.qza \
  --m-metadata-file metadata.tsv \
  --o-visualization taxa_barplot.qzv

echo "=== QIIME2 Pipeline Completed Successfully ==="
