# Downloading Sequencing Data from SRA

All sequencing data for this study are publicly available under:

* **BioProject:** PRJNA1248980
* **SRA Accessions (Run IDs):** 
  *(Full list provided in `accessions.txt` in this repository.)*

## How to Download the Data

1. **Install the SRA Toolkit:**
   [https://github.com/ncbi/sra-tools/wiki/Downloads](https://github.com/ncbi/sra-tools/wiki/Downloads)

2. **Download the SRA files (recommended):**

   ```bash
   prefetch SRRXXXXXXX
   ```

3. **Convert to FASTQ:**

   ```bash
   fasterq-dump SRRXXXXXXX --split-files --threads 8 --outdir fastq/
   ```

4. **Batch download from the provided list:**

   ```bash
   xargs prefetch < accessions.txt

   for acc in $(cat accessions.txt); do
       fasterq-dump $acc --split-files --threads 8 --outdir fastq/
   done
   ```

For more details on SRA Toolkit usage, see:
[https://github.com/ncbi/sra-tools](https://github.com/ncbi/sra-tools)

---
