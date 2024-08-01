#!/bin/bash

#SBATCH --job-name=kpm_data
#SBATCH --output=kpm_data.log
#SBATCH --error=kpm_data.error
#SBATCH --mail-user=messierkp@nih.gov
#SBATCH --mail-type=END,FAIL
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=36g
#SBATCH --partition=highmem

Rscript data-raw/DATASET.R