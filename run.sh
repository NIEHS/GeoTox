#!/bin/bash

#SBATCH --job-name=paper_demo
#SBATCH --output=paper_demo.log
#SBATCH --error=paper_demo.error
#SBATCH --mail-user=messierkp@nih.gov
#SBATCH --mail-type=END,FAIL
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=36g
#SBATCH --partition=highmem

Rscript -e "rmarkdown::render('vignettes/paper-demo-01.Rmd')"