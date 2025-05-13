#!/bin/bash

## Replication code for "Homogeneity and Clustering in Worker Flows"
##
## Creator: Barum Park
## Last run: 2025.01.16

# Enter paths to the project directory, conda, and snakemake here
ROOT_DIR=
CONDA_DIR=
SNAKEMAKE_ENV=

# specify no of cores to use in run
THREADS=63

# go to project directory
cd $ROOT_DIR

# initialize renv environment and install packages
echo "Initializing renv environment ..."

R -e 'if (!require("renv")) install.packages("renv", repos = "cran.rstudio.com")' -q
R -e 'renv::init(bare = TRUE); renv::settings[["use.cache"]](FALSE)' -q

# setting up packages
echo "Moving sbmob package into renv/local ..."
mkdir -p renv/local
cp pkgs/sbmob_0.0.1.4.tar.gz renv/local/sbmob_0.0.1.4.tar.gz

echo "Installing necessary R packages ..."
Rscript --vanilla ./scripts/setup.R

echo "Done on $(date)"


# start replication run
source $CONDA_DIR/etc/profile.d/conda.sh
echo "Running conda version: $(conda --version)"
conda activate $SNAKEMAKE_ENV
echo "Running snakemake version: $(snakemake --version)"

echo "Start running replication code ..."
echo "Run on $(date)"

echo "Running replication ..."

snakemake -s run.smk -j $THREADS --scheduler greedy --nolock > snakemake.log 2>&1

echo "Compressing Snakemake log file ..."

tar -zcf snakemake.log.tar.gz snakemake.log

echo "Done on $(date)"

### EOF ###