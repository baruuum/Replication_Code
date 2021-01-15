#!/bin/bash
#
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=20
#SBATCH --cpus-per-task=1
#SBATCH --time=32:00:00
#SBATCH --mem=32GB
#SBATCH --job-name=flows_and_boundaries
#SBATCH --mail-type=ALL
#SBATCH --mail-user=bp1094@nyu.edu

module purge
module load r/gnu/3.5.1
module load gcc/6.3.0

cd /scratch/bp1094/FnB/

echo "Start running replication code ..."
echo "Run on $(date)$"

Rscript --no-save _RUN_REPLICATION.R > replication_log.txt 2>&1

echo "Finished on $(date)"
