#!/bin/bash
#
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=24
#SBATCH --time=18:00:00
#SBATCH --mem=64GB
#SBATCH --job-name=dim3
#SBATCH --mail-type=ALL
#SBATCH --mail-user=b.park@cornell.edu

module purge
module load r/gcc/4.1.0

cd /scratch/bp1094/space/
Rscript --vanilla --verbose 3_Fit_Dim3.R > logs/3_Fit_Dim3.log 2>&1
