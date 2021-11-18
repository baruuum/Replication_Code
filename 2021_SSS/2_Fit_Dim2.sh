#!/bin/bash
#
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=24
#SBATCH --time=18:00:00
#SBATCH --mem=64GB
#SBATCH --job-name=dim2
#SBATCH --mail-type=ALL
#SBATCH --mail-user=b.park@cornell.edu

module purge
module load r/gcc/4.1.0

cd /scratch/bp1094/space/
Rscript --vanilla --verbose 2_Fit_Dim2.R > logs/2_Fit_Dim2.log 2>&1
