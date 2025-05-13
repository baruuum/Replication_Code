# Replication files for "Equivalence and Clustering in Worker Flows"


This is the replication package for the paper:

[Barum Park](https://github.com/baruuum) "Equivalence and Clustering in Worker Flows: Stochastic Blockmodels for the Analysis of Mobility Tables" 

which has been accepted for publication in [*Sociological Methodology*](https://journals.sagepub.com/home/SMX).

## How to replicate the results

All of the analysis were done with the `R (version 4.4.1)` statistical software, where the `renv` package was used for package management. Workflow management is done using the `snakemake (version 8.14.0)` software activated in a isolated `conda (version 24.5.0)` environment. All analyses were conducted on a Linux operating system (`Ubuntu 20.04.6 LTS`). To replicate the results exactly, the OS and software versions need to match with those specified here. 

To run the replication, specify the paths to the project directory, conda, and snakemake environment in lines `9-11` of the `replication.sh` file:
```bash
ROOT_DIR=
CONDA_DIR=
SNAKEMAKE_ENV=
```
Then, specify the path to directories in which the results, logs, etc. should be stored in lines `2-8` of the `config.yaml` file:
```yaml
paths: 
  res_dir:
  sim_dir:
  fit_dir :
  log_dir:
  plot_dir:
  table_dir:
```
Thereafter, running
```bash 
bash replication.sh
``` 
will replicated all results. Notice that by running the file, necessary `R` packages will be downloaded and installed. If some of the packages fail to install, the code will skip those packages instead of stopping, which will likely result in errors downstream.

## Log files

The log files of the last replication run by the author are contained in three different places: 
1. The `logs` directory contains the log files for each `rule` that is executed in the `snakemake` workflow 
2. The `snakemake` output is compressed and stored in the `snakemake.log.tar.gz` file
3. The output of running `replication.sh` is stored in the `replication.log` file

## The `sbmob` package

Included in the replication files is the `sbmob` package, which is used to fit all models presented in the paper. If desired, the package can be installed separately from within `R` with
```r
install.packages("pkgs/sbmob_0.0.1.4.tar.gz", repos = NULL, type = "source")
```
