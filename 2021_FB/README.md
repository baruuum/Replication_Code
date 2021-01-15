# Replication Files for *Flows and Boundaries* 

Creator : Barum Park

Last Replication (`_RUN_REPLICATION.R`): 02/25/2020  
Last run of additional analyis (`_ADDITIONAL_FIGURES.R`): 07/31/2020

Please direct questions regarding the rawdata to `siwei.cheng@nyu.edu`. For questions regarding the analysis and models/algorithms, contact `b.park@cornell.edu`.

## Instructions

To replicate the results, you have to do the following:

1. Download the code into any directory (we'll call it the `root` directory for convenience). The `root` directory should look exactly the same as the github repository (you can just `clone` it).
2. Create a new directory `root/infomap` and compile the infomap algorithm into the directory by following the instructions given [here](https://www.mapequation.org/code.html#Installation).
3. Create the subdirectory `root/rawdata`. This directory has to contain the following files:
    - `edges_67to15ASEC_dropimp_primeage.dta`: CPS-ASEC samples from year 1975 to 2014 consisting of workers of age between 25 to 55, where imputed occupations are excluded. This dataset should contain the following columns:
        1. `occ1`: the origin of the transition (a character string containing the name of the occupation in the 1990 Census occupation codes)
        2. `occ2`: the destination of the transition (a character string containing the name of the occupation in the 1990 Census occupation codes)
        3. `begin_year`: the beginning year of the transition (one minus the year in which the respondent was interviewed)
        
    - `edges_67to15ASEC_dropimp_primeage_m.dta`: the same dataset as `edges_67to15ASEC_dropimp_primeage.dta` but restricted to men.
    
    - `edges_67to15ASEC_dropimp_primeage_f.dta`: the same dataset as `edges_67to15ASEC_dropimp_primeage.dta` but restricted to women.
    
    - `edges_post03ASEC_OCC2010.csv`: the same dataset as `edges_67to15ASEC_dropimp_primeage.dta` but restricted to years starting in 2003 and using the harmonized 2000 occupational codes instead of the harmonized 1990 version.
    
    - `ASEC_individual_level.dta`: CPS-ASEC samples from year 1989 to 2015 restricted to workers of age between 25 and 55. This dataset has to contain the following columns. 
    
        1. `year`: the interview year
        2. `occ1990`: occupation of respondent coded according to the 1990 Census occupation codes
        3. `log_adj_wageinc`:  personal annual wage and salary income of respondent, converted to 1999 dollars and log-transformed, where imputed incomes are treated as missing values.
        3. `age`: the age of the respondent
        4. `age_sq`: `age` squared
        5. `race_3cat`: race of respondent (white, black, other)
        6. `educ_4cat`: education level of respondent (less than
high school, high school graduate, some college, and college or above)
        7. `sex`: sex of the respondent (men, women)
        
      Except for `occ1990`, `year`, and `age`, the concrete numbers assigned to the categories are irrelevant as the code uses the `factor` function to treat them as categorical variables.
      
    - `occ_class_scheme.dta`: Dataset containing the mapping between 1990 Census occupation codes and the Weeden-Grusky class scheme. This dataset has to contain the following columns:
    
        1. `occ1990`: the occupation code
        2. `occ1990_labels`: the name of the occupation
        3. `macro_adj`: the macro-class to which the occupation belongs
        4. `meso_adj`: the meso-class to which the occupation belongs
        5. `micro_adj`: the micro-class to which the occupation belongs. 
    
    - `occlevel_skills.dta`: occupation-level skill requirements. This dataset must contain the following columns.
    
        1. `occ1990`: the occupation name (label)
        2. the columns `programming`, `verbal`, `quantitative`, `analytic`, `creative`, `computer`, `sciandeng`, `techmisc`, `managerial`, `carework` which each measure a different dimension of the skill requirements. For details on the construction of these measures see Siwei Cheng, Bhumika Chauhan, and Swati Chintala. 2019. "The rise of programming and the stalled gender revolution," *Sociological Science*, 6: 321-351.

4. The code depends on the following R packages, so they will need to be installed together with their dependencies:
```
    cowplot
    data.table, 
    doParallel, 
    doRNG, 
    emojifont, 
    gdata, 
    ggplot2,
    ggraph,
    gridExtra, 
    here, 
    igraph, 
    purrr, 
    Rcpp, 
    RcppArmadillo, 
    readstata13, 
    stringr
```
This can be done by typing 
```
    install.packages("<package-name>", dependencies = TRUE, repose = "<address-of-repository>")
```
into the R console. The replication code will also check whether all necessary packages were installed. If some of the packages cannot be found, you'll receive an error message that asks you to install the necessary packages.

4. After this setup, running 
```
    Rscript --no-save _RUN_REPLICATION.R > replication_log.txt > 2>&1
``` 
will reproduce all presented results in the paper including those presented in the online supplement. You might also simply load the `_RUN_REPLICATION.R` into your `Rgui` or `RStudio` and run it there. _Note of Caution:_ Running the full replication took about a day (approximately 18 hours) on a Intel(R) Xeon E5-2690v2 3.0GHz cluster using 19 CPUs. The bottleneck in the code is, obviously, the bootstrapping step to estimate the 95\% CIs for the modularity trend. Skipping this step should significantly reduce the running time of the replication code.

5. The output of the last replication by Barum Park on 02/24/2020 is contained in the `replication_log.txt`. At the end of the file, you can find the `sessionInfo()` of the session. All analyses used Infomap version 0.18.27.

6. The `_ADDITIONAL_FIGURES.R` script will run additional analyses and create Figure E15 of the paper. This script has to be run after running `_RUN_REPLICATION.R`. As the script adds only a single figure and executing it takes only a couple of seconds, an additional script was constructed, rather than runing the whole replication from scratch. The output of the last replication, including the `sessionInfo()`, can be found in `additional_figures_log.txt`. The last part of the analysis can be reproduced by moving into the `root` directory and executing
```
    Rscript --no-save _ADDITIONAL_FIGURES.R > additional_figures_log.txt 2>&1
``` 

## Troubleshooting

1. Some of the functions rely on the `RcppArmadillo` package. On macOS, it sometimes happens that the installation fails with the error message that `-lgfortran` or `-lquadmath` cannot be found. If so, try to install the necessary command-line tools and the `gfortran` library (notice that the `gfortran` version has to match your MacOS version). Step-by-step instructions can be found [here](https://thecoatlessprofessor.com/programming/cpp/r-compiler-tools-for-rcpp-on-macos/)

2. Some of the results (Table 2 and Table D4) are exported as `LaTeX` code from R. As the `LaTeX` code relies on the `arydshln` package, you'll need to add the following lies to the preamble of your LaTeX document:
```
        \usepackage{arydsln}
``` 

3. Unfortunately, when the infomap algorithm is compiled using the instructions laid out at `mapequation.org`, it will not run on a windows machine. For Windows users, installing Bash on Ubuntu on Windows might be an option. However, as of this date, the desktop version of rstudio is no longer supported on Bash on Ubuntu on Windows. A workaround is to install rstudio-server and using a Chrome/Firefox browser. After installing both base-R and rstudio-server, you can launch rstudio with `sudo rstudio-server start`. Then, open your browser and enter the address `localhost:8787`. You'll find the usual rstudio environment there.

4. If you want to use the `/jobs/run_replication.sh` bash script, you need to modify the path to the `_RUN_REPLICATION.R` file. There is no bash script for `_ADDITIONAL_FIGURES.R` as this script can be run within a couple of seconds.

## Session Information of the Last Replication Run of `_RUN_REPLICATION.R`

```
R version 3.5.1 (2018-07-02)
Platform: x86_64-centos-linux-gnu (64-bit)
Running under: CentOS Linux 7 (Core)

Matrix products: default
BLAS: /share/apps/lapack/3.7.0/gnu/lib64/libblas.so.3.7.0
LAPACK: /share/apps/lapack/3.7.0/gnu/lib64/liblapack.so.3.7.0

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
 [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
 [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
 [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
 [9] LC_ADDRESS=C               LC_TELEPHONE=C            
[11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] parallel  stats     graphics  grDevices utils     datasets  methods  
[8] base     

other attached packages:
 [1] doRNG_1.8.2       rngtools_1.5      doParallel_1.0.14 iterators_1.0.10 
 [5] foreach_1.4.4     emojifont_0.5.3   gridExtra_2.3     ggplot2_3.1.0    
 [9] data.table_1.12.8 igraph_1.2.4.1    here_0.1         

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.1                pillar_1.3.1             
 [3] compiler_3.5.1            plyr_1.8.4               
 [5] tools_3.5.1               readstata13_0.9.2        
 [7] sysfonts_0.8              digest_0.6.18            
 [9] lattice_0.20-35           tibble_2.1.1             
[11] gtable_0.3.0              pkgconfig_2.0.2          
[13] rlang_0.3.4               Matrix_1.2-14            
[15] RcppArmadillo_0.9.400.3.0 proto_1.0.0              
[17] stringr_1.4.0             withr_2.1.2              
[19] dplyr_0.8.0.1             showtextdb_2.0           
[21] gtools_3.8.1              rprojroot_1.3-2          
[23] grid_3.5.1                tidyselect_0.2.5         
[25] glue_1.3.1                R6_2.4.0                 
[27] gdata_2.18.0              reshape2_1.4.3           
[29] purrr_0.3.2               magrittr_1.5             
[31] backports_1.1.2           scales_1.0.0             
[33] codetools_0.2-15          showtext_0.7             
[35] assertthat_0.2.1          colorspace_1.4-1         
[37] labeling_0.3              stringi_1.4.3            
[39] lazyeval_0.2.2            munsell_0.5.0            
[41] crayon_1.3.4             
```
