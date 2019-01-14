### Was There a Culture War? Polititcal Polarization and Secular Trends in U.S. Public Opinion

Last update on 12/21/2018 by Barum Park

- Notes on Data and Directories:
    1) **To replicate the analysis**, you need three directories. Let `wd.base` be the directory into which the files have been downloaded. You have to create the directories `~/wd.base/code`, `~/wd.base/raw_data`, and `~/wd.base/codebook/`. 
    2) After creating these directories, place **all `R` codes** into `~/wd.base/code/`.
    2) **Raw data Files** (ANES, GSS, and the random seeds for `STAN`) should be placed into `~wd.base/raw_data`. ANES data can be downloaded from "http://www.electionstudies.org". GSS data can be downloaded from http://gss.norc.org/.
        - ANES 1948-2012 cumulative data file should be saved under the name `anes_timeseries_cdf_rawdata.txt`
        - ANES 2016 data file should be saved under the name `anes_timeseries_2016_rawdata.txt`
        - GSS 1972-2016 cumulative data file should be saved under the name `36797-0001-Data.tsv`
        - `stan.seeds.csv` contains the random seeds that were used in the `STAN` models and should be placed here as well.
    4) The **codebook** has to be placed into the `~/wd.base/codebook` directory, as the `variable_summary_final.xlsx` file is used in the `1-1.Prep_Data_NES.R` and `1-2.Prep_Data_GSS.R` code.
    5) After steps 1) to 4) you have to run the `_MAINCODE.R` to replicate the analysis.
    
- Notes on Running `_MAINCODE.R`:
    1) Before running the code you have to replace `"working directory here!"` string with the path to the `wd.base` directory mentioned above.
    1) The replication code is written in a way that running `_MAINCODE.R` will generate **all** results presented in the paper. However, due to `R`'s inefficient memory handling, replicating the results in a single run might not be possible, if the machine on which the code is executed has not sufficient memory (even though large objects are repeatedly cleared from the environment in the code). If this is the case, breaking up the loops and running each part of the code separately will probably solve the problem.
    2) **Running the whole replication code is quite time consuming**: running `_MAINCODE.R` from begining to end takes approximately 3 and a half hours on a modern computer. 
    3) At the start of `_MAINCODE.R`, you have to specify the number of CPUs to use in running the models in the `n.cores` object. If you are not sure how many cores your computer has, using `n.cores <- parallel::detectCores()` might be an option (although not recommended). In the distributed version of the code, `n.cores` is set to 6. 
    4) Notice that the code involving the `rstan::sampling` command is written such that **each chain will sample 1,000 posterior draws**. This implies that if you specify `n.cores <- 3`, you'll have 3,000 posterior samples, instead of the 6,000 that were analyzed in the paper. The numerical results of the analysis might slightly differ, accordingly.
    5) **Exact replication of the results**: Even if the same random seeds are used in estimating the Bayesian models, exact numerical results might slightly differ from the results presented in the paper if the operating system or the interface differs. All models that are presented in the paper were run in `R` version `3.4.2` using `rstan` version `2.17.3` on a `Intel(R) Xeon E5-2690v2 3.0GHz` machine with `x86_64-centos-linux-gnu (64-bit)` operating system. The random seeds are distributed together with the `R` code.  

- Other Notes:
    1) Due to the sheer volume of output, traceplots are not reproduced in the replication package. The user might access the produced `stanfit` objects directly.
    2) The `replication.log` file shows the log of the last run of the full `_MAINCODE.R` code. The `sessionInfo()` of the run is found at the end of the file, which might be helpful for exact replications. The last date on which the full `_MAINCODE.R` code was run and numerical results checked is 12/03/2018.
