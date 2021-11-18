
## ------------------------------------------------------------------
## Basic Setup
## ------------------------------------------------------------------

### Installing and loading pkgs

message("\n\n----------- Installing packages and cmdstan...\n\n")
libs = c(
    "dplyr", 
    "data.table", 
    "survey", 
    "readstata13",
    "rstan",
    "bayesplot",
    "here",
    "scales",
    "ggplot2",
    "plotly",
    "reshape2",
    "loo",
    "mclust",
    "MASS"
)

# check packages
libs_installed = sapply(libs, requireNamespace, quietly = TRUE)

# install if not present
if (!all(libs_installed)) {
    
    not_installed = names(libs_installed)[!libs_installed]
    for (l in not_installed) 
        install.packages(l, repos = "https://cran.rstudio.com")
    
}

# Setting Paths
library("here")

# base directory
mcode_path = here("stan_code")
rawdata_path = here("rawdata")
data_path = here("data")
res_path = here("results")
output_path = here("output")
cmdstan_path = here("cmdstan")

# create necessary directories
for (pp in c(mcode_path, data_path, res_path, output_path, cmdstan_path)) {
    
    if(!dir.exists(pp)) {
        
        dir.create(pp)
        
    }
    
}

# install cmdstanr if not present
if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    
    install.packages(
        "cmdstanr", 
        repos = c("https://mc-stan.org/r-packages/", getOption("repos"))
    )
    
    # install cmdstan
    cmdstanr::install_cmdstan(dir = cmdstan_path, cores = 4L)
    
}

## ------------------------------------------------------------------
## Reading in GSS and calculate Pop Estimates
## ------------------------------------------------------------------

message("\n\n----------- Running 1-1_Read_GSS_&_Pop_Est.R...\n\n")

# input: 
#   /rawdata/GSS7214_R3.DTA
# output : 
#   /data/gss_06_valid.csv (2006 GSS)
#   /data/gss_weights.csv  (survey weights)
#   /data/gss_pop_est.csv  (population prop. estimates)
source(here("scripts", "1-1_Read_GSS_&_Pop_Est.R"))

message("\n\n----------- Running 1-2_Data_Setup.R...\n\n")

# generate datasets for analysis
# input : 
#   /data/gss_06_valid.csv
# output: 
#   /data/acq_dat.rds
source(here("scripts", "1-2_Data_Setup.R"))   

message("\n\n----------- Running 1-3_Predictor_Setup.R...\n\n")

# generate dataset of predictors
# input:
#   /data/gss_06_valid.csv
# output: 
#   /data/recoded_vars.rds (list of org and derived vars)
#   /data/predictors.rds (dataset of predictors to be used)
source(here("scripts", "1-3_Predictor_Setup.R"))

message("\n\n----------- Running 1-4_Pop_Ests.R...\n\n")
# generate population % estimates
# output: 
#   data/pop_est.csv
source(here("scripts", "1-4_Pop_Ests.R"))



## ------------------------------------------------------------------
## Writing Stan models
## ------------------------------------------------------------------

message("\n\n----------- Running 2-1_Model_Codes.R...\n\n")
## write stan files
# output:
#   /stan_code/social_space.stan
#   /stan_code/randmix.stan
#   /stan_code/overdispersed.stan
source(here("scripts", "2-1_Model_Codes.R"))



## ------------------------------------------------------------------
## Print Session Info.
## ------------------------------------------------------------------

cat("\n\n---- Session Info ----\n")
print(sessionInfo())


### END OF CODE ###