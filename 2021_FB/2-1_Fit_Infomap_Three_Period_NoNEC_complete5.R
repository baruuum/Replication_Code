###############################################################################
## 
## Fitting algorithm to three-period data
##
## Creator     : Barum Park
## Last Update : 02/21/2020
##
## Notes
##
## 1) This code implements the source code provided from www.mapequation.org
##    directly. THE CODE WILL NOT RUN ON A WINDOWS PLATFORM. For details of
##    implementing the algorithm, consult the webpage cited above or the 
##    documentation in the main.R file 
##
###############################################################################

## Basic Configurations -------------------------------------------------------

# load libraries
library(here)
library(igraph)
library(data.table)

# check 
if (!exists("p_vec"))
    stop("object p_vec not found")


## Running Models and Save Results---------------------------------------------

message("\n\n\n***** Fitting map equation to three-period data *****\n")

for (period in p_vec) {
    
    message(paste0("\nFitting algorithm to period ",
                   gen_p_labs(period, full = TRUE),
                   " ..."))
    
    # dir_path
    dir_path = here("output", "data", "three_period", period)
    
    # create directory if not present already
    if (!dir.exists(dir_path)) {
        
        message(
            paste0("Creating ", 
                   period, 
                   " subdirectory into output/data/three_period/ ...")
        )
        
        dir.create(dir_path, recursive = T)
        
    }
    
    # loading data
    message("Loading data ...")
    dat = fread(
        here(
            "data", 
            paste0("dat_", period, "_NoNEC_weighted_complete5.csv")
        )
    )
    
    # fit algorithm
    message("Fitting algorithm ...")
    res = fit_infomap(
        dat = dat[occ1 != occ2],
        verbose = 0L
    )
    
    message("Saving modules ...")
    saveRDS(
        res, 
        paste0(dir_path, 
               "/modules_", 
               period, 
               "_NoNEC_weighted_complete5.rds")
    )
    
}

# remove objects
rm(dir_path, res, dat, period)

message("\nDone!")

### END OF CODE ###
