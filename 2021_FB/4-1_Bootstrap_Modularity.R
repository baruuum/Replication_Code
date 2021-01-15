###############################################################################
## 
## Bootstrapping confidence intervals for LinkRank modularity 
##
## Creator     : Barum Park
## Last Update : 02/21/2020
##
###############################################################################


# Basic Configurations --------------------------------------------------------

# load libraries
library(here)
library(igraph)
library(data.table)
library(doParallel)
library(doRNG)

if (!exists("w_size_vec"))
    stop("cannot find w_size_vec")

if (!exists("n_cores"))
    stop("cannot find n_cores")

if (!exists("n_boot"))
    stop("cannot find n_boot")

if (!exists("b_level"))
    stop("cannot find b_level")


message(
    paste0(
        "\n\n\n***** Bootrstrapping modularity using ",
        n_boot, 
        " resamples *****\n"
    )
)


# create directory to save results if not existent
dir_path = here("data", "moving_window")

if (!dir.exists(dir_path)) {
    
    message(
        paste0("Creating moving_window subdirectory into ",
               here("data"),
               " ..."
        )
    )
    
    dir.create(dir_path)
    
}


# Load data --------------------------------------------------------------------

message("\nBootstrapping modularity NoNEC/FullSamp/complete ...")

message("Loading data ...")

dat = merge(
    fread(here("data", "dat_FullSamp_NoNEC.csv")), 
    fread(here("data", "obs_weights_FullSamp_NoNEC.csv")), 
    by = 'int_year', 
    all.x = TRUE
)

# check weights
if ( 
    dat[, .(weight = sum(obs_weights)), by = int_year]$weight %>%
    round %>%
    unique %>%
    length %>%
    `!=`(1L)
)
    stop("weights don't sum to same number over time")

# get missing patterns
m_pattern = get_missing_pattern(
    dat = dat, 
    w_size = w_size_vec,
    include_isolates = FALSE)

for (w_size in w_size_vec) 
{
    
    message(paste0("\nWindow size = ", w_size, " ..."))
    
    message("\nRunning bootstrap on non-complete occupations ...\n")
        
    res = bootstrap_lrmod_mv(
        dat, 
        w_size = w_size, 
        n_cores = n_cores, 
        n_resample = n_boot, 
        level = b_level,
        verbose = 1
    )
    
    # save result
    fwrite(
        res, 
        paste0(dir_path, "/MV", w_size, "_NoNEC_BOOT.csv")
    )
        

    message("\nRunning bootstrap on complete occupations ...\n")
        
    # extract complete cases
    c_occs = m_pattern[get(paste0("m_count_win_", w_size)) == 0]$occ_label
    c_dat = dat[occ1 %in% c_occs & occ2 %in% c_occs]
    
    # booststrap LR modularity
    res_complete = bootstrap_lrmod_mv(
        c_dat, 
        w_size = w_size, 
        n_cores = n_cores, 
        n_resample = n_boot, 
        level = b_level,
        verbose = 1
    )
    
    message("Done!\nSaving Results ...")
    
    # save result
    fwrite(
        res_complete, 
        paste0(
            dir_path, 
            "/MV", 
            w_size, 
            "_NoNEC_complete", 
            w_size, 
            "_BOOT.csv"
        )
    )
    
}

message("\nDone!")

### END OF CODE ###
