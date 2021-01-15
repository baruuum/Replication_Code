###############################################################################
## 
## Fitting map equation to moving windows
##
## Creator     : Barum Park
## Last Update : 02/21/2020
##
## Data specifications:
## 1) FullSamp/NoNEC
## 2) FullSamp/FullOccs
## 3) Female/NoNEC
## 4) Male/NoNEC
## 5) complete/NoNEC
## 6) Occ2000/NoNEC
## 7) FullSamp/NoNEC (ties reweighted by self-loops)
##
###############################################################################


## Basic Configurations -------------------------------------------------------

if (!exists("w_size_vec"))
    stop("cannot find w_size_vec")

if (!exists("n_cores"))
    stop("cannot find n_cores")


message(
    paste0("\n\n\n***** Fitting algorithm to moving windows of size ",
           paste0(w_size_vec, collapse = ", "),
           " *****\n"
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


# Fit algorithm to NoNEC ------------------------------------------------------

message("\nFitting algorithm for NoNEC occupations on FullSamp ...")

dat = merge(
    fread(here("data", "dat_FullSamp_agg_NoNEC.csv")), 
    fread(here("data", "obs_weights_FullSamp_NoNEC.csv")), 
    by = 'int_year', 
    all.x = TRUE
)[
    , weight := N * obs_weights
][
    , `:=`(N = NULL, obs_weights = NULL)
]

# check weights
if ( 
    dat[, .(weight = sum(weight)), by = int_year]$weight %>%
    round %>%
    unique %>%
    length %>%
    `!=`(1L)
)
    stop("weights don't sum to same number over time")

for (w_size in w_size_vec) 
{
    
    message(paste0("\nWindow size = ", w_size, " ..."))

    # fit moving windows
    res = fit_infomap_mv(
        dat, w_size = w_size, n_cores = n_cores, verbose = 0
    )
    
    # reweight by self-loops
    message("Reweighting by self-loops ...")
    res_reweighted = fit_infomap_mv(
        dat, 
        w_size = w_size, 
        n_cores = n_cores, 
        verbose = 0, 
        reweight_by_self_loops = TRUE
    )
    
    message("Done!\nSaving Results ...")
    
    # save result
    saveRDS(res, paste0(dir_path, "/MV", w_size, "_NoNEC.rds"))
    
    saveRDS(
        res_reweighted,
        paste0(dir_path, "/MV", w_size, "_NoNEC_reweighted.rds")
    )
    
}



# Fit algorithm to FullOccs ---------------------------------------------------

message("\nFitting algorithm for FullOccs occupations on FullSamp ...")

dat = merge(
    fread(here("data", "dat_FullSamp_agg_FullOccs.csv")), 
    fread(here("data", "obs_weights_FullSamp_FullOccs.csv")), 
    by = 'int_year', 
    all.x = TRUE
)[
    , weight := N * obs_weights
][
    , `:=`(N = NULL, obs_weights = NULL)
]

# check weights
if ( 
    dat[, .(weight = sum(weight)), by = int_year]$weight %>%
    round %>%
    unique %>%
    length %>%
    `!=`(1L)
)
    stop("weights don't sum to same number over time")


for (w_size in w_size_vec) 
{
    
    message(paste0("\nWindow size = ", w_size, " ..."))
    
    # fit moving windows
    res = fit_infomap_mv(
        dat, w_size = w_size, n_cores = n_cores, verbose = 0
    )
    
    message("Done!\nSaving Results ...")
    
    # save result
    saveRDS(res, paste0(dir_path, "/MV", w_size, "_FullOccs.rds"))
    
}



# Fit algorithm to male and female subpopulations -----------------------------

for (subp in c("Male", "Female"))
{

    message(
        paste0(
            "\nFitting algorithm to NoNEC occupations on ",
            subp, 
            " subpopulation ..."
        )
    )

    dat = merge(
        fread(
            here("data", 
                 paste0("dat_", subp, "_agg_NoNEC.csv")
            )
        ), 
        fread(
            here("data", 
                 paste0("obs_weights_", subp, "_NoNEC.csv")
            )
        ), 
        by = 'int_year', 
        all.x = TRUE
    )[
        , weight := N * obs_weights
    ][
        , `:=`(N = NULL, obs_weights = NULL)
    ]
    
    # check weights
    if ( 
        dat[, .(weight = sum(weight)), by = int_year]$weight %>%
        round %>%
        unique %>%
        length %>%
        `!=`(1L)
    )
        stop("weights don't sum to same number over time")
    
    
    for (w_size in w_size_vec) 
    {
        
        message(paste0("\nWindow size = ", w_size, " ..."))
        
        # fit moving windows
        res = fit_infomap_mv(
            dat, w_size = w_size, n_cores = n_cores, verbose = 0
        )
        
        message("Done!\nSaving Results ...")
        
        # save result
        saveRDS(
            res, 
            paste0(dir_path, "/MV", w_size, "_", subp, "_NoNEC.rds")
        )
        
    }
    
}



# Fit algorithm to NoNEC (occupations appearing in all windows only) ----------

message("\nFitting algorithm for NoNEC occupations appearing in all windows ...")

dat = merge(
    fread(here("data", "dat_FullSamp_agg_NoNEC.csv")), 
    fread(here("data", "obs_weights_FullSamp_NoNEC.csv")), 
    by = 'int_year', 
    all.x = TRUE
)[
    , weight := N * obs_weights
][
    , `:=`(N = NULL, obs_weights = NULL)
]

# check weights
if ( 
    dat[, .(weight = sum(weight)), by = int_year]$weight %>%
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

# object to store retained occupations
n_occs_complete = integer(length(w_size_vec)) %>%
    `names<-`(paste0("win_size_", w_size_vec))


for (w_size in w_size_vec) 
{
    
    message(paste0("\nWindow size = ", w_size, " ..."))
    
    # extract complete cases
    c_occs = m_pattern[get(paste0("m_count_win_", w_size)) == 0]$occ_label
    c_dat = dat[occ1 %in% c_occs & occ2 %in% c_occs]
    
    # store numbers
    n_occs_complete[paste0("win_size_", w_size)] = length(c_occs)
    
    # fit moving windows
    res = fit_infomap_mv(
        c_dat, w_size = w_size, n_cores = n_cores, verbose = 0
    )
    
    message("Done!\nSaving Results ...")
    
    # save result
    saveRDS(
        res, 
        paste0(dir_path, "/MV", w_size, "_NoNEC_complete", w_size, ".rds")
    )
    
}



# Fit algorithm to NoNEC (OCC2000 codes) --------------------------------------

message("\nFitting algorithm for NoNEC occupations appearing in all windows ...")

dat = merge(
    fread(here("data", "dat_FullSamp_agg_NoNEC_OCC2000.csv")), 
    fread(here("data", "obs_weights_FullSamp_NoNEC_OCC2000.csv")), 
    by = 'int_year', 
    all.x = TRUE
)[
    , weight := N * obs_weights
][
    , `:=`(N = NULL, obs_weights = NULL)
]

# check weights
if ( 
    dat[, .(weight = sum(weight)), by = int_year]$weight %>%
    round %>%
    unique %>%
    length %>%
    `!=`(1L)
)
    stop("weights don't sum to same number over time")



for (w_size in w_size_vec) 
{
    
    message(paste0("\nWindow size = ", w_size, " ..."))
    
    # fit moving windows
    res = fit_infomap_mv(
        dat, w_size = w_size, n_cores = n_cores, verbose = 0
    )
    
    message("Done!\nSaving Results ...")
    
    # save result
    saveRDS(res, paste0(dir_path, "/MV", w_size, "_NoNEC_OCC2000.rds"))
    
}

### END OF CODE ###
