#!/usr/bin/Rscript

## Fitting DCSBM to Goodman's 1981 Data
## Author: Barum Park

DEBUG = FALSE

if (!DEBUG) {

    source(".Rprofile")

    log_file = file(snakemake@log[[1]], open = "wt")
    sink(log_file, type = "output")
    sink(log_file, type = "message")

}

## ------------------------------------------------------------------
## Setup and loading data
## ------------------------------------------------------------------

# libraries
suppressMessages({
    library("sbmob")
    library("doParallel")
    library("doRNG")
})

# functions
import::from("src/R/utils.R", print_session, glass_table)
import::from("src/R/sim_utils.R", gen_init_spec)

# set threads
if (DEBUG) {

    n_threads = 4
    set.seed(354894)

} else {

    n_threads = snakemake@threads
    set.seed(snakemake@params[["seed"]])

}



## ------------------------------------------------------------------
## Fit models
## ------------------------------------------------------------------

logger::log_info("Loading data ...")

# load data

# data matrix
mobmat = glass_table()

# number of occupations
n_occs = NROW(mobmat)

# maximum no of blocks to fit
n_max_blocks = if (DEBUG) 6 else snakemake@params[["max_blocks"]]

# no of refits per model
n_refit = if (DEBUG) 30 else snakemake@params[["n_refit"]]

# Empty list to store results
res_list = vector(mode = "list", n_max_blocks - 1L)

logger::log_info("Start fitting models ...")
message("\nModel info ---------------------------------------------")

message("Number of occupations: ", n_occs)
message("Fitting up to ", n_max_blocks, " blocks")
message("Initializing ", n_threads - 1L, " times in parallel")

if (sbmob::is_connected(as.matrix(sbmob::mobmat_to_dat(mobmat)), FALSE)) {

    message("Mobility network is connected")

} else {

    message("Mobility network is disconnected")

}

message("--------------------------------------------------------\n")


# create clusters 
# note: change nullfile() to "" to see output
cl = makeCluster(n_threads - 1, outfile = nullfile())
registerDoParallel(cl)

# fit models
for (n_blocks in 2:n_max_blocks) {

    logger::log_info("Fitting ", n_blocks, " blocks to data ...")

    res = foreach(
        refit = 1:n_refit,
        .errorhandling = "pass",
        .packages = "sbmob",
        .export = "gen_init_spec"
    ) %dorng% {

        inits = gen_init_spec(refit)

        logger::log_info(
            "Fitting ",
            refit,
            switch(as.character(refit), "1" = "st", "2" = "nd", "3" = "rd", "th"),
            " initialization ..."
        )

        fit = sbm(
            mobmat_to_dat(mobmat),
            nblocks = n_blocks,
            verbose = 0L,
            gen_inits = inits[1],
            spectral_type = inits[2],
            control = list(
                maxit = 100,
                estep_maxit = 50,
                mstep_maxit = 300,
                mstep_m = 6,
                mstep_maxlinesearch = 50
            )
        )

        return(fit)

    }
    logger::log_info("Done!")

    # check convergence
    success = lapply(res, `[[`, "converged") |>
        sapply(function(w) isTRUE(w))
    
    n_err = sum(!success)

    if (n_err > 0)
        message(n_err, " model(s) didn't converge")

    # keep only converged models
    res = res[success]

    # get model with highest elbo
    max_res = which.max(sapply(res, `[[`, "elbo"))
    max_res = res[[max_res]]
    max_res$n_blocks = n_blocks

    message("Maximized ELBO: ", round(max_res$elbo, 5))

    res_list[[n_blocks - 1L]] = max_res

}

# stop cluster
stopCluster(cl)

if (!DEBUG) {

    # saving
    logger::log_info("Saving results ...")
    saveRDS(res_list, snakemake@output[["fitted_models"]])

    # closing log
    print_session()
    sink()

}

### EOF ###