#!/bin/env/Rscript

## Setting Up Simulation Runs
## Author: Barum Park

DEBUG = FALSE

## ------------------------------------------------------------------
## Simulation design
## ------------------------------------------------------------------

if (DEBUG) {

    setClass(
        "snakemake",
        representation(
            threads = "integer",
            input = "list",
            params = "list",
            output = "list",
            log = "list"
        )
    )

    snakemake = new(
        "snakemake",
        params = list(
            seed = 42,
            run = 1:20,
            image = c("symm", "cycl", "hier"),
            sample_sizes = c(50, 100, 500),
            block_sizes  = c(2, 3, 5),
            beta         = 1:3,
            alpha        = c(1, .75, .5),
            block10      = TRUE
        ),
        threads = 4L
    )

} else {

    source(".Rprofile")

    log_file = file(snakemake@log[[1]], open = "wt")
    sink(log_file, type = "output")
    sink(log_file, type = "message")

}

# libraries
suppressMessages({
    library("data.table")
})

# utility functions
import::from("src/R/utils.R", print_session)



## ------------------------------------------------------------------
## Creating fixed params (alpha, beta)
## ------------------------------------------------------------------

logger::log_info("Creating simulation design ...")

# set seed
sim_seed = snakemake@params[["seed"]]
set.seed(sim_seed)

sample_sizes = snakemake@params[["sample_sizes"]]

ab_pars = lapply(sort(sample_sizes), function(w) {
    list(lalpha = rnorm(w), lbeta = rnorm(w))
})
names(ab_pars) = paste0("n_", sort(sample_sizes))

if (!DEBUG) {

    logger::log_info("Saving fixed params ...")
    saveRDS(ab_pars, snakemake@output[["sim_fixed_pars"]])

}


## ------------------------------------------------------------------
## Creating random seeds
## ------------------------------------------------------------------

logger::log_info("Creating random seeds for simulation ...")

list_run = snakemake@params[["run"]]
list_image = snakemake@params[["image"]]
list_sample_sizes = snakemake@params[["sample_sizes"]]
list_block_sizes = snakemake@params[["block_sizes"]]
list_beta = snakemake@params[["beta"]]
list_alpha = snakemake@params[["alpha"]]

sdat = expand.grid(
    list_run,
    list_image,
    list_sample_sizes,
    list_block_sizes,
    list_beta,
    list_alpha
) |> 
    setDT()

setnames(sdat, c("run", "image", "samp", "block", "beta", "alpha"))


if (snakemake@params[["block10"]]) {

    # add 10 blocks condition to samp = 500 case
    b10 = expand.grid(
        list_run,
        list_image,
        list_beta,
        list_alpha
    ) |> 
        setDT()
    
    setnames(b10, c("run", "image", "beta", "alpha"))

    b10[, `:=`(samp = 500, block = 10)]
    setcolorder(b10, names(sdat))

    # update data
    sdat = rbind(sdat, b10)

    # udpate list
    list_block_sizes = c(list_block_sizes, 10)

}

sdat[, seed := {
    sim_seed +
    match(image, list_image) * 1 +
    match(samp, list_sample_sizes) * 10 +
    match(block, list_block_sizes) * 100 +
    match(beta, list_beta) * 1000 +
    match(alpha, list_alpha) * 10000 +
    match(run, list_run) * 100000
}]

# add sim file name
sdat[, filename := paste0(
    "sim_", run, "_", image, "_", samp, "_", block, "_", beta, "_", alpha, ".rds")
]

# check uniqueness
stopifnot(length(unique(sdat$seed)) == nrow(sdat))

if (!DEBUG) {

    logger::log_info("Saving seeds ...")
    fwrite(sdat, snakemake@output[["sim_seed_list"]])

    # close log
    print_session()
    sink()

}

### EOF ###