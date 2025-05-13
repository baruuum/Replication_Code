#!/bin/env/Rscript

## Aggregating Simulation Results
## Author: Barum Park

DEBUG = FALSE

if (!DEBUG) {

    source(".Rprofile")
    log_file = file(snakemake@log[[1]], open = "wt")
    sink(log_file, type = "output")
    sink(log_file, type = "message")

}

import::from("src/R/utils.R", print_session, print_table)

## ------------------------------------------------------------------
## Load simulation results
## ------------------------------------------------------------------

# check filenames
logger::log_info("Reading Data ...")

fp = if (DEBUG) {

    paste0(
        "/N/project/fb/parkba/projects/mobmeth/results/tmpfits/sim_11_symm_50_2_1_1",
        "_", 1:20, ".rds"
    )

} else {

    snakemake@input[["sim_refits"]]

}

# file names
fnames = sapply(fp, basename)

# common specification
sp = unique(
    sapply(
        fnames,
        function(w) gsub("(^sim_.+)(_\\d+\\.rds)$", "\\1", w)
    )
)

# get specs
run = as.integer(gsub("^sim_(\\d+)_.+$", "\\1", sp))
n = as.integer(gsub("^sim_\\d+_[a-z]+_(\\d+)_.+$", "\\1", sp))
K = as.integer(gsub("^sim_\\d+_[a-z]+_\\d+_(\\d+)_.+$", "\\1", sp))
itype = gsub("^sim_\\d+_([a-z]+)_.+$", "\\1", sp)
beta = as.numeric(gsub("^sim_\\d+_[a-z]+_\\d+_\\d+_(\\d+\\.*\\d*).+$", "\\1", sp))
alpha = as.numeric(gsub("^sim_.+_(\\d*\\.*\\d*)$", "\\1", sp))

message("no of nodes:  ", n)
message("no of blocks: ", K)
message("image type:   ", itype)
message("beta:         ", beta)
message("alpha:        ", alpha)
message("Length of list: ", length(fp))
message(
    "Refit #s: ", 
    paste0(
        sapply(
            fnames,
            function(w) gsub("(^sim_.+_)(\\d+)(\\.rds$)", "\\2", w)
        ),
        collapse = ", "
    )
)

# load results for each spec as list
sim_res = lapply(fp, readRDS)
names(sim_res) = fnames

logger::log_info("Extracting fit with max ELBO ...")

# fits and runtimes
fits = lapply(sim_res, `[[`, "fit")
runtimes = sapply(sim_res, `[[`, "run_time")

# data and ground truth (from first refit)
data = sim_res[[paste0(sp, "_1.rds")]]$data
gtruth = sim_res[[paste0(sp, "_1.rds")]]$gtruth
mobmat = sbmob::dat_to_mobmat(data)
sparse = if (sum(mobmat == 0) / prod(dim(mobmat)) > .70) TRUE else FALSE

# get elbo (NAs for models not converging)
elbos = sapply(fits, function(w) { 
    if ("elbo" %in% names(w)) { 
        w$elbo
    } else { 
        NA
    }
})

# check for NULL results ("passed")
passed_runs = sapply(elbos, is.na)
if (any(passed_runs)) {

    warning("There were ", sum(passed_runs), " passed runs")

    if (all(passed_runs))
        stop("All runs ended in error for spec ", sp)

}
    
# refit with max elbo
max_elbo = which.max(elbos)

# print crosstabs of class memberships
message("Crosstab with 'true' partition")
print_table(
    table(
        apply(sim_res[[max_elbo]]$fit$lxi, 1, which.max),
        gtruth$z
    )
)

logger::log_info("Saving results ...")

saveRDS(
    list(
        fit = sim_res[[max_elbo]],
        max_run = max_elbo,
        n = n,
        K = K,
        sparse = sparse,
        gtruth = gtruth,
        run_times = sapply(sim_res, `[[`, "run_time")
    ),
    snakemake@output[["res"]]
)

# print session info and close
print_session()

if (!DEBUG)
    sink()

### EOF ###