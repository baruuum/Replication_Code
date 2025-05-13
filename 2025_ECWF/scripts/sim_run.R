#!/bin/env/Rscript

## Simulation Runs
## Author: Barum Park

DEBUG = FALSE

if (!DEBUG) {

    source(".Rprofile")

    log_file = file(snakemake@log[[1]], open = "wt")
    sink(log_file, type = "output")
    sink(log_file, type = "message")

}

## ------------------------------------------------------------------
## Setup
## ------------------------------------------------------------------

# helper function
is.error = function(x) inherits(x, "try-error")

logger::log_info("Setting up simulation ...")

# libraries
suppressMessages({
    library("sbmob")
    library("data.table")
    library("Matrix")
})

# utility functions
import::from("src/R/sim_utils.R", gen_sim_lpsi, gen_sim_lpi, gen_init_spec)
import::from("src/R/utils.R", print_session)

if (!DEBUG) {

    # set threads
    n_threads = snakemake@threads

    # get output filename
    specs = basename(snakemake@output[[1]])

    # get fixed parameters
    fixed_pars = readRDS(snakemake@input[["sim_fixed_pars"]])

    # get seeds
    seed_list = fread(snakemake@input[["sim_seed_list"]])

} else {

    n_threads = 4
    specs = "sim_19_symm_50_2_1_0.75_12.rds"
    fixed_pars = readRDS("./results/sim/sim_fixed_pars.rds")
    seed_list = fread("./results/sim/sim_seed_list.csv")

}



## ------------------------------------------------------------------
## Simulations
## ------------------------------------------------------------------

# control params
tol = 1e-7
clist = list(
    maxit  = 300,
    tol = tol,
    estep_maxit = 50,
    estep_tol = tol,
    mstep_m = 6,
    mstep_epsilon = tol,
    mstep_past = 1,
    mstep_delta = tol,
    mstep_maxit = 100,
    mstep_maxlinesearch = 100,
    debug = FALSE
)

logger::log_info("Start Running Simulations ...")

# get params
run = as.integer(gsub("^sim_(\\d+)_.+$", "\\1", specs))
n = as.integer(gsub("^sim_\\d+_[a-z]+_(\\d+)_.+$", "\\1", specs))
K = as.integer(gsub("^sim_\\d+_[a-z]+_\\d+_(\\d+)_.+$", "\\1", specs))
itype = gsub("^sim_\\d+_([a-z]+)_.+$", "\\1", specs)
beta = as.numeric(gsub("^sim_\\d+_[a-z]+_\\d+_\\d+_(\\d+\\.*\\d*).+$", "\\1", specs))
alpha = as.numeric(gsub("^sim_.+_(\\d+\\.*\\d*)_\\d+\\.rds$", "\\1", specs))
refit_no = as.integer(gsub("^sim_.+_(\\d+)\\.rds$", "\\1", specs))

# use the same seed for the same run (i.e., same across refits)
# to ensure that the data is the same
sim_seed = seed_list[
    filename == gsub("(^sim_.+)(_\\d+)(\\.rds)$", "\\1\\3", specs),
    ]$seed

set.seed(sim_seed)

# check
stopifnot(
    isTRUE(all(!is.na(c(run, n, K, itype, beta, alpha, refit_no))))
)

message("\n")
message("-------------------------------------------------------------")
message("sim run no:   ", run)
message("refit no: ", refit_no)
message("-------------------------------------------------------------")
message("no of nodes:  ", n)
message("no of blocks: ", K)
message("image type:   ", itype)
message("gamma:        ", beta)
message("nu:           ", alpha)
message("-------------------------------------------------------------")
message("random seed:  ", sim_seed)
message("conv. tol.:   ", tol)
message("E-step maxit: ", clist$estep_maxit)
message("M-step maxit: ", clist$mstep_maxit)
message("-------------------------------------------------------------")

# get fixed params
ab = fixed_pars[[paste0("n_", n)]]
lalpha = ab$lalpha
lbeta = ab$lbeta
lpsi = beta * gen_sim_lpsi(itype, K)
lpi = gen_sim_lpi(alpha, K)

message("lpsi:")
message(paste0(capture.output(lpsi), collapse = "\n"))
message("\npi:")
message(capture.output(exp(lpi)))
message("\n\n")

# start time of simulation
sim_start_time = Sys.time()

# simulate data
z_true = 0
tab_z = table(z_true)
connected = FALSE

logger::log_info("Simulating data ...")
while (
    length(tab_z) != K ||
    min(tab_z) < 4 ||
    !connected
) {

    simres = sim_sbm_poisson(
        lpsi = lpsi,
        lpi = lpi,
        lalpha = lalpha,
        lbeta = lbeta
    )
    mobmat = simres$mobmat
    diag(mobmat) = 0
    dat = mobmat_to_dat(mobmat)

    z_true = simres$z
    tab_z = table(z_true)
    connected = is_connected(mobmat, verbose = FALSE)

}

# save parameters
gtruth = list(
    lpsi = lpsi,
    lpi = lpi,
    lalpha = lalpha,
    lbeta = lbeta,
    z = z_true
)

# sparseness
sparseness = sum(mobmat == 0) / prod(dim(mobmat))
sparse = if (sparseness > .70) TRUE else FALSE
message("\nSparseness of the simluated matrix is ", round(sparseness, 3))

message("Proportion of classes are :")
message(
    paste0(
        capture.output(
            prop.table(table(z_true))
        ),
        collapse = "\n"
    )
)

logger::log_info("\n\nStart fitting model ...")

# respecigying seed (in order to get random initial values)
set.seed(sim_seed + refit_no)

init_spec = gen_init_spec(refit_no)

start_time = Sys.time()
res = try(sbm(
    dat,
    gen_inits = init_spec[1],
    spectral_type = init_spec[2],
    nblocks = K,
    control = clist,
    sparse = sparse,
    n_threads = 1,
    verbose = T
))
end_time = Sys.time()

if (!is.error(res)) {

    logger::log_info("ELBO converged at : ", round(res$elbo, 3))

} else {

    logger::log_info("Encountered and error while fitting model")

}

logger::log_info("\nSaving results ...")

run_time = difftime(end_time, start_time, units = "sec")
out = if (!is.error(res)) {
    list(
        fit = res,
        run_time = run_time
    )
} else {

    list(
        fit = NA,
        run_time = run_time
    )

}

# add ground truth and data for first refit
if (refit_no == 1) {

    out$data = dat
    out$gtruth = gtruth

}

saveRDS(out, snakemake@output[[1]])
print_session()

if (!DEBUG)
    sink()

### EOF ###