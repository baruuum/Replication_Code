#!/env/bin/Rscript

## Analyzing Results From Simulation Runs
## Author: Barum Park

DEBUG = FALSE

# set threads & dirs
if (!DEBUG) {

    source(".Rprofile")

    log_file = file(snakemake@log[[1]], open = "wt")
    sink(log_file, type = "output")
    sink(log_file, type = "message")

    n_threads = snakemake@threads
    fp_sim_res = snakemake@input[["res"]]
    fp_plot_runtime = snakemake@output[["plot_runtime"]]
    fp_plot_rand = snakemake@output[["plot_rand"]]
    fp_plot_adj_rand = snakemake@output[["plot_adj_rand"]]
    fp_plot_nmi = snakemake@output[["plot_nmi"]]

} else {

    n_threads = 4
    fp_sim_res = dir(file.path("./results", "sim", "runs"), full.name = TRUE)

}

# libraries
suppressMessages({
    library("sbmob")
    library("ggplot2")
    library("data.table")
})

# set threads
setDTthreads(n_threads)

# get utility functions
import::from("src/R/utils.R", print_session, format_params, print_table)
import::from("src/R/plot_utils.R", ggtheme)



## ------------------------------------------------------------------
## Get simulation results
## ------------------------------------------------------------------


logger::log_info("Loading simulation results...")

# get results
sim_res = lapply(seq_along(fp_sim_res), function(i) {

    f = fp_sim_res[i]

    sim = readRDS(f)
    specs = strsplit(
        gsub("^sim_(.+)\\.rds$", "\\1", basename(f)), "_"
    )[[1]]

    z = apply(sim$fit$fit$lxi, 1L, which.max) - 1L
    z_true = sim$gtruth$z

    data.table(
        n = sim$n,
        blocks = sim$K,
        run = specs[1],
        type = specs[2],
        beta = specs[5],
        alpha = specs[6],
        sparse = sim$sparse,
        z_var = var(z),
        rand = sbmob::rand_indx(z, z_true, adjust = FALSE),
        nmi = sbmob::nmi(z, z_true, norm = 1L),
        elbo = sim$fit$fit$elbo,
        time = list(sim$run_times)
    )

}) |> 
    rbindlist()

# get "obviously" non-convergent runs
zvars = sim_res[z_var == 0]
logger::log_info("Summarizing runs with zero variance in class-memberships")
message(
    "\nTotal number of failed runs: ",
    nrow(zvars), " / ", nrow(sim_res)
)
message("size & blocks:")
print_table(table(zvars$n, zvars$blocks))
message("\n")
message("Block-structure type:")
print_table(table(zvars$type))
message("\n")
message("alpha (nu parameter in paper):")
print_table(table(zvars$alpha))
message("\n")
message("beta (gamma parameter in paper):")
print_table(table(zvars$beta))
message("\n")
message("sparse:")
print_table(table(zvars$sparse))
message("\n")


logger::log_info("Recoding variables for plotting ...")

# get results & recode
alpha_cross = data.table(
    alpha = c("0.5", "0.75", "1"),
    new_alpha = c("0.50", "0.75", "1.00")
)

# reformat parameters
alpha_unique = unique(sim_res$alpha)
alpha_cross = data.table(
    alpha = alpha_unique,
    alpha_new = format_params(alpha_unique)
)

beta_unique = unique(sim_res$beta)
beta_cross = data.table(
    beta = beta_unique,
    beta_new = format_params(beta_unique)
)

sim_res[alpha_cross, alpha := i.alpha_new, on = "alpha"]
sim_res[beta_cross, beta := i.beta_new, on = "beta"]


## ------------------------------------------------------------------
## Analyzing running times
## ------------------------------------------------------------------

logger::log_info("Plotting run times ...")

block_labs = paste0("M = ", sort(unique(sim_res$blocks)))
block_labs_refined = ifelse(
    grepl("M = 10", block_labs),
    block_labs,
    gsub("= ", "=  ", block_labs)
)

sim_res[, blocks := factor(
    paste0("M = ", blocks),
    levels = block_labs,
    labels = block_labs_refined
)]

n_labs = paste0("Nodes = ", sort(unique(sim_res$n)))
n_labs_refined = ifelse(
    grepl("Nodes = 10", n_labs),
    gsub("= ", "=  ", n_labs),
    n_labs
)
sim_res[, n := factor(paste0("Nodes = ", n), levels = n_labs, labels = n_labs_refined)]

ptime_dat = sim_res[
    , .(n, blocks, time)
][
    , .(time = unlist(time)), by = c("n", "blocks")
]

ptime_plot = ggplot(ptime_dat, aes(y = time)) +
    geom_boxplot(
        aes(x = blocks),
        width = .4,
        size = .5,
        outlier.shape = NA
    ) +
    scale_y_continuous(
        name = "Running time (seconds, log-scale)\n",
        trans = "log10"
    ) +
    scale_x_discrete(
        name = "\nNumber of Fitted Classes"
        # guide = guide_axis(n.dodge = 2)
    ) +
    facet_wrap(~ n, scales = "free_x") +
    ggtheme

if (!DEBUG) {

    pdf(fp_plot_runtime, width = 8, height =4.5)
    print(ptime_plot)
    dev.off()

}

message("Median runtimes:")
ptime_dat[
    , 
    as.list(quantile(time, c(.1, .5, .9))),
    by = c("n", "blocks")
][
    order(n, blocks)
] |>
    print_table()
    


## ------------------------------------------------------------------
## Analyzing block recovery
## ------------------------------------------------------------------

logger::log_info("Plotting block recovery stats ...")

# add labels for plotting
sim_res[, nk := paste0(n, ", ", blocks)]
nk_vals = unique(sim_res[order(as.integer(n), blocks)]$nk)
sim_res[, nk := factor(nk, levels = nk_vals, labels = nk_vals)]

alpha_vals = unique(sim_res$alpha) |> sort()
beta_vals = unique(sim_res$beta) |> sort()
sim_res[, alpha := factor(
        alpha,
        levels = alpha_vals,
        labels = paste0("nu = ", alpha_vals)
    )
][
    , beta := factor(
        beta,
        levels = beta_vals,
        labels = paste0("gamma = ", beta_vals)
    )
]

# reshape
pfit_dat = melt(
    sim_res[
        z_var != 0,
        c(
            "type",
            "nk",
            "beta",
            "alpha",
            "rand",
            "nmi"
        )
    ],
    id.vars = c("type", "nk", "beta", "alpha"),
    value.name = "value",
    variable.name = "measure"
)

# rand index
plot_rand = ggplot(pfit_dat[measure == "rand"], aes(y = value)) +
    geom_jitter(
        aes(x = nk, group = nk),
        width = .25,
        size = .25,
        alpha = .3,
        col = "darkmagenta"
    ) +
    stat_summary(
        aes(x = nk, group = nk),
        fun = median,
        geom = "point",
        size = 1.5,
        col = "black",
        shape = 19
    ) +
    facet_grid(alpha ~ beta) +
    scale_y_continuous(limits = c(-.05, 1.05)) +
    labs(
        x = "No. of Nodes and Classes\n",
        y = "\nRand Index"
    ) +
    ggtheme +
    theme(axis.text.x = element_text(size = 7)) +
    coord_flip()

if (!DEBUG) {

    pdf(fp_plot_rand, width = 8, height = 5)
    print(plot_rand)
    dev.off()

}

message("Rand Indices: ")
sum_stats = pfit_dat[
    measure == "rand",
    .(median_value = median(value)),
    by = c("nk", "alpha", "beta")
] 
print_table(sum_stats)
message("5 smallest Rand Indices:")
sum_stats[
    order(median_value)
][
    1:5
] |> 
    print_table()


# Normalized mutual information
plot_nmi = ggplot(pfit_dat[measure == "nmi"], aes(y = value)) +
    geom_jitter(
        aes(x = nk, group = nk),
        width = .25,
        size = .25,
        alpha = .3,
        col = "darkmagenta"
    ) +
    stat_summary(
        aes(x = nk, group = nk),
        fun = median,
        geom = "point",
        size = 1.5,
        col = "black",
        shape = 19
    ) +
    facet_grid(alpha ~ beta) +
    scale_y_continuous(limits = c(-.05, 1.05)) +
    labs(
        x = "No. of Nodes and Classes\n",
        y = "\nNormalized Mutual Information"
    ) +
    ggtheme +
    theme(axis.text.x = element_text(size = 7)) +
    coord_flip()

if (!DEBUG) {

    pdf(fp_plot_nmi, width = 8, height = 5)
    print(plot_nmi)
    dev.off()

}

message("NMI values: ")
sum_stats = pfit_dat[
    measure == "nmi",
    .(median_value = median(value)),
    by = c("nk", "alpha", "beta")
] 
print_table(sum_stats)
message("5 smallest NMI values:")
sum_stats[
    order(median_value)
][
    1:5
] |> 
    print_table()

# print session
if (!DEBUG) {

    print_session()
    sink()

}

### EOF ###