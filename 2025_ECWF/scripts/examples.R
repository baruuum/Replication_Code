#!/usr/bin/Rscript

## Hypothetical Examples
## Author: Barum Park

DEBUG = FALSE

if (!DEBUG) {

    source(".Rprofile")
    
    log_file = file(snakemake@log[[1]], open = "wt")
    sink(log_file, type = "output")
    sink(log_file, type = "message")

}

# libraries
suppressMessages({
    library("sbmob")
    library("ggplot2")
    library("data.table")
})

# utility functions
import::from(
    "src/R/utils.R",
    inv_logit,
    mat_to_latex,
    print_session
)
import::from("src/R/plot_utils.R", gen_block_lines)

# set threads & seed
n_threads = if (DEBUG) 4 else snakemake@threads

if (!DEBUG)
    set.seed(snakemake@params[["seed_examples"]])


## ------------------------------------------------------------------
## Helper function
## ------------------------------------------------------------------

# plot simulated SBM
plot_rand_sbm = function(n, psi, z) {

    y = matrix(NA, n, n)
    for (i in 1:n) {
        for (j in 1:n) {

            p = psi[z[i], z[j]]
            y[i, j] = sample(c(0, 1), 1L, prob = c(1 - p, p))

        }
    }

    df = sbmob::mobmat_to_dat(y, index = 1)
    df[, `:=`(zi = z[i], zj = z[j])]
    df[
        , zi := factor(
            zi, 
            levels = sort(unique(zi)), 
            labels = paste0("Class ", strrep("I", sort(unique(zi))))
        )
    ][
        , zj := factor(
            zj, 
            levels = sort(unique(zj)), 
            labels = paste0("Class ", strrep("I", sort(unique(zi))))
        )
    ]
    
    
    ggplot(
        df,
        aes(y = factor(i), x = factor(j), fill = factor(y))
    ) +
        geom_tile(colour = "white") +
        scale_x_discrete(
            name = "",
            breaks = 1:n,
            expand = c(0, 0)
        ) +
        scale_y_discrete(
            name = "",
            breaks = 1:n,
            expand = c(0, 0),
            limits = rev
        ) +
        scale_fill_manual(
            values = c("gray95", "darkmagenta")
        ) +
        facet_grid(zi ~ zj, scales = "free", switch = "both") +
        theme(
            aspect.ratio = 1,
            legend.position = "none",
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            strip.background = element_rect(fill = NA),
            strip.placement = "outside",
            panel.spacing = unit(0, "pt"),
            panel.border = element_rect(color = "black", fill = NA, size = 1.5),
            strip.text = element_text(size = 14)
        )
    
    # theme(
    #     aspect.ratio = 1,
    #     legend.position = "none",
    #     axis.title = element_blank(),
    #     axis.text = element_blank(),
    #     axis.ticks = element_blank()
    # )
    

}



## ------------------------------------------------------------------
## Examples I (homophily)
## ------------------------------------------------------------------

logger::log_info("Creating homophily example ...")

# parameters
n = 20
Psi1 = matrix(
    c( 2, -2,
      -2,  2),
    nrow = 2,
    byrow = TRUE
)
Psi1 = inv_logit(Psi1)
rownames(Psi1) = colnames(Psi1) = paste0("Block ", 1:NROW(Psi1))

# simulate block memberships
z = c(rep(1, n / 2), rep(2, n / 2))


# plot
p1 = plot_rand_sbm(n, Psi1, z)

if (!DEBUG) {

    # save plot and matrix
    pdf(snakemake@output[["plot_homophily"]], width = 5, height = 5)
    print(p1)
    dev.off()

    writeLines(
        mat_to_latex(Psi1, digits = 2),
        snakemake@output[["tab_psi_homophily"]]
    )

}



## ------------------------------------------------------------------
## Example II (Hierarchy)
## ------------------------------------------------------------------

logger::log_info("Creating hierarchy example ...")

# parameters
Psi2 = matrix(
    c(2, -2, -2,
      2,  2, -2,
      2,  2,  2),
    nrow = 3,
    byrow = TRUE
)
Psi2 = inv_logit(Psi2)
rownames(Psi2) = colnames(Psi2) = paste0("Block ", 1:NROW(Psi2))

# block memberships
z2 = c(rep(1, 6), rep(2, 7), rep(3, 7))

# generate plot
p2 = plot_rand_sbm(n, Psi2, z2)

if (!DEBUG) {

    # save plot and matrix
    pdf(snakemake@output[["plot_hierarchy"]], width = 5, height = 5)
    print(p2)
    dev.off()

    writeLines(
        mat_to_latex(Psi2, digits = 2),
        snakemake@output[["tab_psi_hierarchy"]]
    )

}



## ------------------------------------------------------------------
## Examples III (exchange)
## ------------------------------------------------------------------

logger::log_info("Creating exchange example ...")

# parameters
Psi3 = matrix(
    c(-2, 2,
      2,  -2),
    nrow = 2,
    byrow = TRUE
)
Psi3 = inv_logit(Psi3)
rownames(Psi3) = colnames(Psi3) = paste0("Block ", 1:NROW(Psi3))

# simulate block memberships
z = c(rep(1, n / 2), rep(2, n / 2))


# plot
p3 = plot_rand_sbm(n, Psi3, z)

if (!DEBUG) {

    # save plot and matrix
    pdf(snakemake@output[["plot_exchange"]], width = 5, height = 5)
    print(p3)
    dev.off()

    writeLines(
        mat_to_latex(Psi3, digits = 2),
        snakemake@output[["tab_psi_exchange"]]
    )

}


## ------------------------------------------------------------------
## Example IV (Cyclical)
## ------------------------------------------------------------------

logger::log_info("Creating cyclical example ...")

# parameters
Psi4 = matrix(
    c(-2,  2, -2,
      -2, -2,  2,
       2, -2, -2),
    nrow = 3,
    byrow = TRUE
)
Psi4 = inv_logit(Psi4)
rownames(Psi4) = colnames(Psi4) = paste0("Block ", 1:NROW(Psi4))

# block memberships
z = c(rep(1, 6), rep(2, 7), rep(3, 7))

# generate  plot
p4 = plot_rand_sbm(n, Psi4, z)

if (!DEBUG) {

    # save plot and matrix
    pdf(snakemake@output[["plot_cyclical"]], width = 5, height = 5)
    print(p4)
    dev.off()

    writeLines(
        mat_to_latex(Psi4, digits = 2),
        snakemake@output[["tab_psi_cyclical"]]
    )

}


if (!DEBUG) {

    print_session()
    sink()

}

### EOF ###