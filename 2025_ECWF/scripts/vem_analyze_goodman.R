#!/env/bin/Rscript

## Analyzing Results From DCSBM Fitted to Goodman's 1981 Data
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
    library("gridExtra")
    library("igraph")
    library("data.table")
})

# set threads & dirs
if (DEBUG) {

    n_threads = 4
    fp_res = "./results/fit/vem_fit_goodman.rds"

} else {

    n_threads = snakemake@threads
    fp_res = snakemake@input[["fitted_models"]]

}

# get utility functions
import::from(
    "src/R/utils.R",
    print_session,
    print_table,
    glass_table,
    mat_to_latex,
    upfirst
)
import::from("src/R/plot_utils.R", gen_block_lines, ggtheme)
import::from("src/R/loglin_utils.R", collapsed_fit)

# plot colors
vopt = "A"
virb = 0
vire = 1

# occ labels
occ_labels = c(
    "1.  Professional and high administrative",
    "2.  Managerial and executive",
    "3.  Other nonmanual (high grade)",
    "4.  Other nonmanual (low grade)",
    "5a. Routine grades of nonmanual",
    "5b. Skilled manual",
    "6.  Semiskilled manual",
    "7.  Unskilled manual"
)



## ------------------------------------------------------------------
## Get models
## ------------------------------------------------------------------

logger::log_info("Loading data & models...")

# data matrix
mobmat = glass_table()

# load models
res = readRDS(fp_res)



## ------------------------------------------------------------------
## Elbo and ICL
## ------------------------------------------------------------------

logger::log_info("Plotting ICL and ELBO ...")

# elbo & ICL
elbo = sapply(res, `[[`, "elbo")
icl = sapply(res, `[[`, "ICL")
blocks = seq_along(res) + 1L
max_icl = which.max(icl) + 1L

df_elbo = rbind(
    data.table(blocks = blocks, value = elbo, stat = "ELBO"),
    data.table(blocks = blocks, value = icl, stat = "ICL")
)

df_elbo[, stat := factor(
    stat,
    levels = c("ELBO", "ICL"),
    labels = c(
        "Maximized ELBO",
        "Integrated Classification Likelihood"
    )
)]

icl_plot = ggplot(
    df_elbo,
    aes(x = blocks, y = value, col = stat)
) +
    geom_vline(
        xintercept = max_icl,
        linetype = 1,
        col = "darkmagenta",
        alpha = .6
    ) +
    geom_point() +
    geom_line(linetype = 3) +
    scale_x_continuous(
        name = "\nNumber of Fitted Classes",
        breaks = 2:8
    ) +
    scale_y_continuous(
        name = "",
        limits = c(
            -400, -100
        )
    ) +
    scale_color_manual(
        name = "",
        values = c("darkgrey", "black")
    ) +
    guides(
        color = guide_legend(position = "inside"),
        linetype = guide_legend(position = "inside")
    ) +
    ggtheme +
    theme(
        legend.position.inside = c(.025, .925),
        legend.justification = 0,
        legend.direction = "vertical",
        legend.spacing.y = unit(-1, "line"),
        legend.background = element_rect(fill = "transparent", colour = NA)
    )


if (!DEBUG) {

    pdf(snakemake@output[["plot_icl_goodman"]], width = 5, height = 4.5)
    print(icl_plot)
    dev.off()

    # get ICL stats
    icl_stats = as.matrix(
        df_elbo[stat == "Integrated Classification Likelihood", -"stat"]
    )
    colnames(icl_stats) = c("Number of Classes", "ICL")

    # trim digits
    icl_stats = gsub(
        "(-\\d+\\.\\d{2})\\d+\\w", "\\1", 
        mat_to_latex(icl_stats, rownames = FALSE, colnames = TRUE)
    )
    
    writeLines(
        icl_stats,
        snakemake@output[["tab_icl_goodman"]]
    )


}



## ------------------------------------------------------------------
## Plot mobility patterns and partitions
## ------------------------------------------------------------------

# create data.table out of mobility matrix
df = mobmat_to_dat(mobmat, index = 1)
df[, occ_send := factor(i, levels = 1:NROW(mobmat))]
df[, occ_receive := factor(j, levels = 1:NROW(mobmat))]
df[, diags := factor(as.numeric(i == j) * i)]

# get predicted based on quasi-independence
df[i != j, yhat := loglin_blocks(df, rep(1, nrow(mobmat)))$fit$fitted.values]
df[i == j, yhat := y]
df[, y_scaled := y / yhat]

# get VEM blocks
vem = res[[max_icl - 1L]]

# create mapping from occ to class
class_map = data.table(
    unord_class = vem$post_z,
    occ_no = seq_len(nrow(vem$lxi))
)

# reorder class (according to mean occ_no)
class_map[
    , class := match(
        unord_class,
        class_map[, mean(occ_no), unord_class][order(V1)]$unord_class
    )
][
    # drop old class labels
    , unord_class := NULL
]
setorder(class_map, "class", "occ_no")

# add labels to sender
df[, occ_send_labeled := factor(
    i,
    levels = 1:NROW(mobmat),
    labels = occ_labels
)]

logger::log_info("Plotting mobility pattern with partition ...")

# get block_lines for vem partition
bdf = gen_block_lines(class_map, rev_y = TRUE, draw_all = TRUE)

# get pattern for missing cells
min_x = min_y = .5
max_x = 1.5
max_y = 2.5
mis_vert = rbindlist(
    lapply(seq(min_x + .1, max_x - .1, .1), function(w) {
        data.table(
            x = w, xend = w, y = min_y, yend = max_y
        )
    })
)
mis_hor = rbindlist(
    lapply(seq(min_y + .1, max_y - .1, .1), function(w) {
        data.table(
            x = min_x, xend = max_x, y = w, yend = w
        )
    })
)

mis_cells = rbind(mis_vert, mis_hor)

# plot partition
g0 = ggplot(
    df,
    aes(
        x = occ_receive,
        y = occ_send_labeled
    )
) +
    geom_tile(aes(fill = log(y_scaled)), colour = "black") +
    geom_segment(
        data = mis_cells, 
        aes(x = x, xend = xend, y = y, yend = yend),
        col = "black",
        linewidth = .1
    ) +
    geom_segment(
        data = bdf,
        aes(x = x, xend = xend, y = y, yend = yend),
        col = "white",
        linewidth = .8
    ) +
    scale_fill_viridis_c(
        name = expression(
            paste(
                "log ",
                bgroup("(", frac("Observed", "Quasi-Indep."), ")"),
                " "
            )
        ),
        option = vopt,
        begin = virb,
        end = vire,
        direction = -1,
        na.value = "white"
    ) +
    scale_x_discrete(
        name = "",
        breaks = 1:NROW(mobmat),
        labels = gsub("(^.+)(\\..+$)", "\\1", occ_labels),
        expand = c(0, 0)
    ) +
    scale_y_discrete(
        name = "",
        breaks = occ_labels,
        expand = c(0, 0),
        limits = rev
    ) +
    ggtheme +
    theme(
        aspect.ratio = 1,
        legend.position = "bottom",
        axis.text.y = element_text(hjust = 0, size = 12)
    )


if (!DEBUG) {

    pdf(snakemake@output[["plot_heat_goodman"]], width = 6.5, height = 5)
    # pdf("/Volumes/D/_projects/mobmeth/results/plots/heat_goodman.pdf", width = 6.5, height = 5)
    print(g0)
    dev.off()

}



## ------------------------------------------------------------------
## Goodman(1981)'s approach
## ------------------------------------------------------------------

logger::log_info("Running Goodman's collapsibility tests")

vem_part = lapply(
    sort(unique(class_map$class)),
    function(w) {
        class_map[class == w]$occ_no
    }
)

vem_collapsed = collapsed_fit(vem_part, mobmat)[
    type == "G2"
][
    , -"type"
]

# rename results for printing
setnames(
    vem_collapsed,
    c("G2", "df", "p", "Collapsed Rows/Cols")
)

vem_tests = mat_to_latex(vem_collapsed, digits = 3)

message("Classes that pass test in VEM partition: ")

vem_collapsed[p > .05] |>
    print_table()

plist = combn(seq_len(nrow(mobmat)), 2)
pairwise_collapsed = collapsed_fit(
    lapply(seq_len(ncol(plist)), function(w) plist[, w]),
    mobmat
)[
    type == "G2",
][
    , -"type"
]

setnames(
    pairwise_collapsed,
    c("G2", "df", "p", "Collapsed Rows/Cols")
)

pair_tests = mat_to_latex(pairwise_collapsed, digits = 3)

message("Pairs that pass the test:")
pairwise_collapsed[p > .05] |>
    print_table()

# save tables
if (!DEBUG) {

    logger::log_info("Saving tables ...")

    writeLines(
        vem_tests,
        snakemake@output[["collapsed_vem"]]
    )

    writeLines(
        pair_tests,
        snakemake@output[["collapsed_pairs"]]
    )

}


## ------------------------------------------------------------------
## Trying out Community Detection
## ------------------------------------------------------------------

if (!DEBUG) {

    set.seed(snakemake@params[["seed_goodman_community"]])
    cd_list = as.list(snakemake@params[["clust_algs"]])

} else {

    set.seed(543123)

    # cluster using network algorithms
    cd_list = list(
        "cluster_infomap",
        "cluster_walktrap",
        "cluster_edge_betweenness",
        "cluster_louvain",
        "cluster_leiden"
    )

}


logger::log_info("Trying out community detection algorithms ...")

rownames(mobmat) = colnames(mobmat) = paste0("occ", 1:nrow(mobmat))
g = graph_from_adjacency_matrix(
    mobmat,
    mode = "directed",
    weighted = "weight",
    diag = FALSE
)

# symmetrized version
# symmetrized version
mobmat_sym = 0.5 * (mobmat + t(mobmat))
g_sym = graph_from_adjacency_matrix(
    mobmat_sym,
    mode = "undirected",
    weighted = "weight",
    diag = FALSE
)

clust_res = lapply(cd_list, function(w) {

    f = get(w, envir = .GlobalEnv)
    symmetrized = FALSE

    if (w == "cluster_infomap") {
        clusts = f(g, e.weights = E(g)$weights, modularity = FALSE)

    } else if (
        w %in% c("cluster_edge_betweenness", "cluster_leiden", "cluster_louvain")
    ) {

        clusts = suppressWarnings(f(g_sym, weights = E(g_sym)$weight))
        symmetrized = TRUE

    } else {

        clusts = suppressWarnings(f(g, weights = E(g)$weight))

    }

    alg_name = gsub(
        "_", " ", gsub(
            "cluster_", "", w
        )
    ) |>
        upfirst()

    dt = data.table(Occupation = occ_labels)
    dt[, (alg_name) := clusts$membership]

    return(dt)

})

clust_df = Reduce(function(...) merge(..., all = TRUE, by = "Occupation"), clust_res)

if (!DEBUG) {

    # save clustering results
    writeLines(
        mat_to_latex(clust_df, digits = 0),
        snakemake@output[["clustering"]]
    )

    print_session()
    sink()

}

### EOF ###