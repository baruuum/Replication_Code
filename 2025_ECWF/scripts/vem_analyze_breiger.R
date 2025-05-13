#!/env/bin/Rscript

## Analyzing Results From DCSBM Fitted to Breiger's 1981 Data
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
    fp_res = "./results/fit/vem_fit_breiger.rds"

} else {

    n_threads = snakemake@threads
    fp_res = snakemake@input[["fitted_models"]]

}

# get utility functions
import::from(
    "src/R/utils.R",
    mat_to_latex,
    tsapply,
    print_session,
    print_table,
    upfirst
)
import::from("src/R/plot_utils.R", ggtheme, gen_block_lines, grab_legend)
import::from("src/R/loglin_utils.R", collapsed_fit)

# plot colors
vopt = "A"
virb = 0
vire = 1

# occ labels
occ_labels = c(
    "  1. Professionals, self-employed",
    "  2. Professionals, salaried",
    "  3. Managers",
    "  4. Sales, other",
    "  5. Proprietors",
    "  6. Clerks",
    "  7. Sales, retail",
    "  8. Crafts, manufacturing",
    "  9. Crafts, other",
    "10. Crafts, construction",
    "11. Service",
    "12. Operatives, other",
    "13. Operatives, manufacturing",
    "14. Laborers, manufacturing",
    "15. Laborers, other",
    "16. Farmers",
    "17. Farm laborers "
)



## ------------------------------------------------------------------
## Get models
## ------------------------------------------------------------------

logger::log_info("Loading data & models...")

# load data
data(breiger1981)

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
            floor(min(df_elbo$value) / 250) * 250,
            (floor(max(df_elbo$value) / 250) + 1L) * 250
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
        legend.background=element_rect(fill = "transparent", colour = NA)
    )

icl_plot_cropped = ggplot(
    df_elbo[blocks > 3 & stat == "Integrated Classification Likelihood"],
    aes(x = blocks, y = value)
) +
    geom_point() +
    geom_line(linetype = 3) +
    scale_x_continuous(
        name = NULL,
        breaks = 4:8
    ) +
    scale_y_continuous(name = NULL, limits = c(-1360, -1300)) +
    ggtheme +
    theme(
        axis.text = element_text(size = 7),
        plot.background = element_rect(fill = "transparent", colour = NA)
    )

icl_plot = icl_plot + annotation_custom(
    ggplotGrob(icl_plot_cropped),
    xmin = 4.15,
    xmax = 8.15, 
    ymin = -2580,
    ymax = -1600
)

if (!DEBUG) {

    pdf(snakemake@output[["plot_icl_breiger"]], width = 6, height = 5.5)
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
        snakemake@output[["tab_icl_breiger"]]
    )

}



## ------------------------------------------------------------------
## Plot mobility patterns and partitions
## ------------------------------------------------------------------

# create data.table out of mobility matrix
mobmat = breiger1981$mobmat
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

# get Breiger's blocks (note: class labels already ordered)
class_map_breiger = data.table(
    occ_no = 1:nrow(mobmat),
    class = breiger1981$H7
)
setorder(class_map_breiger, "class", "occ_no")

# add labels to sender
df[, occ_send_labeled := factor(
    i,
    levels = 1:NROW(mobmat),
    labels = occ_labels
)]

logger::log_info("Plotting base mobility pattern ...")

# baseline plot
g0 = ggplot(
    df,
    aes(
        x = occ_receive,
        y = occ_send_labeled
    )
) +
    geom_tile(aes(fill = log(y_scaled)), colour = "black") +
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
        na.value = "black"
    ) +
    scale_x_discrete(
        name = "",
        breaks = 1:NROW(mobmat),
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
        axis.text.y = element_text(hjust = 0)
    )


if (!DEBUG) {

    pdf(snakemake@output[["plot_heat_breiger"]], width = 12, height = 6)
    print(g0)
    dev.off()

}

# Calculating a couple of basic stats
logger::log_info("Selection of statistics:")

message(
    "Average mobilty ratio within block 1: ",
    df[
        i %in% class_map[class == 1]$occ_no & 
        j %in% class_map[class == 1]$occ_no &
        i != j, 
        mean(log(y_scaled))
    ] |> round(3)
)

message(
    "Average mobilty ratio within block 2: ",
    df[
        i %in% class_map[class == 2]$occ_no & 
        j %in% class_map[class == 2]$occ_no &
        i != j, 
        mean(log(y_scaled))
    ] |> round(3)
)

message(
    "Average mobilty ratio between block 1 and 2: ",
    df[
        (
            i %in% class_map[class == 1]$occ_no & 
            j %in% class_map[class == 2]$occ_no
        ) | (
            i %in% class_map[class == 2]$occ_no & 
            j %in% class_map[class == 1]$occ_no
        ),
        mean(log(y_scaled))
    ] |> round(3)
)

logger::log_info("Plotting Breiger and VEM partition ...")

# get block_lines for vem partition
bdf = gen_block_lines(class_map, rev_y = TRUE, draw_all = TRUE)

# VEM partition
g1 = ggplot(
    df,
    aes(
        x = occ_receive,
        y = occ_send
    )
) +
    geom_tile(aes(fill = log(y_scaled)), colour = "black") +
    geom_segment(
        data = bdf,
        aes(x = x, xend = xend, y = y, yend = yend),
        col = "white",
        linewidth = .5
    ) +
    scale_fill_viridis_c(
        option = vopt,
        begin = virb,
        end = vire,
        direction = -1,
        na.value = "black"
    ) +
    scale_x_discrete(
        name = "",
        breaks = 1:NROW(mobmat),
        expand = c(0, 0)
    ) +
    scale_y_discrete(
        name = "",
        breaks = 1:NROW(mobmat),
        expand = c(0, 0),
        limits = rev
    ) +
    ggtheme +
    theme(
        aspect.ratio = 1,
        legend.position = "none"
    ) +
    ggtitle("DCSBM Partition")


# get block lines for Breiger's partition
bdf_breiger = gen_block_lines(class_map_breiger, rev_y = TRUE, draw_all = TRUE)

# Breiger's partition
g2 = ggplot(
    df,
    aes(
        x = occ_receive,
        y = occ_send_labeled
    )
) +
    geom_tile(aes(fill = log(y_scaled)), colour = "black") +
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
        na.value = "black"
    ) +
    geom_segment(
        data = bdf_breiger,
        aes(x = x, xend = xend, y = y, yend = yend),
        col = "white",
        linewidth = .5
    ) +
    scale_x_discrete(
        name = "",
        breaks = 1:NROW(mobmat),
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
        axis.text.y = element_text(hjust = 0)
    ) +
    ggtitle("Breiger's Partition")

# get legend
leg = grab_legend(g2)

if (!DEBUG) {

    G = cbind(
        ggplotGrob(g2 + theme(legend.position = "none")),
        ggplotGrob(g1)
    )

    pdf(snakemake@output[["plot_comp_breiger"]], width = 12, height = 6.5)
    grid.arrange(
        G, leg,
        layout_matrix = rbind(c(1, 1, 1), c(NA, NA, 2)),
        heights = c(15, 1)
    )
    dev.off()

}



## ------------------------------------------------------------------
## Crosstable of partitions
## ------------------------------------------------------------------

logger::log_info("Creating crosstab between two partitions ...")

# prepare dataset
ctab_dat = merge(class_map, class_map_breiger, by = "occ_no", all = T)
setnames(ctab_dat, "class.x", "class_vem")
setnames(ctab_dat, "class.y", "class_breiger")

# get blockcomparisons
block_comp = ctab_dat[
    , c(
        "rand" = sbmob::rand_indx(class_vem, class_breiger, adjust = F),
        "nmi" = sbmob::nmi(class_vem, class_breiger, norm = 1L)
    )
]

rand = round(block_comp["rand"], 2)
nmi = round(block_comp["nmi"], 2)

message("Rand index: ", rand)
message("Normalized mutual information: ", nmi)

# create crosstab
ctab = as.matrix(ctab_dat[, table(class_breiger, class_vem)])
rownames(ctab) = paste0("Class ", as.roman(1:nrow(ctab)))
colnames(ctab) = paste0("Class ", as.roman(1:ncol(ctab)))

# change tab to character
class(ctab) = "character"

# drop zeros
ctab[ctab == "0"] = ""

# get latex code
tab_latex = mat_to_latex(
    ctab,
    rownames = TRUE,
    colnames = TRUE,
    rowtitle = "Breiger (H7)",
    coltitle = "DCSBM Partition",
    below_caption = paste0(
        "Note: Empty entries are cells with zero counts. The maximum a posteriori (MAP) estimates are used to allocated occupations into classes of the DCSBM partition. The Rand index and normalized mutual information between the partitions are, respectively, ",  # nolint
        rand, " and ", nmi, "."),
    digits = 0
)

if (!DEBUG) {

    writeLines(
        tab_latex,
        snakemake@output[["tab_crosstab_breiger"]]
    )

}
    

## ------------------------------------------------------------------
## Goodman(1981)'s approach
## ------------------------------------------------------------------


logger::log_info("Goodman's collapsibility tests")

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
## BIC for partitions
## ------------------------------------------------------------------

logger::log_info("Comparing partitions with BIC ...")

glm_list = list(
    glm_null = loglin_blocks(df, rep(1, length(class_map$class))),
    glm_breiger = loglin_blocks(df, class_map_breiger$class),
    glm_goodman = loglin_blocks(df, c(1:2, c(3, 3), 4:16)),
    glm_vem = loglin_blocks(df, class_map$class)
)

ic_tab = tsapply(glm_list, function(fit) {

    ic = fit$ic_tab[c("BIC (with diag)"), "IC"]
    ll = fit$ic_tab[2, "ll"]
    df = fit$ic_tab["BIC (with diag)", "df"]

    c(ll, df, ic)

})

# add number of classes
ic_tab = cbind(c(1, 8, 16, 6), ic_tab)
rownames(ic_tab) = c("Quasi-Independence", "Breiger", "Goodman", "DCSBM")
colnames(ic_tab) = c("No. of Classes", "Log-likelihood", "df", "BIC")
ic_tab = ic_tab[order(ic_tab[, "BIC"]), ]

# save output
if (!DEBUG)
    writeLines(
        mat_to_latex(ic_tab, rownames = T, colnames = T, digits = 3),
        snakemake@output[["tab_bic_comp_breiger"]]
    )



## ------------------------------------------------------------------
## Trying out Community Detection
## ------------------------------------------------------------------

if (!DEBUG) {

    set.seed(snakemake@params[["seed_breiger_community"]])
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
    weighted = TRUE,
    diag = FALSE
)

# symmetrized version
mobmat_sym = 0.5 * (mobmat + t(mobmat))
g_sym = graph_from_adjacency_matrix(
    mobmat_sym,
    mode = "undirected",
    weighted = TRUE,
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