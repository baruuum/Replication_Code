
## ------------------------------------------------------------------
## Setup
## ------------------------------------------------------------------

library("here")
library("data.table")
library("plotly")
library("ggplot2")
library("rstan")

# load functions
source(here("scripts", "0_Functions.R"))

# set seed
set.seed(42)

# order for plotting
g_order = c(
    "White",
    "Black",
    "Hispanic",
    "Asian",
    "Cons",
    "Rel. Never",
    "Prison",
    "Gay/Les"
)

## ------------------------------------------------------------------
## Helper Functions
## ------------------------------------------------------------------

# printing values of D (and posterior intervals)
print_dval = function(dat, x, y) {
    
    if (nrow(dat[item_1 == x & item_2 ==y]) != 0) {
        
        df = dat[item_1 == x & item_2 == y, .(item_1, item_2, q025, q50, q975)]
        
    } else {
        
        df = dat[item_1 == y & item_2 == x, .(item_1, item_2, q025, q50, q975)]
        
    }
    
    message(
        paste0(
            "Posterior median D-value for pair ",
            x, 
            " and ",
            y,
            " is ",
            round(df$q50, 5),
            " with 95% posterior interval of (",
            round(df$q025, 5),
            ", ",
            round(df$q975, 5),
            ")"
        )
    )
    
}

# function to control order of groups in ggplots
reorder_within = function(x, by, within, fun = mean, sep = "___", ...) {
    new_x = paste(x, within, sep = sep)
    stats::reorder(new_x, by, FUN = fun)
}

scale_x_reordered = function(..., sep = "___") {
    reg = paste0(sep, ".+$")
    ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}


## ------------------------------------------------------------------
## Get Data
## ------------------------------------------------------------------

# get data
post_samps = fread(here("data", "post_samples.csv"))
item_sum = fread(here("data", "item_sum_w_labs.csv"))

# get params of interest
samps = post_samps[
    , grepl("(pos\\_|gamma\\[|alpha)", names(post_samps)) & 
        !grepl("star", names(post_samps)), 
    with = F
]


## ------------------------------------------------------------------
## Get D-values
## ------------------------------------------------------------------

# no of samples
n_samps = nrow(samps)
    
# get individual positions
pos_i = gen_positions(samps, "i")
n_ind = nrow(pos_i[[1]])

# get group positions
pos_j = gen_positions(samps, "j")
n_items = nrow(pos_j[[1]])

# combine to list
comb_list = mapply(list, pos_i, pos_j, SIMPLIFY = F)

# calculate D-values for all pairs and draws
ij_dists = lapply(comb_list, function (w) {
    
    x = vec_pdist(w[[1]], w[[2]])
    t(
        apply(expand.grid(1:n_items, 1:n_items), 1, function (z) {
            sd(x[, z[1]] - x[, z[2]])
        }) 
    )
    
})

# create dataset
tmp = data.table(
    cbind(
        expand.grid(1:n_items, 1:n_items), 
        t(do.call(rbind, ij_dists))
    )
)
setnames(tmp, c("item_1", "item_2", paste0("draw_", 1:n_samps)))
df = melt(tmp, id.vars = c("item_1", "item_2"))

# generate dataset
d_dat = merge(
    df, 
    item_sum[, c("item_no", "short_labs")], 
    by.x = "item_1",
    by.y = "item_no"
)
# add labels for second group
d_dat[, item_1 := NULL]
setnames(d_dat, "short_labs", "item_1")
d_dat = merge(
    d_dat, 
    item_sum[, c("item_no", "short_labs")], 
    by.x = "item_2",
    by.y = "item_no"
)
d_dat[, item_2 := NULL]
setnames(d_dat, "short_labs", "item_2")

# drop same-group D-vals
d_dat = d_dat[item_1 != item_2]

# get quantiles
q_dat_dup = d_dat[
    , as.list(quantile(value, c(.025,.1, .5, .9, .975))), 
    by = list(item_1, item_2)
]
setnames(q_dat_dup, c("item_1", "item_2", "q025", "q10", "q50", "q90", "q975"))

# check number of duplicates (note: D-vals are symmetric)
stopifnot(
    sum(duplicated(q_dat_dup[, -c("item_1", "item_2")])) == nrow(q_dat_dup) / 2L
)

# drop duplicates 
d_labs = unique(q_dat_dup[, c(item_1, item_2)])
d_combs = t(combn(d_labs, 2))
q_dat = q_dat_dup[
    paste0(item_1, item_2) %in% apply(d_combs, 1, function(w) paste0(w[1], w[2])), 
]


# smallest median D-value
min_d = q_dat[q50 == min(q50)]
message(
    paste0(
        "Pair with smallest D-value is ",
        min_d$item_1,
        " and ",
        min_d$item_2,
        " with a median value of ",
        round(min_d$q50, 5),
        " and 95% posterior interval of (",
        round(min_d$q025, 5),
        ", ",
        round(min_d$q975, 5),
        ")"
    )
)


# posterior median of mean D-value
med_d = d_dat[ 
    , .(m = mean(value)), by = variable
][
    , median(m)
]
message(
    paste0(
        "Median of the posterior mean D-value across all groups is ",
        round(med_d, 5)
    )
)


# print out values in manuscript
print_dval(q_dat, "2nd. Home", "Rel. Never")
print_dval(q_dat, "Lib", "Cons")
print_dval(q_dat, "Rel. Reg", "Rel. Never")
print_dval(q_dat, "White", "Asian")
print_dval(q_dat, "White", "Black")
print_dval(q_dat, "White", "Hispanic")
print_dval(q_dat, "Hispanic", "Unemp")
print_dval(q_dat, "Asian", "Unemp")

# plot D-values (selected vars.)
g_short = g_order
g_long = item_sum[match(g_short, item_sum$short_labs)]$long_labs

plot_dat_d = q_dat_dup[
    item_2 %in% g_short
][
    , o := rank(-q50), by = item_2
][
    , item_2 := factor(item_2, levels = g_short, labels = g_long)
][
    , o_labs := item_1[order(-q50)]
]


# save plot
message("Saving D-vals plot (selected groups) ...")
pdf(here("output", "plots", "dvals.pdf"), width = 8, height = 5)
print(
    ggplot(
        plot_dat_d, 
        aes(
            x = reorder_within(factor(item_1), -q50, item_2), 
            y = q50
        )
    ) +
    geom_hline(yintercept = med_d, linetype = 2, col = "grey70", size = .25) +
    geom_point(size = 1)+
    geom_linerange(aes(ymin = q025, ymax=q975), size = .25) +
    geom_linerange(aes(ymin = q10, ymax = q90), size = .75) +
    facet_wrap(~item_2, nrow = 2, scale = "free_x") +
    scale_x_reordered() +
    theme_bw() +
    theme(
        axis.text.x = element_text(
            angle = 90, 
            vjust = 0.4, 
            hjust = 1, 
            size = 8
        ),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white")
    )
)
dev.off()

d_all = dcast(
    q_dat_dup[, c("item_1", "item_2", "q50")], 
    item_1 ~ item_2, value.var = "q50"
)[
    , -c("item_1")
] 
d_all_mat = as.matrix(d_all)
rownames(d_all_mat) = colnames(d_all_mat)
    
new_order = c(1, 13, 11, 8, 10, 2, 7, 3, 6, 12, 4, 9)
plot_dat_d_all = d_all_mat[new_order, new_order]

message("Saving D-vals plot (all groups) ...")
pdf(here("output", "plots", "dvals_all_heat.pdf"), height=5, width=6)
print(
    ggplot(
        reshape2::melt(plot_dat_d_all), 
        aes(x = Var1, y = Var2, fill = value)
    ) +
    geom_tile()+
    scale_fill_gradient(
        name = "Distance\n\n",
        low  = "black",
        high = "white", 
        na.value = "black"
    ) +
    coord_fixed() +
    theme_bw()+
    scale_x_discrete(expand = c(0, 0))+
    scale_y_discrete(expand = c(0, 0))+
    labs(x = NULL, y = NULL) +
    theme(
        axis.text.x= element_text(angle = 90),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "right",
        legend.spacing.x = unit(.25, "line"),
        legend.key.width=unit(1, "line"),
        legend.title = element_blank()
    )
)
dev.off()


## ------------------------------------------------------------------
## D-vals using MAD
## ------------------------------------------------------------------


message("Calculating D-vals using MADs instead of SDs ...")

# calculate D-values for all pairs and draws
ij_dists_mad = lapply(comb_list, function (w) {
    
    x = vec_pdist(w[[1]], w[[2]])
    t(
        apply(expand.grid(1:n_items, 1:n_items), 1, function (z) {
            mad(x[, z[1]] - x[, z[2]], constant = 1.0)
        }) 
    )
    
})

# create dataset
tmp = data.table(
    cbind(
        expand.grid(1:n_items, 1:n_items), 
        t(do.call(rbind, ij_dists_mad))
    )
)
setnames(tmp, c("item_1", "item_2", paste0("draw_", 1:n_samps)))
df_mad = melt(tmp, id.vars = c("item_1", "item_2"))

# generate dataset
d_dat_mad = merge(
    df_mad, 
    item_sum[, c("item_no", "short_labs")], 
    by.x = "item_1",
    by.y = "item_no"
)
# add labels for second group
d_dat_mad[, item_1 := NULL]
setnames(d_dat_mad, "short_labs", "item_1")
d_dat_mad = merge(
    d_dat_mad, 
    item_sum[, c("item_no", "short_labs")], 
    by.x = "item_2",
    by.y = "item_no"
)
d_dat_mad[, item_2 := NULL]
setnames(d_dat_mad, "short_labs", "item_2")

# drop same-group D-vals
d_dat_mad = d_dat_mad[item_1 != item_2]

# get quantiles
q_dat_mad_dup = d_dat_mad[
    , as.list(quantile(value, c(.025,.1, .5, .9, .975))), 
    by = list(item_1, item_2)
]
setnames(q_dat_mad_dup, c("item_1", "item_2", "q025", "q10", "q50", "q90", "q975"))

# check number of duplicates (note: D-vals are symmetric)
stopifnot(
    sum(duplicated(q_dat_mad_dup[, -c("item_1", "item_2")])) == nrow(q_dat_mad_dup) / 2L
)

# drop duplicates 
d_labs_mad = unique(q_dat_mad_dup[, c(item_1, item_2)])
d_combs_mad = t(combn(d_labs_mad, 2))
q_dat_mad = q_dat_mad_dup[
    paste0(item_1, item_2) %in% apply(d_combs_mad, 1, function(w) paste0(w[1], w[2])), 
]


# posterior median of mean D-value
med_d_mad = d_dat_mad[ 
    , .(m = mean(value)), by = variable
][
    , median(m)
]

plot_dat_d_mad = q_dat_mad_dup[
    item_2 %in% g_short
][
    , o := rank(-q50), by = item_2
][
    , item_2 := factor(item_2, levels = g_short, labels = g_long)
][
    , o_labs := item_1[order(-q50)]
]


# save plot
message("Saving D-vals plot (selected groups) using MAD ...")
pdf(here("output", "plots", "dvals_mad.pdf"), width = 8, height = 5)
print(
    ggplot(
        plot_dat_d_mad, 
        aes(
            x = reorder_within(factor(item_1), -q50, item_2), 
            y = q50
        )
    ) +
        geom_hline(yintercept = med_d_mad, linetype = 2, col = "grey70", size = .25) +
        geom_point(size = 1)+
        geom_linerange(aes(ymin = q025, ymax=q975), size = .25) +
        geom_linerange(aes(ymin = q10, ymax = q90), size = .75) +
        facet_wrap(~ item_2, nrow = 2, scale = "free_x") +
        scale_x_reordered() +
        theme_bw() +
        theme(
            axis.text.x = element_text(
                angle = 90, 
                vjust = 0.4, 
                hjust = 1, 
                size = 8
            ),
            axis.title = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_rect(fill = "white")
        )
)
dev.off()

d_all_mad = dcast(
    q_dat_mad_dup[, c("item_1", "item_2", "q50")], 
    item_1 ~ item_2, value.var = "q50"
)[
    , -c("item_1")
] 
d_all_mat_mad = as.matrix(d_all_mad)
rownames(d_all_mat_mad) = colnames(d_all_mat_mad)

new_order = c(1, 13, 11, 8, 10, 2, 7, 3, 6, 12, 4, 9)
plot_dat_d_all_mad = d_all_mat_mad[new_order, new_order]

message("Saving D-vals plot (all groups) using MAD...")
pdf(here("output", "plots", "dvals_all_heat_mad.pdf"), height = 5, width=6)
print(
    ggplot(
        reshape2::melt(plot_dat_d_all_mad), 
        aes(x = Var1, y = Var2, fill = value)
    ) +
        geom_tile()+
        scale_fill_gradient(
            name = "Distance\n\n",
            low  = "black",
            high = "white", 
            na.value = "black"
        ) +
        coord_fixed() +
        theme_bw()+
        scale_x_discrete(expand = c(0, 0))+
        scale_y_discrete(expand = c(0, 0))+
        labs(x = NULL, y = NULL) +
        theme(
            axis.text.x= element_text(angle = 90),
            axis.ticks = element_blank(),
            axis.line = element_blank(),
            panel.grid.major = element_blank(),
            legend.position = "right",
            legend.spacing.x = unit(.25, "line"),
            legend.key.width=unit(1, "line"),
            legend.title = element_blank()
        )
)
dev.off()


### END OF CODE ###

