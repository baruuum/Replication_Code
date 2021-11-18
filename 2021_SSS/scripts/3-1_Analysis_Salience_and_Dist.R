## ------------------------------------------------------------------
## Setup
## ------------------------------------------------------------------

library("here")
library("data.table")
library("rstan")
library("ggplot2")
library("mclust")

# source functions
source(here("scripts", "0_Functions.R"))



## ------------------------------------------------------------------
## File specific helper functions
## ------------------------------------------------------------------

# function to draw blocks in plotting
gen_blocks = function(a, b) {
    a = a - .5; b = b + .5
    rbind(
        c(a, a, a, b),
        c(a, b, a, a),
        c(a, b, b, b),
        c(b, b, a, b)
    )
}

# function to print data.tables of distances
print_dt_lab = function(dat, lab) {
    
    message(
        paste0(
            capture.output(
                dat[lab1 == lab | lab2 == lab]
            ), 
            collapse = "\n"
        )
    )
    
}



## ------------------------------------------------------------------
## Load data
## ------------------------------------------------------------------

message("Loading data ...")
post_samps = fread(here("data", "post_samples.csv"))
ind_sum = fread(here("data", "ind_sum.csv"))
item_sum = fread(here("data", "item_sum_w_labs.csv"))

# get params of interest
samps = post_samps[
    , grepl("(pos\\_|gamma\\[|alpha)", names(post_samps)) & !grepl("star", names(post_samps)), 
    with = F
]

# get predictors
p_dat = merge(
    ind_sum, 
    readRDS(here("data", "predictors.rds")), 
    by.x = "old_id", 
    by.y = "id") 
setorder(p_dat, new_id)

# quants to plot
quants = c(.025, .05, .5, .95, .975)

# mapping between predictors and groups
h_vars_sum = list(
    c("White", "race_fact", "White"),
    c("Black", "race_fact", "Black"),
    c("Hispanic", "race_fact", "Hispanic"),
    c("Unemp", "unemp", 1),
    c("Rel. Reg", "att_fact", "Regularly"),
    c("Rel. Never", "att_fact", "Never/Rarely"),
    c("Lib", "sym_3", "Liberal"),
    c("Cons", "sym_3", "Conservative")
)



## ------------------------------------------------------------------
## Salience
## ------------------------------------------------------------------

pos_i = gen_positions(samps, "i")
pos_j = gen_positions(samps, "j")
pos_list = mapply(list, pos_i, pos_j, SIMPLIFY = F)

message("Calculating salience measures ...")
sal_list = lapply(
    pos_list,
    function (w) {
        
        dd = vec_pdist(w[[1]],w[[2]])
        
        dist_mat = sapply(h_vars_sum, function(vv) {
            
            # individuals belonging to ingroup
            ig_ind = p_dat[get(vv[2]) == vv[3], new_id]
            
            # individuals belonging to outgroup
            og_ind = 1:nrow(p_dat)
            og_ind = og_ind[og_ind %in% ig_ind == F]
            
            # item
            ig_item = item_sum[short_labs == vv[1], item_no]

            # ingroup distance
            ig_dist = median(dd[ig_ind, ig_item])
            
            # outgroup distance
            og_dist = median(dd[og_ind, ig_item])
            
            # difference
            dist_diff = ig_dist - og_dist
            
            return(c(ig_dist, og_dist, dist_diff))
            
        })
        colnames(dist_mat) = sapply(h_vars_sum, `[`, 1L)
        
        return(dist_mat)
        
    }
)


sal_dat = as.data.table(do.call(rbind, sal_list))
sal_dat[, draw := rep(1:length(sal_list), each = 3L)]
sal_dat[, type := rep(c("ingroup", "outgroup", "diff"), length(sal_list))]

sal_long = melt(sal_dat, id.vars = c("type", "draw")) 
sal_long = sal_long[
    , {
        x = as.list(quantile(value, quants)) 
        names(x) = paste0("q", quants) 
        x
    }, 
    by = list(type, variable)
]

# reorder labels for plots
g_order = levels(sal_long$variable)
g_order = g_order[c(1:4, 6,5, 7,8)]
sal_long[, variable := factor(variable, levels = g_order)]

message("Plotting salience measures ...")
pdf(here("output", "plots", "salience.pdf"), width = 6, height = 3)
print(
    ggplot(sal_long[type == "diff"], aes(x = variable)) + 
    geom_hline(yintercept = 0, col = "grey50", linetype = 2) + 
    geom_linerange(aes(ymin = q0.025, ymax = q0.975), color = "black", size = .25) +
    geom_linerange(aes(ymin = q0.05, ymax = q0.95), color = "black", size = .75) + 
    geom_point(aes(y = q0.5), size = 2) +
    theme_bw() +
    labs(x = "", y = "Relative Distance to Own Group") + 
    theme(
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()
    ) 
)
dev.off()


## ------------------------------------------------------------------
## Pairwise distances between groups
## ------------------------------------------------------------------

message("Calculating between-group distances ...")
# distances
j_dists = lapply(pos_j, function (w) vec_pdist(w,w))

# median distances
med_j_dists = apply(simplify2array(j_dists), 1:2, median)
colnames(med_j_dists) = rownames(med_j_dists) = item_sum$short_labs

# whites
message("Median distance from 'White' to other racial groups is")
message(
    paste0(
        capture.output(
            med_j_dists["White", c("Black", "Hispanic", "Asian")]
        ), collapse = "\n"
    )    
)

message(
    paste0(
        "Median distance between 'Rel. Reg' and 'Rel. Never' is ",
        round(med_j_dists[11,12], 5)
    )
)


## check clustering with model-based BIC results 
message("Clustering group-positions using GMM via EM ...")
clust_dat = apply(simplify2array(pos_j), 1:2, median)
z = mclustBIC(clust_dat)
y = mclust::Mclust(clust_dat, x = z)
m_class = y$classification
for (cc in 1:max(m_class))
    message(
        paste0(
            "cluster ", cc, ": ", 
            paste0(item_sum[m_class == cc]$short_labs, collapse = ", ")
        )
    )
    
# reorder distance matrix according to clustering
new_order = c("2nd. Home", "White", "Rel. Reg", "Cons",
              "Asian", "Hispanic",
              "Rel. Never", "Lib", "Gay/Les",
              "Black", "Unemp", "Cohab", "Prison")
reordered_dists = med_j_dists[new_order, new_order]
# generate df
dist_df = setDT(reshape2::melt(reordered_dists))

# lines for clusters
new_lines = as.data.table(
    rbind(
        gen_blocks(1, 4),
        gen_blocks(5, 6),
        gen_blocks(7, 9),
        gen_blocks(10, 13)
    )
)
setnames(new_lines, c("x", "xend", "y", "yend"))

# plot heatmap
pdf(here("output", "plots", "dist_heat.pdf"), height=5, width=6)
print(
    ggplot(dist_df, aes(x = Var1, y = Var2, fill = value)) + 
    geom_tile()+
    scale_fill_gradient(
        name = "Distance\n\n",
        low="black",
        high="white", 
        na.value="black"
    ) +
    geom_segment(
        data = new_lines, 
        aes(x = x, xend = xend,y = y,yend = yend),
        col = "white",
        size = .5,
        inherit.aes = F
    ) +
    coord_fixed() +
    theme_bw()+
    scale_x_discrete(expand = c(0, 0))+
    scale_y_discrete(expand = c(0, 0))+
    labs(x = NULL, y = NULL) +
    theme(
        axis.text.x = element_text(angle = 90),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "right",
        legend.spacing.x = unit(.25, "line"),
        legend.key.width = unit(1, "line"),
        legend.title = element_blank()
    )
)
dev.off()

# get summaries of distances
dist_draws = lapply(
    seq_along(j_dists), 
    function(w) {
    
    long_dat = as.data.table(reshape2::melt(j_dists[[w]]))[Var1 != Var2]
    indx = !duplicated(t(apply(long_dat, 1, sort)))
    long_dat = long_dat[indx] 
    long_dat[, `:=`(order = order(value), draw = w)]
    
    return(long_dat)
    
}) 
dist_draws = do.call(rbind, dist_draws)

# get quantiles
dist_quants = dist_draws[
    , as.list(quantile(value, c(.025, .5, .975))), 
    by = list(Var1, Var2)
]

# merge with labels
dist_quants = merge(
    dist_quants, 
    item_sum[, c("item_no", "short_labs")], 
    by.x = "Var1", 
    by.y = "item_no", 
    all.x = TRUE
)
setnames(dist_quants, "short_labs","lab1")
dist_quants = merge(
    dist_quants, 
    item_sum[, c("item_no", "short_labs")], 
    by.x = "Var2", 
    by.y = "item_no", 
    all.x = T
)
dist_quants[, c("Var1","Var2") := list(NULL,NULL)]
setnames(dist_quants, "short_labs", "lab2")
setnames(dist_quants, c("2.5%", "50%", "97.5%"), c("lo", "med", "hi"))

# order by median
dist_quants = dist_quants[order(med)]

# print out distances for selected groups
for (l in c("Lib", "Cons", "Rel. Never", "White", "Black")) {
    
    message(paste0("\n\n Distances for ", l, ": \n"))
    print_dt_lab(dist_quants, l)
    
}

rm(list = ls())



### END OF CODE ###

