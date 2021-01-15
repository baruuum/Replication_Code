################################################################################
##
## Transform network plot (Fall 2020)
##
################################################################################

# Set up  ----------------------------------------------------------------------

# libraries
library("here")
library("data.table")
library("igraph")

# current time
message(
    paste0("Start running 07_Plot_transform.R on ", Sys.time(), "\n")
)

# current objects
obj_list = ls()

# create directory if non-existent
if (!dir.exists(here("output", "plots", "2020_transformed"))) {
    
    message("Creating directory to save results ...")
    dir.create(here("output", "plots", "2020_transformed"), recursive = T)
    
}


# source functions
source(here("R", "_plot_utils.R"))
Rcpp::sourceCpp(here("src", "procrustes.cpp"))

# helper function to rotate/scale after procrustes analysis
transform_proc = function(x, proc_res) {
    
    sweep(proc_res$sigma * x %*% proc_res$R, 2L, proc_res$t_vec, `+`)
    
}


# Read Data --------------------------------------------------------------------

# colors
col_tab = fread(here("data", "col_tab.csv"))

# get graphs
g19 = readRDS(here("data", "largest_comp_19.rds"))
g20 = readRDS(here("data", "largest_comp_20.rds"))

# get layouts
layout19 = readRDS(here("data", "layout19drl.rds"))
layout20 = readRDS(here("data", "layout20drl.rds"))

# create data for 2019
dat19 = data.table(
    V(g19)$color,
    layout19
) %>%
    setnames(c("color", "dim1", "dim2")) %>%
    merge(
        col_tab[, .(field, color)],
        by = "color",
        all.x = TRUE
    )

# data for 20
dat20 = data.table(
    V(g20)$color,
    layout20
) %>%
    setnames(c("color", "dim1", "dim2")) %>%
    merge(
        col_tab[, .(field, color)],
        by = "color",
        all.x = TRUE
    )



# Procrustes transform ---------------------------------------------------------


message("Performing Procrustean transform ...")

# mean data for 19
means19 = dat19[
    ,
    as.list(colMeans(.SD)),
    by = field,
    .SDcols = c("dim1", "dim2")
] %>%
    merge(
        col_tab[, .(field, color)],
        by = "field",
        all = T
    )

# mean data for 20
means20 = dat20[
    ,
    as.list(colMeans(.SD)),
    by = field,
    .SDcols = c("dim1", "dim2")
] %>%
    merge(
        col_tab[, .(field, color)],
        by = "field",
        all = T
    )

# check that order is the same
stopifnot(
    identical(means19$field, means20$field)
)

# procrustes transform of 20 onto 19 (target) positions

# field-means 20
X = as.matrix(means20[field != 7, .(dim1, dim2)])
# field-means 19
Xstar = as.matrix(means19[field != 7, .(dim1, dim2)])

# get transformation
proc_res = procrustes(X, Xstar)

# apply transformation to old layout
new_layout = transform_proc(layout20, proc_res)



# Plot results -----------------------------------------------------------------


message("Creating network plots ...")

# extract attributes
g_attr = igraph_attr_extr(
    g20,
    v_attr = c("name", "color", "type")
)


# load plot params
p_par = readRDS(here("data", "plot_params_19.rds"))

# create data
plot_df = igraph_plot_prep(
    ve_list = g_attr, 
    layout  = new_layout,
    v_pch   = ifelse(g_attr$v_dat$type, p_par$v_pch[1], p_par$v_pch[2]),
    v_fill  = scales::alpha(g_attr$v_dat$color, p_par$v_alpha),
    v_frame = p_par$v_frame,
    v_cex   = ifelse(g_attr$v_dat$type, p_par$v_cex[1], p_par$v_cex[2]),
    v_lwd   = p_par$v_lwd,
    e_lwd   = p_par$e_lwd,
    e_color = p_par$e_color
)

# create data for black background
plot_df_bbg = igraph_plot_prep(
    ve_list = g_attr, 
    layout  = new_layout,
    v_pch   = ifelse(g_attr$v_dat$type, p_par$v_pch[1], p_par$v_pch[2]),
    v_fill  = scales::alpha(g_attr$v_dat$color, p_par$v_alpha),
    v_frame = p_par$v_frame,
    v_cex   = ifelse(g_attr$v_dat$type, p_par$v_cex[1], p_par$v_cex[2]),
    v_lwd   = p_par$v_lwd,
    e_lwd   = p_par$e_lwd,
    e_color = p_par$e_color_bbg
)


# plot courses first (white background)
plot_network(
    plot_df, 
    plot_first = "courses",
    outfile = here("output", "plots", "2020_transformed", "c_first_20_wh_trans.pdf"), 
    bg_col = "white"
)

# plot courses first (black background)
plot_network(
    plot_df_bbg, 
    plot_first = "courses",
    outfile = here("output", "plots", "2020_transformed", "c_first_20_bl_trans.pdf"), 
    bg_col = "black"
)



message("Creating network plot with color-blind friendly colors...")

plot_df_cbf = igraph_plot_prep(
    ve_list = g_attr, 
    layout  = new_layout,
    v_pch   = ifelse(g_attr$v_dat$type, p_par$v_pch[1], p_par$v_pch[2]),
    v_fill  = scales::alpha(
        col_tab[match(g_attr$v_dat$color, color)]$color_cbf,
        p_par$v_alpha
    ),
    v_frame = p_par$v_frame,
    v_cex   = ifelse(g_attr$v_dat$type, p_par$v_cex[1], p_par$v_cex[2]),
    v_lwd   = p_par$v_lwd,
    e_lwd   = p_par$e_lwd,
    e_color = p_par$e_color
)

plot_df_cbf_bbg = igraph_plot_prep(
    ve_list = g_attr, 
    layout  = new_layout,
    v_pch   = ifelse(g_attr$v_dat$type, p_par$v_pch[1], p_par$v_pch[2]),
    v_fill  = scales::alpha(
        col_tab[match(g_attr$v_dat$color, color)]$color_cbf,
        p_par$v_alpha
    ),
    v_frame = p_par$v_frame,
    v_cex   = ifelse(g_attr$v_dat$type, p_par$v_cex[1], p_par$v_cex[2]),
    v_lwd   = p_par$v_lwd,
    e_lwd   = p_par$e_lwd,
    e_color = p_par$e_color_bbg
)


plot_network(
    plot_df_cbf, 
    plot_first = "courses",
    outfile = here("output", "plots", 
                   "2020_transformed", "c_first_20_wh_trans_cbf.pdf"), 
    bg_col = "white"
)

# plot courses first (black background)
plot_network(
    plot_df_cbf_bbg, 
    plot_first = "courses",
    outfile = here("output", "plots", 
                   "2020_transformed", "c_first_20_bl_trans_cbf.pdf"), 
    bg_col = "black"
)


plot_df_cbf = igraph_plot_prep(
    ve_list = g_attr, 
    layout  = new_layout,
    v_pch   = ifelse(g_attr$v_dat$type, p_par$v_pch[1], p_par$v_pch[2]),
    v_fill  = scales::alpha(
        col_tab[match(g_attr$v_dat$color, color)]$color_cbf2,
        p_par$v_alpha
    ),
    v_frame = p_par$v_frame,
    v_cex   = ifelse(g_attr$v_dat$type, p_par$v_cex[1], p_par$v_cex[2]),
    v_lwd   = p_par$v_lwd,
    e_lwd   = p_par$e_lwd,
    e_color = p_par$e_color
)

plot_df_cbf_bbg = igraph_plot_prep(
    ve_list = g_attr, 
    layout  = new_layout,
    v_pch   = ifelse(g_attr$v_dat$type, p_par$v_pch[1], p_par$v_pch[2]),
    v_fill  = scales::alpha(
        col_tab[match(g_attr$v_dat$color, color)]$color_cbf2,
        p_par$v_alpha
    ),
    v_frame = p_par$v_frame,
    v_cex   = ifelse(g_attr$v_dat$type, p_par$v_cex[1], p_par$v_cex[2]),
    v_lwd   = p_par$v_lwd,
    e_lwd   = p_par$e_lwd,
    e_color = p_par$e_color_bbg
)

plot_network(
    plot_df_cbf, 
    plot_first = "courses",
    outfile = here("output", "plots", 
                   "2020_transformed", "c_first_20_wh_trans_cbf2.pdf"), 
    bg_col = "white"
)

# plot courses first (black background)
plot_network(
    plot_df_cbf_bbg, 
    plot_first = "courses",
    outfile = here("output", "plots", 
                   "2020_transformed", "c_first_20_bl_trans_cbf2.pdf"), 
    bg_col = "black"
)




# Cleaning up ------------------------------------------------------------------

# clear objects (although package will remain attached)
rm(list = ls()[!(ls() %in% obj_list)])

message(
    paste0("Done running script on ", Sys.time(), "\n")
)


### END OF CODE ###