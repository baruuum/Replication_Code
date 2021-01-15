################################################################################
##
## Plotting Networks (Fall 20)
##
################################################################################

# Set up  ----------------------------------------------------------------------

# libraries
library("here")
library("data.table")
library("igraph")

# current time
message(
    paste0("\nStart running 06_Plot_20.R on ", Sys.time())
)

# source plotting functions
source(here("R", "_plot_utils.R"))

# current objects
obj_list = ls()

# create directory if non-existent
if (!dir.exists(here("output", "plots", "2020"))) {
    
    message("Creating directory to save results ...")
    dir.create(here("output", "plots", "2020"), recursive = T)
    
}



# Read Data --------------------------------------------------------------------

# read data from pajek into igraph
g = read_graph(
    here("raw_data", "fall2020", "p2_f2f_ith.net"), 
    format = "pajek"
)

# check edge-structure (is bipartite?)
bipart = bipartite_mapping(g)
stopifnot(bipart$res)

# assign bipartite structure to graph
V(g)$type = bipart$type

# data on students fileds
major = rio::import(
    here("raw_data", "fall2020", "f2020_f2f_ith_field6_studentnbr.xls"),
    setclass = "data.table"
) %>% 
    setnames(c("id", "field"))
major[, id := paste0("_", id)]

# add class ids
class_ids = V(g)$name[!(V(g)$name %in% major$id)]

# update major data
major = rbind(
    major,
    data.table(
        id = class_ids,
        field = 7
    )
)

# check
stopifnot(
    nrow(major) == vcount(g),
    uniqueN(major) == vcount(g)
)

# read colors
col_tab = fread(here("data", "col_tab.csv"))

# update major data with color
major = merge(
    major, 
    col_tab,
    by = "field",
    all = TRUE
)



# Prepare for plotting ---------------------------------------------------------

# get names
n_names = V(g)$name

# reorder major data according to vertex names
major = major[match(n_names, id)]

# check order
stopifnot(identical(n_names, major$id))

# vertex color attributes
V(g)$color = major$color

# decompose graph into components
decomp = decompose(g)
c_sizes = sapply(decomp, vcount)
# reduced graph
g_reduced = decomp[[which.max(c_sizes)]]

# check results
comps = components(g)
stopifnot(max(comps$csize) == vcount(g_reduced))

# save largest component
saveRDS(g_reduced, here("data", "largest_comp_20.rds"))



# Plot results -----------------------------------------------------------------

# seed for layout
set.seed(42)

message("Calculating layout for network plot ...")
layout_drl = layout_with_drl(g_reduced)
saveRDS(layout_drl, here("data", "layout20drl.rds"))

message("Creating network plots ...")

# extract attributes
g_attr = igraph_attr_extr(
    g_reduced,
    v_attr = c("name", "color", "type")
)


# load plot params
p_par = readRDS(here("data", "plot_params_19.rds"))

# create data
plot_df = igraph_plot_prep(
    ve_list = g_attr, 
    layout  = layout_drl,
    v_pch   = ifelse(g_attr$v_dat$type, p_par$v_pch[1], p_par$v_pch[2]),
    v_fill  = scales::alpha(g_attr$v_dat$color, p_par$v_alpha),
    v_frame = p_par$v_frame,
    v_cex   = ifelse(g_attr$v_dat$type, p_par$v_cex[1], p_par$v_cex[2]),
    v_lwd   = p_par$v_lwd,
    e_lwd   = p_par$e_lwd,
    e_color = p_par$e_color
)

# for black background (only edge color changes)
plot_df_bbg = igraph_plot_prep(
    ve_list = g_attr, 
    layout  = layout_drl,
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
    outfile = here("output", "plots", "2020", "c_first_20_wh.pdf"), 
    bg_col = "white"
)

# plot courses first (black background)
plot_network(
    plot_df_bbg, 
    plot_first = "courses",
    outfile = here("output", "plots", "2020", "c_first_20_bl.pdf"), 
    bg_col = "black"
)

# plot only students
plot_network(
    plot_df_bbg, 
    plot_first = "students",
    only_first = T,
    outfile = here("output", "plots", "2020", "s_only_20_bl.pdf"), 
    bg_col = "black"
)

# plot only courses
plot_network(
    plot_df_bbg, 
    plot_first = "courses",
    only_first = T,
    outfile = here("output", "plots", "2020", "c_only_20_bl.pdf"), 
    bg_col = "black"
)

# plot by field
for (ff in 1:6) {
    
    f_col = scales::alpha(col_tab[field == ff]$color, p_par$v_alpha)
    f_df = subset_plot_df(plot_df_bbg, "v_fill", f_col, keep_edges = "connected")
    
    plot_network(
        f_df, 
        plot_first = "students",
        only_first = TRUE,
        outfile = here(
            "output", 
            "plots", 
            "2020",
            paste0("field", ff, "_20_bl.pdf")
        ), 
        bg_col = "black"
    )
    
}


message("Creating network plot with color-blind friendly colors...")

plot_df_cbf = igraph_plot_prep(
    ve_list = g_attr, 
    layout  = layout_drl,
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
    layout  = layout_drl,
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
    outfile = here("output", "plots", "2020", "c_first_20_wh_cbf.pdf"), 
    bg_col = "white"
)

# plot courses first (black background)
plot_network(
    plot_df_cbf_bbg, 
    plot_first = "courses",
    outfile = here("output", "plots", "2020", "c_first_20_bl_cbf.pdf"), 
    bg_col = "black"
)


plot_df_cbf = igraph_plot_prep(
    ve_list = g_attr, 
    layout  = layout_drl,
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
    layout  = layout_drl,
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
    outfile = here("output", "plots", "2020", "c_first_20_wh_cbf2.pdf"), 
    bg_col = "white"
)

# plot courses first (black background)
plot_network(
    plot_df_cbf_bbg, 
    plot_first = "courses",
    outfile = here("output", "plots", "2020", "c_first_20_bl_cbf2.pdf"), 
    bg_col = "black"
)



# Cleaning up ------------------------------------------------------------------

# clear objects (although package will remain attached)
rm(list = ls()[!(ls() %in% obj_list)])

message(
    paste0("Done running script on ", Sys.time(), "\n")
)


### END OF CODE ###