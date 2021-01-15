################################################################################
##
## Plotting Networks (Fall 19)
##
################################################################################

# Set up -----------------------------------------------------------------------

# libraries
library("here")
library("data.table")
library("igraph")

# current time
message(
    paste0("\nStart running 05_Plot_19.R on ", Sys.time())
)

# current objects
obj_list = ls()

# source plotting functions
source(here("R", "_plot_utils.R"))

# create directory if non-existent
if (!dir.exists(here("output", "plots", "2019"))) {
    
    message("Creating directory to save results ...")
    dir.create(here("output", "plots", "2019"), recursive = T)
    
}



# Read data --------------------------------------------------------------------

# read data from pajek into igraph
g = read_graph(
    here("raw_data", "fall2019", "university.net"), 
    format = "pajek"
)

# check edge-structure (is bipartite?)
bipart = bipartite_mapping(g)
stopifnot(bipart$res)

# assign bipartite structure to graph
V(g)$type = bipart$type

# data on students fields
major = rio::import(
    here("raw_data", "fall2019", "f2019_uni_field6_studentnbr.xls"),
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



# Colors codes -----------------------------------------------------------------


message("Define plotting colors ...")
# 1 humanities, arts, and design
# 2 social sciences
# 3 STEM
# 4 multidisciplinary, mixed majors (UGs w/ two or more majors that don’t fall in same HAD, SS, or STEM group – e.g., Bio and English would be mixed; Econ and Gov would be social sciences)
# 5 undeclared, non-degree continuing education (there are very few of these in the Ithaca student group)
# 6 business and law

# rgb (color-blind friendly)
rgb_list = list(
    humanities  = c(253, 219, 199),
    socscience  = c(33 , 102, 172),
    stem        = c(239, 138, 98),
    multiple    = c(178, 24 , 43),
    undeclared  = c(103, 167, 207),
    businesslaw = c(209, 229, 240),
    courses     = c(247, 247, 247)
)

# color table
col_tab = data.table(
    field = seq_along(rgb_list),
    color = c(
        "yellow",
        "blue",
        "orange",
        "#C21E56", # rose red
        "green",
        "#40E0D0", # turquoise
        "white"
    ),
    color_cbf = sapply(
        rgb_list, function(w) {
            rgb(
                w[1], w[2], w[3], 
                maxColorValue = 255
            )
        }
    ),
    color_cbf2 = c(
        "#E69F00", 
        "#56B4E9", 
        "#009E73", 
        "#F0E442", 
        "#0072B2", 
        "#CC79A7",
        "white"
    )
)

# save color mapping
rio::export(col_tab, here("data", "col_tab.csv"))

# update major data with color
major = merge(
    major, 
    col_tab,
    by = "field",
    all = TRUE
)



# Preparing to plot ------------------------------------------------------------

# get names
n_names = V(g)$name

# reorder major data according to vertex names
major = major[match(n_names, id)]

# check order
stopifnot(identical(n_names, major$id))

# vertex attribute (shape/color)
V(g)$shape = ifelse(V(g)$type, "circle", "square")
V(g)$color = major$color

# decompose graph into components
decomp = decompose(g)
c_sizes = sapply(decomp, vcount)
# reduced graph
g_reduced = decomp[[which.max(c_sizes)]]

# check results
comps = components(g)
stopifnot(max(comps$csize) == vcount(g_reduced))

# save graph
saveRDS(g_reduced, here("data", "largest_comp_19.rds"))



# Generating plots -------------------------------------------------------------

# seed for layout
set.seed(42)

message("Calculating layout for network plot ...")
layout_drl = layout_with_drl(g_reduced)
saveRDS(layout_drl, here("data", "layout19drl.rds"))

message("Creating network plots ...")

g_attr = igraph_attr_extr(
    g_reduced,
    v_attr = c("name", "color", "type")
)

# color parameters
v_alpha = .6
vf_alpha = .6
v_cex = c(.6, .9)
v_pch = c(21, 22)
v_frame = scales::alpha("black", vf_alpha)
v_fill  = scales::alpha(g_attr$v_dat$color, v_alpha)
v_lwd = .1

e_alpha = .3
e_color = scales::alpha("grey40", e_alpha)
e_color_bbg = scales::alpha("grey", e_alpha) # color for black background
e_lwd = .3

# save parameters
saveRDS(
    list(
        v_alpha = .6,
        vf_alpha = .6,
        v_cex = c(.6, .9),
        v_pch = c(21, 22),
        v_frame = scales::alpha("black", vf_alpha),
        v_lwd = .1,
        e_alpha = .3,
        e_color = scales::alpha("grey40", e_alpha),
        e_color_bbg = scales::alpha("grey", e_alpha),
        e_lwd = .3
    ),
    here("data", "plot_params_19.rds")
)

# df for white background
plot_df = igraph_plot_prep(
    ve_list = g_attr, 
    layout  = layout_drl,
    v_pch   = ifelse(g_attr$v_dat$type, v_pch[1], v_pch[2]),
    v_fill  = scales::alpha(g_attr$v_dat$color, v_alpha),
    v_frame = v_frame,
    v_cex   = ifelse(g_attr$v_dat$type, v_cex[1], v_cex[2]),
    v_lwd   = v_lwd,
    e_lwd   = e_lwd,
    e_color = e_color
)

# df for black background
plot_df_bbg = igraph_plot_prep(
    ve_list = g_attr, 
    layout  = layout_drl,
    v_pch   = ifelse(g_attr$v_dat$type, v_pch[1], v_pch[2]),
    v_fill  = scales::alpha(g_attr$v_dat$color, v_alpha),
    v_frame = v_frame,
    v_cex   = ifelse(g_attr$v_dat$type, v_cex[1], v_cex[2]),
    v_lwd   = v_lwd,
    e_lwd   = e_lwd,
    e_color = e_color_bbg
)

# plot courses first (white background)
plot_network(
    plot_df, 
    plot_first = "courses",
    outfile = here("output", "plots", "2019", "c_first_19_wh.pdf"), 
    bg_col = "white"
)

# plot courses first (black background)
plot_network(
    plot_df_bbg, 
    plot_first = "courses",
    outfile = here("output", "plots", "2019", "c_first_19_bl.pdf"), 
    bg_col = "black"
)

# plot only students
plot_network(
    plot_df_bbg, 
    plot_first = "students",
    only_first = T,
    outfile = here("output", "plots", "2019", "s_only_19_bl.pdf"), 
    bg_col = "black"
)

# plot only courses
plot_network(
    plot_df_bbg, 
    plot_first = "courses",
    only_first = T,
    outfile = here("output", "plots", "2019", "c_only_19_bl.pdf"), 
    bg_col = "black"
)

# plot by field
for (ff in 1:6) {
    
    f_col = scales::alpha(col_tab[field == ff]$color, v_alpha)
    f_df = subset_plot_df(plot_df_bbg, "v_fill", f_col, keep_edges = "connected")

    plot_network(
        f_df, 
        plot_first = "students",
        only_first = TRUE,
        outfile = here(
            "output", 
            "plots", 
            "2019",
            paste0("field", ff, "_19_bl.pdf")
            ), 
        bg_col = "black"
    )
    
}


message("Creating network plot with color-blind friendly colors...")

plot_df_cbf = igraph_plot_prep(
    ve_list = g_attr, 
    layout  = layout_drl,
    v_pch   = ifelse(g_attr$v_dat$type, v_pch[1], v_pch[2]),
    v_fill  = scales::alpha(
        col_tab[match(g_attr$v_dat$color, color)]$color_cbf,
        v_alpha
    ),
    v_frame = v_frame,
    v_cex   = ifelse(g_attr$v_dat$type, v_cex[1], v_cex[2]),
    v_lwd   = v_lwd,
    e_lwd   = e_lwd,
    e_color = e_color
)

plot_df_cbf_bbg = igraph_plot_prep(
    ve_list = g_attr, 
    layout  = layout_drl,
    v_pch   = ifelse(g_attr$v_dat$type, v_pch[1], v_pch[2]),
    v_fill  = scales::alpha(
        col_tab[match(g_attr$v_dat$color, color)]$color_cbf,
        v_alpha
    ),
    v_frame = v_frame,
    v_cex   = ifelse(g_attr$v_dat$type, v_cex[1], v_cex[2]),
    v_lwd   = v_lwd,
    e_lwd   = e_lwd,
    e_color = e_color_bbg
)

plot_network(
    plot_df_cbf, 
    plot_first = "courses",
    outfile = here("output", "plots", "2019", "c_first_19_wh_cbf.pdf"), 
    bg_col = "white"
)

# plot courses first (black background)
plot_network(
    plot_df_cbf_bbg, 
    plot_first = "courses",
    outfile = here("output", "plots", "2019", "c_first_19_bl_cbf.pdf"), 
    bg_col = "black"
)



plot_df_cbf = igraph_plot_prep(
    ve_list = g_attr, 
    layout  = layout_drl,
    v_pch   = ifelse(g_attr$v_dat$type, v_pch[1], v_pch[2]),
    v_fill  = scales::alpha(
        col_tab[match(g_attr$v_dat$color, color)]$color_cbf2,
        v_alpha
    ),
    v_frame = v_frame,
    v_cex   = ifelse(g_attr$v_dat$type, v_cex[1], v_cex[2]),
    v_lwd   = v_lwd,
    e_lwd   = e_lwd,
    e_color = e_color
)

plot_df_cbf_bbg = igraph_plot_prep(
    ve_list = g_attr, 
    layout  = layout_drl,
    v_pch   = ifelse(g_attr$v_dat$type, v_pch[1], v_pch[2]),
    v_fill  = scales::alpha(
        col_tab[match(g_attr$v_dat$color, color)]$color_cbf2,
        v_alpha
    ),
    v_frame = v_frame,
    v_cex   = ifelse(g_attr$v_dat$type, v_cex[1], v_cex[2]),
    v_lwd   = v_lwd,
    e_lwd   = e_lwd,
    e_color = e_color_bbg
)


plot_network(
    plot_df_cbf, 
    plot_first = "courses",
    outfile = here("output", "plots", "2019", "c_first_19_wh_cbf2.pdf"), 
    bg_col = "white"
)

# plot courses first (black background)
plot_network(
    plot_df_cbf_bbg, 
    plot_first = "courses",
    outfile = here("output", "plots", "2019", "c_first_19_bl_cbf2.pdf"), 
    bg_col = "black"
)



# Cleaning up ------------------------------------------------------------------

# clear objects (although package will remain attached)
rm(list = ls()[!(ls() %in% obj_list)])

message(
    paste0("Done running script on ", Sys.time(), "\n")
)



### END OF CODE ###