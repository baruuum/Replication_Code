## Short tests on the plot functions

library("here")
library("igraph")
library("data.table")
source(here("R", "_plot_utils.R"))

a = igraph::sample_pa(20, directed = F)

b = igraph_attr_extr(a)

tmp_layout = layout_with_fr(a)
plot_df = igraph_plot_prep(
    ve_list = b,
    layout  = tmp_layout
)

plot_network(plot_df, outfile = here("tmp.pdf"))
