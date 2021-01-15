###############################################################################
##
## Graphical Illustration of Network Flows and Communities
##
## Creator     : Barum Park
## Last Update : 08/14/2020
##
###############################################################################


# Basic Setup -----------------------------------------------------------------

# load libraries
library(here)
library(igraph)
library(ggplot2)
library(data.table)
library(gridExtra)

# number of steps (for simulation)
n_steps = 10

# Generate hypothetcial graph
g = graph_from_literal(A +-+ B,
                       B -+ C,
                       C -+ D,
                       D -+ E,
                       E +-+ A,
                       F +- G,
                       G +- H,
                       H +- I,
                       I +- J,
                       J +- F,
                       A -+ F,
                       J +-+ H,
                       G -+ A)

message("\n\n\n***** Creating hypothetical graphs for illustrations *****\n")

#check whether graph is "strongly" connected
stopifnot(is.connected(g, mode = "strong"))

# origin of focus
org = "G"
# destinations of focus
dest = c("B", "J", "E", "I")

# number of vertices
v_count = vcount(g)

# adjacency matrix with diagonals
A = get.adjacency(g, sparse = F)
A_w_diag = A
diag(A_w_diag) = rep(1, v_count)

# create graph with diagonals
g_w = graph_from_adjacency_matrix(A_w_diag)

# Plots of graph and mobility network representations -------------------------

# clustering (infomap)
suppress_all(clust <- cluster_infomap(g_w))

# add clustering to vertex attributes
vertex_attr(g, "clust") = membership(clust)

# create plot (without self-loops for clarity)
g_plot = ggraph::ggraph(g, layout = "kk") +
    ggraph::geom_edge_link(
        arrow = arrow(
            length = unit(2, "mm"), 
            type   = "closed"
        ),
        start_cap = ggraph::circle(3, "mm"),
        end_cap = ggraph::circle(3, "mm"),
        width = .5,
        edge_colour = "grey50"
    ) +
    ggraph::geom_node_point(
        size = 5, 
        aes(col = factor(clust))
    ) +
    ggraph::geom_node_text(
        size = 5, 
        aes(label = name),
        nudge_y = .3,
        nudge_x = -.02
    ) +
    ggraph::theme_graph() +
    scale_color_manual(
        values = c("black", "grey50")
    ) +
    ggtitle("   (1) Network Graph") +
    fnb_theme + 
    theme(
        panel.border = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(rep(1, 4), unit = "mm")
    )
print(g_plot)

# plot adjacency matrix (with diagonals blocked)
A_df = reshape2::melt(A_w_diag) %>% data.table
A_df[, Var1 := factor(Var1, levels = LETTERS[v_count:1])]

A_plot = ggplot(A_df, aes(x = Var2, y = Var1)) + 
    geom_tile(
        aes(fill=factor(value)), 
        col = "white",
        size = 2
    ) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_fill_manual(
        values=c("lightgray","black")
    ) +
    coord_fixed() +
    ggtitle("(2) Mobility Table") + 
    fnb_theme + 
    theme(
        panel.border = element_blank(),
        plot.title = element_text(
            face = "bold",
            hjust = -0.1
        ),
        legend.position = "none",
        axis.title = element_blank()
    ) 



# Plots of visiting probabilities ---------------------------------------------

# Transition matrix (row-stochastic)
r_sums = rowSums(A_w_diag)
P = diag(1 / r_sums) %*% A_w_diag
rownames(P) = colnames(P)


# Prob. to reach destination from origin at different steps
org_id = match(org, rownames(P))
dest_id = match(dest, rownames(P))
n_dest = length(dest_id)

res_mat = matrix(NA, nrow=n_steps, ncol = n_dest)

p = P
x = rep(0, nrow(p))
x[org_id] = 1

for (z in 1:n_steps) {
   y = x %*% p 
   for (j in 1:n_dest) {
      res_mat[z,j] = y[dest_id[j]]
   }
   p = p %*% p
}

# create data.frame
p_df = data.table(res_mat)
names(p_df) = c(paste0(dest[1:n_dest]))
p_df$steps = rep(1:n_steps)

# melt into long format
p_df = melt(p_df, id.vars = "steps")
names(p_df) = c("Transitions", "Destination", "Probability")

# plot
v_plot = ggplot(
    p_df, 
    aes(
        x = Transitions, 
        y = Probability, 
        shape = Destination,
        linetype = Destination
    )
) + 
    geom_point(size = 2) +
    geom_line() + 
    labs(
        x = "Number of Steps",
        y = "Probability of Visiting Destination"
    ) +
    scale_x_continuous(
        breaks=1:n_steps
    ) +
    ggtitle(
        paste0(
            "(3) Visiting Probabilities from ",
            org,
            " to ",
            paste0(dest, collapse=", ")
        )
    ) +
    fnb_theme +
    theme(
        axis.title.y = element_blank(), 
        legend.position = "right",
        legend.direction = "vertical",        
        plot.title = element_text(face = "bold")
    )



# Plot of expected number of visits -------------------------------------------

e_df = data.table(apply(res_mat, 2L, cumsum))
names(e_df) = c(paste0(dest[1:n_dest]))
e_df$steps = rep(1:n_steps)
e_df = melt(e_df, id.vars = "steps")
names(e_df) = c("Transitions", "Destination", "Visits")

e_plot = ggplot(
    e_df, 
    aes(
        x = Transitions, 
        y = Visits, 
        shape = Destination,
        linetype = Destination
    )
) + 
    geom_point(size = 2) +
    geom_line() + 
    theme_bw() +
    labs(
        x = "Number of Steps",
        y = "Expected Number of Visits"
    ) +
    scale_x_continuous(breaks=1:n_steps)+
    ggtitle(
        paste0(
            "(4) Expected Number of Visits from ",
            org,
            " to ",
            paste0(dest, collapse = ", ")
        )
    ) +
    fnb_theme +
    theme(
        axis.title.y = element_blank(), 
        legend.position = "right",
        legend.direction = "vertical",
        plot.title = element_text(face = "bold")
    )


# Putting all plots together ------------------------------------------------------

message("Plotting network, mobility table, and visiting probs (Figure 1) ...")

pdf(here("output", "figures", "Figure_1.pdf"),
    width = 10, height = 8)
print(
    cowplot::plot_grid(
        g_plot, 
        A_plot, 
        v_plot, 
        e_plot, 
        nrow = 2, 
        align = "hv", 
        axis = "tbr")
)
dev.off()

message(paste0("File Path : ", here("output", "figures", "Figure_1.pdf")))
message(paste0("Data Path : No data created"))
         