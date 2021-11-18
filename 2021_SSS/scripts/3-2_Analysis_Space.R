## ------------------------------------------------------------------
## Setup
## ------------------------------------------------------------------

library("here")
library("data.table")
library("rstan")
library("ggplot2")
library("mclust")
library("plotly")

# source functions
source(here("scripts", "0_Functions.R"))

## ------------------------------------------------------------------
## Helper functions
## ------------------------------------------------------------------

# eigenvalues (in proportion)
evals_pos = function(w) {
    
    x = svd(w)$d^2
    return(x/sum(x))
    
}

# approximate overlap in distribution between groups based on 2D-kernel density
ovl = function(df, var, g1,g2) {
    
    z = df[get(var) %in% c(g1, g2), list(dim1, dim2, get(var))]
    m1 = range(z$dim1)
    m2 = range(z$dim2)
    
    common_h =  c(MASS::bandwidth.nrd(z$dim1), MASS::bandwidth.nrd(z$dim2))
    d1 = MASS::kde2d(
        z[V3 == g1]$dim1,
        z[V3 == g1]$dim2,
        n = 25, 
        h = common_h,
        lims=c(m1[1], m1[2], m2[1], m2[2])
    )
    
    d2 = MASS::kde2d(
        z[V3 == g2]$dim1,
        z[V3 == g2]$dim2,
        n = 25, 
        h = common_h,
        lims=c(m1[1], m1[2], m2[1],m2[2])
    )
    
    sizes = diff(m1) / 25 * diff(m2) / 25
    
    sum(pmin(d2$z, d1$z)) * sizes
    
}


## ------------------------------------------------------------------
## Load data
## ------------------------------------------------------------------

# get posterior samples
post_samps = fread(here("data", "post_samples.csv"))

# get params of interest
samps = post_samps[
    , grepl("(pos\\_|gamma\\[|alpha)", names(post_samps)) & !grepl("star", names(post_samps)), 
    with = F
]

# get item summary
item_sum = fread(here("data", "item_sum_w_labs.csv"))

# get individual summary
ind_sum = fread(here("data", "ind_sum.csv"))

# get predictor data
p_dat = merge(
    ind_sum, 
    readRDS(here("data", "predictors.rds")), 
    by.x = "old_id", 
    by.y = "id") 
setorder(p_dat, new_id)


# get individual positions
pos_i = gen_positions(samps, "i")
n_ind = nrow(pos_i[[1]])

# get group positions
pos_j = gen_positions(samps, "j")
n_items = nrow(pos_j[[1]])


## ------------------------------------------------------------------
## 3D Space
## ------------------------------------------------------------------

message("Creating 3d plot ...")

# check eigen values
m_pos_j = apply(simplify2array(pos_j), 1:2, median)
message(
    paste0(
        "Variance explained by the first two dimensions for groups: ",
        round(1 - evals_pos(m_pos_j)[3], 5)
    )
)

m_pos_i = apply(simplify2array(pos_i), 1:2, median)
message(
    paste0(
        "Variance explained by the first two dimensions for individuals: ",
        round(1 - evals_pos(m_pos_i)[3], 5)
    )
)

# rotate positions
rotation = rotate_pos(m_pos_i, return_matrix = T)
m_pos_i = rotation$new_pos
m_pos_j = sweep(m_pos_j, 2, rotation$means) %*% rotation$R

# flip y-axis and x-axis
m_pos_i[, 2:3] = -1.0 * m_pos_i[, 2:3]
m_pos_j[, 2:3] = -1.0 * m_pos_j[, 2:3]

# make into data.table
df_j = data.table(m_pos_j) 
df_i = data.table(m_pos_i) 
setnames(df_j, paste0("dim", 1:ncol(df_j)))
setnames(df_i, paste0("dim", 1:ncol(df_j)))

# calculate scale of plot
min_max = apply(rbind(m_pos_i, m_pos_j), 2, function(w) c(min(w), max(w)))

# grid points
g_points = 4
x = seq(min_max[1, 1], min_max[2, 1], length.out = g_points)
y = seq(min_max[1, 2], min_max[2, 2], length.out = g_points)
z_points = seq(min_max[1, 3], min_max[2, 3], length.out = g_points) 

# make plane coordinates
for (g in 1:g_points)
    assign(
        paste0("g", g), 
        matrix(z_points[g], nrow = g_points, ncol = g_points),
        envir = .GlobalEnv
    )

pos_plot = plot_ly(
    df_j,
    x = ~ dim1,
    y = ~ dim2,
    z = ~ dim3,
    text = item_sum$short_labs,
    color = I("black")
) %>%
    add_text(type = "scatter3d",
             mode = "text",
             textfont = list(size = 10)) %>%
    add_trace(
        type = "scatter3d",
        mode = "markers",
        marker = list(
            size = 4
        )
    ) %>%
    add_trace(
        type = "scatter3d",
        mode = "markers",
        data = df_i,
        x = ~ dim1,
        y = ~ dim2,
        z = ~ dim3,
        marker = list(
            size = 2.5,
            color = I("black")
        ),
        opacity = .35,
        inherit = FALSE
    ) %>%
    add_surface(
        x = ~x, y = ~y, z = ~g1,
        opacity = .4,
        color = I("black"),
        inherit = F
    ) %>%
    add_surface(
        x = ~x, y = ~y, z = ~g2,
        opacity = .4,
        color = I("black"),
        inherit = F
    ) %>%
    add_surface(
        x = ~x, y = ~y, z = ~g3,
        opacity=.4,
        color = I("black"),
        inherit = F
    ) %>%
    add_surface(
        x = ~x, y = ~y, z = ~g4,
        opacity=.4,
        color = I("black"),
        inherit = F
    ) %>%
    layout(
        showlegend = F,
        scene = list(
            xaxis = list(
               title = "Dimension 1",
               titlefont = list(size = 12),
               zeroline = FALSE,
               showticklabels = FALSE
            ),
            yaxis = list(
                title = "Dimension 2",
                titlefont = list(size = 12),
                zeroline = FALSE,
                showticklabels = FALSE
            ),
            zaxis = list(
                title = "",
                zeroline = FALSE,
                showticklabels = FALSE
            )
        )
    ) %>%
    hide_colorbar()

for (vv in 1:nrow(df_j)) {
    
    dd = rbind(df_j[vv], df_j[vv][, dim3 := min_max[1, 3] - .2])
    pos_plot = pos_plot %>%
        add_trace(data = dd,
                  type = "scatter3d",
                  mode = "lines",
                  x = ~ dim1,
                  y = ~ dim2,
                  z = ~ dim3,
                  line = list(color = I("black"), width = 2),
                  inherit = F)
}
# pos_plot

# save plotting data
saveRDS(pos_plot, here("output", "plots", "space3d.rds"))



## ------------------------------------------------------------------
## 2D plot
## ------------------------------------------------------------------

message("Plotting the first two dimensions only ...")

gamma = samps[, grepl("gamma\\[", names(samps)), with = F]
greg = gamma[, lapply(.SD, function(w) exp(w + samps[, alpha]))]
med_greg = apply(greg, 2, median)
df_i_g = cbind(df_i, med_greg)

pdf(here("output", "plots", "dim12.pdf"), width = 5, height = 5.5)
print(
    ggplot(
        df_j[, c("dim1", "dim2")],
        aes(x = dim1, y = dim2)
    ) +
    geom_point(data = df_i[, .(dim1, dim2, med_greg)],
               aes(size = med_greg, alpha = med_greg),
               shape = 21) +
    geom_text(
        label = item_sum$short_labs,
        fontface = 2,
        size = 4
    ) +
    scale_alpha(
        name = expression(
            paste("Individual Gregariousness (", gamma[i], ")")
        ),
        range= c(.2,.8)) + 
    scale_size_continuous(
        name = expression(
            paste("Individual Gregariousness (",gamma[i],")")
        )
    ) +
    labs(x = "Dimension 1", y = "Dimension 2") +
    scale_x_continuous(
        breaks = seq(-3, 2, 1),
        lim = c(-3.5, 2.5)
    ) +
    scale_y_continuous(
        breaks = seq(-2, 3, 1),
        lim = c(-2.5, 3.5)
    ) +
    theme_bw() +
    theme(
        panel.grid = element_blank(),
        plot.margin = unit(c(.1, .1, 1.5 , .1), "cm"),
        legend.position = c(.6, -.15),
        legend.direction = "horizontal",
        legend.title = element_text(size = 10),
        legend.title.align = 0,
        legend.key.size = unit(0.75, "line")
    )
)
dev.off()


## ------------------------------------------------------------------
##  Overlap
## ------------------------------------------------------------------

message("Calculating overlap between individual groups ...")

## ind dists
sub_df = df_i
sub_df[, id := 1:.N]
sub_df = merge(sub_df, p_dat, by.x = "id", by.y = "new_id")
# sub_df = cbind(sub_df, g_dat)


message("Estimated Overlap ...")
message(
    paste0(
        "Income (highest 30% vs. lowest 30%) : ",
        round(ovl(sub_df, "inc_fact", "lowest", "highest"), 5)
    )
)
message(
    paste0(
        "Race (Black vs. White) : ",
        round(ovl(sub_df, "race_fact", "White", "Black"), 5)
    )
)
message(
    paste0(
        "Ideology (liberal vs. conservative) : ",
        round(ovl(sub_df, "sym_3", "Liberal", "Conservative"), 5)
    )
)
message(
    paste0(
        "Religiosity (Regularly vs. Never/Rarely)",
        round(ovl(sub_df, "att_fact", "Regularly", "Never/Rarely"), 5)
    )
)

message("\nPlotting respondents' distribution ...")
pdf(here("output", "plots", "dim12_race.pdf"), width = 6, height = 6)
print(
    ggplot(
            sub_df[race_fact %in% c("Other", "Hispanic") == F], 
            aes(x=dim1, y =dim2)
    ) +
    geom_density_2d(
        aes(col = race_fact), 
        size = .5, 
        alpha = .7
    ) +
    scale_color_manual(
        name = "Race",
        values =c("Black" = "black", "White" = "grey50")
    ) +
    labs(x = "Dimension 1", y = "Dimension 2") +
    theme_bw() +
    lims(x = c(-2.5,2), y = c(-2, 2)) +
    theme(panel.grid=element_blank(), legend.position = "bottom")
)
dev.off()


pdf(here("output", "plots", "dim12_ideo.pdf"), width = 6, height = 6)
print(
    ggplot(
            sub_df[!is.na(sym_3) & sym_3 != "Moderate"], 
            aes(x=dim1, y =dim2)
    ) +
    geom_density_2d(aes(col = sym_3), size = .5, alpha = .7) +
    scale_color_manual(
        name = "Ideological Self-Identification",
        values = c("Liberal" = "black", "Conservative" = "grey50")
    ) +
    labs(x = "Dimension 1", y = "Dimension 2") +
    theme_bw() +
    lims(x = c(-2.5, 2), y = c(-2, 2))+
    theme(panel.grid = element_blank(), legend.position = "bottom")
)
dev.off()

# reorder religiosity
sub_df[
    , att_fact := factor(
        att_fact,
        levels = levels(att_fact)[c(3,2,1)])
]

pdf(here("output", "plots", "dim12_rel.pdf"), width = 6, height = 6)
print(
    ggplot(
        sub_df[!is.na(att_fact) & att_fact != "At least once a month"], 
        aes(x = dim1, y = dim2)
    ) +
    geom_density_2d(aes(col = att_fact), size = .5, alpha = .7) +
    scale_color_manual(
        name = "Religiosity",
        values = c(
            "Never/Rarely" = "black",
            "Regularly" = "grey50"
        )
    ) +
    labs(x = "Dimension 1", y = "Dimension 2") +
    theme_bw() +
    lims(x = c(-2.5, 2), y = c(-2, 2)) +
    theme(panel.grid = element_blank(), legend.position = "bottom")
)
dev.off()

# reorder income
sub_df[, inc_fact:= factor(
    inc_fact,
    labels = c("Lowest 30%", "Middle 30%", "Highest 30%"))
]

pdf(here("output", "plots", "dim12_inc.pdf"), width = 6, height = 6)
print(
    ggplot(
        sub_df[!is.na(inc_fact) & inc_fact != "Middle 30%"], 
        aes(x = dim1, y = dim2)
) +
    geom_density_2d(aes(color = inc_fact), size = .5, alpha = .7) +
    scale_color_manual(
        name = "Family Income",
        values = c("Lowest 30%" = "grey50",
                   "Highest 30%" = "black")
    ) +
    labs(x = "Dimension 1", y = "Dimension 2") +
    theme_bw() +
    lims(x = c(-2.5, 2), y = c(-2, 2)) +
    theme(panel.grid = element_blank(), legend.position = "bottom")
)
dev.off()

rm(list = ls())


### END OF CODE ###
