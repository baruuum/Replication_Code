###############################################################################
## 
## Matching modules over there period data
##
## Creator     : Barum Park
## Last Update : 02/25/2020
##
###############################################################################


## Basic Configurations -------------------------------------------------------

library(igraph)
library(data.table)
library(ggplot2)
library(gridExtra)

if (!exists("p_vec"))
    stop("cannot find p_vec")

message("\n\n\n***** Matching and plotting modules over three periods *****\n")

## Read data & calculate overlap-----------------------------------------------

message("Reading in modules ...")

# read detected modules
mod_list = lapply(p_vec, function(w) 
    {
        readRDS(
            here("output", "data", "three_period", w, 
                 paste0("modules_", w, "_NoNEC_weighted_complete5.rds")
            )
        )
    }
)

# calculate overlaps
message("Calculating over-time overlap in modules ...")

res_forw = module_overlap(mod_list, forward = TRUE)
res_back = module_overlap(mod_list, forward = FALSE)

# get module sizes
mod_size = purrr::imap(
    mod_list, function(w, v) 
    {
    
        n_mods = length(w)
        sizes = purrr::map_int(seq_len(n_mods), ~ length(w[[.x]]))
        data.table(
            period = rep(v, n_mods),
            module = 1:n_mods, 
            size   = sizes
        )
        
    }
) %>%
    rbindlist

message("\nAssigning new labels to modules ... ")

# relable forward-matched modules and save
df_forw = new_mod_labels_forward(res_forw, threshold = 0.5) %>%
    # merge with mod size data
    merge(mod_size,
          by.x = c("org", "period"),
          by.y = c("module", "period"),
          all = T)
# relable backward-matched modules and save
df_back = new_mod_labels_backward(res_back, threshold = 0.5) %>%
    merge(mod_size,
          by.x = c("dest", "period"),
          by.y = c("module", "period"),
          all = T
    )

message("\nSaving relabeled modules ...")

saveRDS(
    df_forw, 
    here("output", "data", "three_period", "modules_matched_forward.rds")
)

saveRDS(
    df_back, 
    here("output", "data", "three_period", "modules_matched_backward.rds")
)


# align periods of two matches (to merge)

message("\nPlotting over-time summary of three-period data (Figure 3) ...")

df_forw = df_forw[
    !is.na(dest_labels)
][
    , period_2 := period + 1
]

df_back[, period_2 := period]

# merge and melt into long format
p_df = merge(
    df_back,
    df_forw[, list(period_2, org, dest, props)],
    by = c("period_2", "org", "dest"),
    all.x = T
)[
    , .(period,
        period_1,
        size,
        new_org_labels,
        new_dest_labels,
        props.x,
        props.y)
] %>%
    melt(
        id.vars = c(
            "period",
            "period_1",
            "size",
            "new_org_labels",
            "new_dest_labels")
    )

# generate new names for proportions
p_df[
    , variable := ifelse(variable == "props.x",
                         "In-flow of Occupations",
                         "Out-flow of Occupations")
]


message(
    paste0(
        "File Path : ", 
        here("output", "figures", "Figure_3.pdf")
    )
)
message(
    paste0(
        "Data Path : ",
        here("output", "data", "Figure_3.rds")
    )
)
saveRDS(p_df, here("output", "data", "Figure_4.rds"))

pdf(here("output", "figures", "Figure_3.pdf"),
    width = 10, height = 7)
suppressWarnings(print(
    ggplot(p_df) + 
        geom_segment(
            aes(x     = period,
                y     = new_dest_labels,
                xend  = period_1,
                yend  = new_org_labels,
                alpha = value),
                col   = "grey20"
        ) +   
        geom_point(
            aes(
                x    = period, 
                y    = new_dest_labels,
                size = size
            )
        ) +
        labs(y ="", x = "\nPeriod") +
        scale_size_continuous(
            name   = "Size",
            breaks = c(5, 10, 25, 50)
        ) +
        scale_x_continuous(
            breaks = 1:3, 
            labels=c("92-96", "03-07", "11-15")
        ) +
        scale_y_discrete(
            na.translate = FALSE,
            breaks = function(x) { ifelse(grepl("\\(|\\.", x), "", x) },
            label = function(x) { ifelse(grepl("\\(|\\.|23", x), "", x) },
            expand = c(0, 1)
        ) +
        scale_alpha(name = "% Overlap") +
        guides(
            alpha = guide_legend(order = 1),
            size  = guide_legend(order = 2)
        ) +
        facet_wrap(~ variable) + 
        fnb_theme +
        theme(
            panel.border = element_rect(color = "black"),
            legend.position = "right",
            legend.direction = "vertical"
        )
))
dev.off()

message("\nDone!")

### END OF CODE ###