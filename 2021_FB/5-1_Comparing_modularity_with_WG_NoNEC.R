###############################################################################
## 
## Comparisons of Modularity between Infomap Partition and WG Scheme
##
## Creator     : Barum Park
## Last Update : 02/23/2020
##
###############################################################################


## Basic Configurations -------------------------------------------------------

# load libraries
library(here)
library(igraph)
library(data.table)
library(ggplot2)

if (!exists("w_size_vec"))
    stop("cannot find w_size_vec")

message("\n\n\n***** Comparing modularity between infomap and WG-scheme *****\n")



# Read in Data ----------------------------------------------------------------

message("Loading data ...")

# load transition data & weights
nonec_dat = merge(
    fread(here("data", "dat_FullSamp_agg_NoNEC.csv")),
    fread(here("data", "obs_weights_FullSamp_NoNEC.csv")),
    by = "int_year",
    all.x = TRUE
)[
    , weights := N * obs_weights
][
    occ1 != occ2
][
    , `:=`(obs_weights = NULL, N = NULL)
]

# load modules (win1/NoNEC)
mod_1 = readRDS(here("data", "moving_window", "MV1_NoNEC.rds"))

# load mapping with WG-scheme
wg_map = fread(here("data", "labmap_NoNEC_with_WG.csv"))



# Calculating LinkRank modularity for WG scheme -------------------------------

message("Calculating modularity for WG-scheme (win1/NoNEC) ...")

# get years
years_nonec = sort(unique(nonec_dat$int_year))

# class-column names
class_names = c("macro", "meso", "micro")
class_col_names = paste0(class_names, "_adj")

# calculate modularity
wg_mod_df = purrr::map(
    years_nonec, 
    function(y)
    {
        
        # create year-specific graph
        g = graph_from_data_frame(
            nonec_dat[int_year == y, .(occ1, occ2, weight = weights)]
        )
        
        # get node-order as stored in object g
        node_order = vertex_attr(g, "name") 
        
        # get class-memberships
        wg_memb = wg_map[
            match(node_order, occ_label), 
            .SD,
            .SDcols = c("occ_label", class_col_names)
        ]
        
        # check order
        if (!identical(wg_memb$occ_label, node_order))
            stop("node order is wrong in wg_memb$occ_label")
        
        # check for possible missing values
        if (any(is.na(wg_memb)))
            stop("wg_memb has missing values")
        
        # calculate LinkRank modularity on WG classes
        res = purrr::map_dbl(
            class_col_names,
            ~ lr_modularity(g, wg_memb[[.x]])
        )
        
        return(c(y, res))
        
    }
    
) %>%
    do.call("rbind", .) %>%
    data.table %>%
    setnames(c("year", class_names))

message("Comparing modularity estimates with the infomap modules ...")
    
# merge with modularity estimates of infomap modules
mod_df = merge(
    wg_mod_df,
    data.table(
        mobility = purrr::map_dbl(mod_1, ~ .x$lr_modularity),
        year     = as.numeric(gsub("midyear_", "", names(mod_1)))
    ),
    all = TRUE
) 
  
# create data.table of differences
diff_df = mod_df[
    ,
    .SD - mobility,
    .SDcols = class_names
][
    , `:=`(year = mod_df$year, mobility = 0)
]

# plot results
message("\nPlotting results of comparison (Figure 6a and 6b)")

# melt and reorder factor levels for partitions
mod_df = melt(
    mod_df, 
    id.vars = "year",
    variable.name = "partition",
    value.name = "modularity"
)[
    , partition := 
      factor(
          partition,
          levels = c(
              "mobility", class_names
          ),
          labels = c(
              "Mobility",
              upper_first_char(class_names)
          )
      )
]

pdf(here("output", "figures", "Figure_6a.pdf"),
    width = 7, height = 6)
print(
    ggplot(
        mod_df,
        aes(
            x = year,
            y = modularity,
            linetype = partition
        )
    ) + 
    geom_line() +
    scale_linetype_manual(
        name = "",
        values = 1:(length(class_names) + 1)
    ) +
    scale_y_continuous(
        name = "LinkRank Modularity\n",
        limits = c(.1, .5)
    ) +
    scale_x_continuous(
        name = "\nYear",
        breaks = seq(1989, 2015, 2)
    ) +
    fnb_theme +
    theme(
        legend.position = c(.025, .025),
        legend.justification = c(0, 0)
    )
)
dev.off()

message(paste0("File Path : ", here("output", "figures", "Figure_6a.pdf")))
fwrite(mod_df, here("output", "data", "Figure_6a.csv"))
message(paste0("Data Path : ", here("output", "data", "Figure_6a.csv")))

# create similar plot for difference between the modularities
diff_df = melt(
    diff_df,
    id.vars = "year",
    variable.name = "partition",
    value.name = "modularity"
)[
    , partition := factor(partition,
                          levels = c("mobility", class_names)
    )
]

pdf(here("output", "figures", "Figure_6b.pdf"),
    width = 7, height = 6)
print(
    ggplot(
        diff_df,
        aes(
            x = year,
            y = modularity,
            linetype = partition
        )
    ) + 
        geom_line() +
        scale_linetype_manual(
            name = "",
            values = 1:(length(class_names) + 1)
        ) +
        scale_y_continuous(
            name = "Difference in Modularity\n(Compared to Infomap Partition) \n",
            limits = c(-.3, 0)
        ) +
        scale_x_continuous(
            name = "\nYear",
            breaks = seq(1989, 2015, 2)
        ) +
        fnb_theme +
        theme(
            legend.position = "none"
        )
)
dev.off()

message(paste0("File Path : ", here("output", "figures", "Figure_6b.pdf")))
fwrite(diff_df, here("output", "data", "Figure_6b.csv"))
message(paste0("Data Path : ", here("output", "data", "Figure_6b.csv")))

message("\nDone!")

### END OF CODE ###