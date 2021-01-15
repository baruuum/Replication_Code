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

message("\n\n\n***** Comparision using adjusted mutual information *****\n")



# Read in Data ----------------------------------------------------------------

message("Loading data ...")

# load mapping with WG-scheme
wg_map = fread(here("data", "labmap_NoNEC_with_WG.csv"))

# load modules (win5/NoNEC/compete)
mod_5_complete = readRDS(
    here("data", "moving_window", "MV5_NoNEC_complete5.rds")
)

# years of 5-year moving windows
years_5 = gsub("midyear_", "", names(mod_5_complete)) %>%
    as.numeric %>%
    sort

# class names
class_names = c("macro", "meso", "micro")
class_col_names = paste0(class_names, "_adj")



# Calculating mutual information measures -------------------------------------


# calculate mutual information
mi_df = purrr::map(
    years_5, 
    function(y)
    {
        
        # merge yearly modules to wg_map
        y_dat = merge(
            wg_map,
            data.table(
                mobility = mod_5_complete[[paste0("midyear_", y)]]$membership,
                occ_label = mod_5_complete[[paste0("midyear_", y)]]$names
            ),
            by = "occ_label",
            all.y = TRUE
        )
        
        # check for missing
        if (any(is.na(y_dat)))
            stop("missing values in y_dat")
        
        # create cross-table between wg-partitions and infomap modules
        y_tab_list = purrr::map(
            class_col_names,
            ~ y_dat[, table(mobility, get(.x))]
        ) 
        
        # calculate mutual information between partitions
        m_info = purrr::map(
            y_tab_list,
            function (x)
            {
                c(adjusted = mutual_info(
                      x, 
                      normalized = "harmonic",
                      adjusted = TRUE
                  ),
                  unadjusted = mutual_info(
                      x,
                      normalized = "harmonic",
                      adjusted = FALSE
                  )
                )
            }
        ) %>%
            do.call("rbind", .) %>%
            data.table
        
        # add labels and year
        m_info[
            , wg_class := class_names
        ][
            , year := y
        ]
        
        return(m_info)
        
    }
    
) %>%
    # bind to single data.table
    rbindlist %>%
    # melt into long-format
    melt(
        id.vars = c("wg_class", "year"),
        variable.name = "adjustment",
        value.name = "mutual_info"
    )



# Plot results ---------------------------------------------------------------

# create factor out of wg_class
mi_df[
    , wg_class := 
        factor(
            wg_class,
            levels = class_names,
            labels = paste0(upper_first_char(class_names), " ") # some extra space for legend
        )
][
    , adjustment := factor(
        adjustment,
        levels = c("adjusted", "unadjusted"),
        labels = c("Adjusted", "Unadjusted")
    )
]

# plot adjusted mutual information
message("\nPlotting adjusted mutual information (Figure 5) ...")
pdf(here("output", "figures", "Figure_5.pdf"),
    width = 7, height = 5)
print(
    ggplot(
        mi_df[adjustment == "Adjusted"],
        aes(x = year,
            y = mutual_info,
            linetype = wg_class)
    ) + 
    geom_line() + 
    scale_linetype_manual(
        name = "",
        values = 2:(length(class_names) + 1)
    ) + 
    scale_y_continuous(
        name = "Adjusted Mutual Information\nwith Mobility Classes\n"
    ) + 
    scale_x_continuous(
        name = "\nYear", 
        breaks = seq(1991, 2013, 2)
    ) + 
    fnb_theme +
    theme(
        legend.position=c(.02, .025),
        legend.justification=c(0, 0),
        legend.title = element_blank()
    )
)
dev.off()

message(paste0("File Path : ", here("output", "figures", "Figure_5.pdf")))

fwrite(
    mi_df[
        adjustment == "Adjusted"
    ][
        , adjustment := NULL
    ],
    here("output", "data", "Figure_5.csv")
)

message(paste0("Data Path : ", here("output", "data", "Figure_5.csv")))

message("\nPlotting comaparison between adjusted and unadjusted MI (Figure E10) ...")

pdf(here("output", "figures", "Figure_E10.pdf"),
    width = 10, height = 5)
print(
    ggplot(
        mi_df,
        aes(x = year,
            y = mutual_info,
            linetype = wg_class)
    ) + 
    geom_line() + 
    scale_linetype_manual(
        name = "",
        values = 2:(length(class_names) + 1)
    ) + 
    scale_y_continuous(
        name = "Normalized Mutual Information\nwith Mobility Classes\n"
    ) + 
    scale_x_continuous(
        name = "\nYear", 
        breaks = seq(1991, 2013, 2)
    ) + 
    facet_wrap(~ adjustment) +
    fnb_theme +
    theme(
        legend.title = element_blank()
    )
)
dev.off()

message(paste0("File Path : ", here("output", "figures", "Figure_E10.pdf")))

fwrite(
    mi_df,
    here("output", "data", "Figure_E10.csv")
)

message(paste0("Data Path : ", here("output", "data", "Figure_E10.csv")))

message("\nDone!")

### END OF CODE ###
