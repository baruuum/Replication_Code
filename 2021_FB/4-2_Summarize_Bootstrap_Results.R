###############################################################################
## 
## Summarizing bootstrapping results
##
## Creator     : Barum Park
## Last Update : 02/21/2020
##
###############################################################################


# Basic Configurations --------------------------------------------------------

# load libraries
library(here)
library(igraph)
library(data.table)
library(ggplot2)

if (!exists("w_size_vec"))
    stop("cannot find w_size_vec")

message("\n\n\n***** Summarizing bootrstrapping results *****\n")

# Load data --------------------------------------------------------------------

message("Loading data ...")

# load and merge data for non-complete occs
boot_df = purrr::map(
    w_size_vec,
    function(x)
    {
        df = fread(
            here(
                "data", 
                "moving_window", 
                paste0("MV", x, "_NoNEC_BOOT.csv")
            )
        )
        
        mod = readRDS(
            here(
                "data", 
                "moving_window", 
                paste0("MV", x, "_NoNEC.rds")
            )
        )
        
        df[
            , `:=`(
                w_size = x,
                est = purrr::map_dbl(mod, ~ .x$lr_modularity),
                complete = FALSE
            )
        ]
            
        
        return(df)
        
    }
) %>%
    rbindlist

# check whether point estimate lies within intervals
if (!all(boot_df[, ifelse(est > lower & est < upper, T, F)] ))
    stop("point estimates lie outside of bootstrap intervals")

# load and merge data for complete occs
boot_df_complete = purrr::map(
    w_size_vec,
    function(x)
    {
        df = fread(
            here(
                "data", 
                "moving_window", 
                paste0(
                    "MV", 
                    x, 
                    "_NoNEC_complete", 
                    x, 
                    "_BOOT.csv"
                )
            )
        )
        
        mod = readRDS(
            here(
                "data", 
                "moving_window", 
                paste0("MV", x, "_NoNEC_complete", x, ".rds")
            )
        )
        
        df[
            , `:=`(
                w_size = x,
                est = purrr::map_dbl(mod, ~ .x$lr_modularity),
                complete = TRUE
            )
        ]
        
        return(df)
        
    }
) %>%
    rbindlist

# check whether point estimate lies within intervals
if (!all(boot_df_complete[, ifelse(est > lower & est < upper, T, F)] ))
    stop("point estimates lie outside of bootstrap intervals")




# Plot results --------------------------------------------------------------------

message("\nPlotting trends in modularity together with boot-ci (Figure 4) ...")

pdf(here("output", "figures", "Figure_4.pdf"),
    width = 8, height = 7)  
print(
    ggplot(
        boot_df[w_size == 1],
        aes(
            x = mid_year, 
            y = est,
            ymin = lower, 
            ymax = upper
        )
    ) +
    geom_ribbon(alpha = .15) +
    geom_line() +
    labs(
        y = "LinkRank Modularity\n",
        x = "\nYear (Mid-Year of Moving Window)"
    )+
    scale_x_continuous(breaks = seq(1989, 2015, 2)) +
    fnb_mod_y_axis + 
    fnb_theme 
)
dev.off()

message("File Path : ", here("output", "figures", "Figure_4.pdf"))
fwrite(boot_df[w_size == 1], here("output", "data", "Figure_4.csv"))
message("Data Path : ", here("output", "figures", "Figure_4.csv"))


message(
    paste0("\nPlotting trends in modularity for all moving windows ",
           "(complete and non-complete, Figure E5) ...")
)

# combine datasets
boot_comb_df = rbind(boot_df, boot_df_complete)

# create factors for plotting
boot_comb_df[
    , w_size := factor(
        w_size, 
        levels = w_size_vec,
        labels = paste0("Window Size = ", w_size_vec)
    )
][
    , complete := factor(
        complete,
        levels = c(TRUE, FALSE),
        labels = c(
            "Occupations Measured in All Moving Windows",
            "Occupations Measured by Year"
        )
    )
]

 
pdf(here("output", "figures", "Figure_E5.pdf"),
    width = 11, height = 9)  
print(
    ggplot(
        boot_comb_df,
        aes(
            x = mid_year,
            y = est,
            ymin = lower,
            ymax = upper
        )
    ) +
    geom_ribbon(alpha = .15)+
    geom_line() +
    facet_grid(w_size ~ complete) +
    labs(y = "LinkRank Modularity\n",
         x = "\nYear (Mid-Year of Moving Window)")+
    scale_x_continuous(breaks = seq(1989, 2015, 13))+
    fnb_mod_y_axis +
    fnb_theme +
    theme(
        legend.position = "none",
        strip.text      = element_text(face = "plain")
    )
)
dev.off()

message("File Path : ", here("output", "figures", "Figure_E5.pdf"))
fwrite(boot_df, here("output", "data", "Figure_E5.csv"))
message("Data Path : ", here("output", "figures", "Figure_E5.csv"))


message("\nDone!")

### END OF CODE ###