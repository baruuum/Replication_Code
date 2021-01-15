#' Additional Analyses
#' 
#' Creator     : Barum Park
#' Last Update : 08/14/2020
#'
#' Notes -----------------------------------------------------------------------
#'
#' This file creates Figure E15, which was added to the paper in the last
#' round of revisions. As the \code{_RUN_REPLIATION.R} script is self-contained, 
#' an additional replication code was created instead of adding this code to the 
#' former one.
#' 
#' The log of our own replication of this script is contained in the
#' additional_figures_log.txt file, which is distributed in this replication 
#' package as well.
#'  
#' Differently from other codes in this replication package, this script 
#' contains its own function definitions.



## Basic configurations --------------------------------------------------------

# record starting time
run_start_time = Sys.time()

# load libraries
suppressPackageStartupMessages({
    
    # data management & plotting
    library("here")
    library("data.table")
    library("magrittr")
    library("ggplot2")

})

if (!requireNamespace("purrr"))
    stop("Please install the package purrr")

# number of cores to use in multi-threading
n_cores = 6L
setDTthreads(threads = n_cores)

# source functions
source(here("R", "_load_all_functions.R"))

# window sizes to be considered (must be odd integers)
w_size_vec = c(1, 3, 5)

# data specifications (drop not-elsewhere-classified occs)
dat_spec = "NoNEC"

# ggplot theme options
fnb_theme = theme_bw() +
    theme(
        panel.grid        = element_blank(),
        strip.background  = element_blank(),
        panel.border      = element_rect(colour = "black"),
        strip.text        = element_text(size = 16,
                                         hjust = .05,
                                         face = "bold"),
        axis.title        = element_text(size = 14),
        legend.background = element_rect(fill = "white",
                                         colour = "black",
                                         size = .1),
        legend.key.size   = unit(2, "line"),
        legend.key.height = unit(1, "line"),
        legend.position = c(.99,.025),
        legend.justification = c(1, 0),
        legend.direction = "horizontal"
    )

# update some default options in ggplot
update_geom_defaults("line", list(size = 0.6))



## Run analysis ----------------------------------------------------------------

# stability analysis                                        (creates Figure E15)
source(here("8-1_Stability.R"))



# Print date and session information -------------------------------------------

# create log-file (and change contact info)
message("\n\n\n")
capture.output(
    print_session_and_date(run_start_time),
    type = "message"
) %>%
    gsub("bp1094@nyu\\.edu", "b\\.park@cornell\\.edu", .) %>%
    paste0(collapse = "\n") %>%
    message 



### END OF CODE ###