#' Replication Code
#' 
#' Creator     : Barum Park
#' Last Update : 02/27/2020
#'
#' Notes -----------------------------------------------------------------------
#'
#' 1) Infomap has to be compiled in a subdirectory of the working directory. 
#'    The name of the subdirectory has to be "infomap" in order for the 
#'    code to work. For all analyses that relies on the infomap algorithm, 
#'    Infomap version 0.18.27 was used.
#'
#' 2) For successful replication, the following R packages have to be installed
#'
#'       cowplot
#'       data.table
#'       doParallel
#'       doRNG
#'       emojifont
#'       gdata
#'       ggraph
#'       ggplot2
#'       gridExtra 
#'       here
#'       igraph 
#'       purrr
#'       Rcpp
#'       RcppArmadillo
#'       readstata13
#'       stringr
#'
#'     and the dependencies therin. For example, the pipe (%>%) operator will
#'     be pulled in via the ggplot2 or igraph package, which depend on the 
#'     magrittr package, the gridExtra package will load (via namespace) 
#'     the grid package, and so on. So, it is important to install all the 
#'     dependencies as well. This can be done by 
#'     
#'         install.packages("<pkg-name>", dependencies = TRUE)
#'
#' 3) Data specifications:
#'
#'     a) FullSamp   : includes all samples
#'     b) Male       : includes only male respondents
#'     c) Female     : includes only female respondents
#'     d) FullOccs   : includes all occupations, with a few exceptions which are
#'                     laid out in the article and can be found in the code 
#'                     1-1_Data_Setup_Yearly.R
#'     e) NoNEC      : not-elsewhere-classified occupations are excluded in 
#'                     addition to those that are excluded in FullOccs
#'     f) complete_5 : analysis restricted to occupations that appear in all
#'                     five-year moving windows
#'     g) Occ2000    : analysis restricted to post-2002 years, with harmonized
#'                     OCC2000 occupational coding scheme used instead of the 
#'                     1990 version
#'                   
#'
#' 4) Unfortunately, when the infomap algorithm is compiled using the 
#'    instructions laid out at mapequation.org, it will not run on a windows 
#'    OS. For Windows users, installing Bash on Ubuntu on Windows might be an 
#'    option. However, as of this date, the desktop version of rstudio is no
#'    longer supported on Bash on Ubuntu on Windows. A workaround is to 
#'    install rstudio-server and using a Chrome/Firefox browser. After 
#'    installing both base-R and rstudio-server, you can launch rstudio
#'    with
#'    
#'        sudo rstudio-server start
#'    
#'    Then, open your browser and enter the address
#'    
#'        localhost:8787
#'    
#'    You'll find the usual rstudio environment there
#'    
#' 5) Some of the results (Table 2, Table D4) are exported as LaTeX code, which
#'    relies on the arydshln package. To compile the LaTeX code into pdf-files
#'    you have to add
#'    
#'        \usepackage{arydsln}
#'        
#'    to your preamble of your LaTeX file.



### Load libraries ------------------------------------------------------------

# load libraries
suppressPackageStartupMessages({

    # data management & data structures
    library(here)
    library(igraph)
    library(data.table)

    # plotting
    library(ggplot2)
    library(gridExtra)
    library(emojifont)

    # parallel computing
    library(doParallel)
    library(doRNG)
    
})

# check packages
nspaces = c("cowplot",
            "gdata", 
            "ggraph",
            "grid",
            "magrittr", 
            "Matrix", 
            "purrr",
            "scales",
            "Rcpp",
            "RcppArmadillo",
            "readstata13", 
            "stringr")

for (ns in nspaces) 
    if (!requireNamespace(ns, quietly = TRUE))
        stop(paste0(ns, " package needed to run this code"))

# record starting time
run_start_time = Sys.time()



## Basic configurations -------------------------------------------------------

# set random seed
set.seed(19871984)

# number of cores used for parallel processing
n_cores = 19

### Setting up Directory Paths

if (!dir.exists(here("data"))) {
   
    message("Creating data directory ...")
    dir.create(here("data"), recursive = T)
   
}

if (!dir.exists(here("output"))) {
   
    message("Creating output directory ...")
    dir.create(here("output"), recursive = T)
    dir.create(here("output", "tables"))
    dir.create(here("output", "figures"))
    dir.create(here("output", "data"))

}

# analyzed periods (in 3-period analysis)
# note: 
# 1) Make sure to enter the periods as a "character" vector if you 
#    want to have periods that span across several years
# 2) To analyze specific "years" as periods, enter the p.vec as
#    a numeric vector
p_vec = c("9296", "0307", "1115")

# window sizes to be considered (must be odd integers)
w_size_vec = c(1, 3, 5)

# number of bootstrap samples to use
n_boot = 1000

# confidence level for bootstrapped intervals
b_level = .95

# model specifications
occ_spec = c("FullOccs",  # all occupations
             "NoNEC")    # drop not-elsewhere-specified occupations

# data specifications 
# note: "fullpop" specification is pushed to the last entry
#       see code in 1_Gen_Period_Data.R for reason
samp_spec = c("Male",   # male population
             "Female", # female population
             "FullSamp")    # full population

# specify starting year of analysis
start_year = 1989

# specify year based on which weights should be normalized
w_stand_year = 1989

# degree of polynomial to use in regression adjustments
p_term = 3

# half-window sizes for regression adjustments
adj_win_vec = 2:5


# NEC names (regular expressions)
nec_regex = c("n\\.e\\.c", 
              "n\\. e\\. c\\.",
              "nec", 
              "elsewhere") 

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

fnb_mod_y_axis = scale_y_continuous(
    breaks = seq(.3, .55, .05),
    limits=c(.3, .55)
)

# update some default options in ggplot
update_geom_defaults("line", list(size = 0.6))



# Sourcing functions ----------------------------------------------------------

# source functions
source(here("R", "_load_all_functions.R"))

# current objects in the environment
current_obs = c("current_obs", ls())



# Create example plots (for illustrative purposes) ----------------------------

source(here("0_Illustrations.R"))

# cleaning up a little ...
rm(list = setdiff(ls(), current_obs))
gc()



# Data setup and summary ------------------------------------------------------

# setting up yearly data for different data specifications
source(here("1-1_Data_Setup_Yearly.R"))

# setting up three-period data as specified in p_vec
source(here("1-2_Data_Setup_Three_Period_NoNEC.R"))

# summarize FullSamps for and FullOccs and NoNEC             (creates Figure E1)
source(here("1-3_Summarize_FullSamp.R"))

# summarize 3-period data                              (creates Figure 2 and E2)
source(here("1-4_Summarize_Three_Period_NoNEC_complete5.R"))

# cleaning up a little ...
rm(list = setdiff(ls(), current_obs))
gc()



# Analyzing three-period data -------------------------------------------------

# fit map equation to 3-period data
source("2-1_Fit_Infomap_Three_Period_NoNEC_complete5.R")

# summarize results
suppressWarnings(source("2-2_Summarize_Modules_Three_Period_NoNEC_complete5.R"))

# match modules over time                                     (creates Figure 3)
source("2-3_Match_Modules_Three_Period_NoNEC_complete5.R")

# comparison with Weeden-Grusky scheme     (creates Table 2, D1, D2, D3, and D4)
source("2-4_Comparison_with_WG_NoNEC_complete5.R")

# cleaning up ...
rm(list = setdiff(ls(), current_obs))
gc()



# Moving windows ---------------------------------------------------------------

# fitting map eqauation to moving windows
source("3-1_Fit_Infomap_to_Moving_Windows.R")

# summarizing results            (creates Figure C1, E3, E4, E5, E6, E7, and E8)
source("3-2_Summarize_Moving_Windows_Results.R")

# regression-based adjustements for scheme-changing years    (creates Figure E9)
source("3-3_Regression_Adjustments_of_Modularity_Trends_NoNEC.R")

# cleaning up ...
rm(list = setdiff(ls(), current_obs))
gc()



# Bootstrap modularity ---------------------------------------------------------

# bootstrap modularity measure for moving window analysis
source("4-1_Bootstrap_Modularity.R")

# summarize bootstrapping results
source("4-2_Summarize_Bootstrap_Results.R")

# cleaning up ...
rm(list = setdiff(ls(), current_obs))
gc()



# Compare moving window results with WG-Scheme ---------------------------------

# comparing modularity                                (creates Figure 6a and 6b)
source("5-1_Comparing_modularity_with_WG_NoNEC.R")

# calculating mutual information between partition    (creates Figure 5 and E10)
source("5-2_Mutual_Information_with_WG_NoNEC_complete5.R")

# cleaning up ...
rm(list = setdiff(ls(), current_obs))
gc()



# Log-linear Models ------------------------------------------------------------

# compare partitions based on BIC and AIC             (creates Figure 7 and E11)
source("6-1_Log-linear_Models.R")

# cleaning up ...
rm(list = setdiff(ls(), current_obs))
gc()



# Skills and Earnings ----------------------------------------------------------

# analyzing skill requirements of occupations         (creates Figure 8 and E12)
source("7-1_Skill_Requirements.R")

# analyzing earnings                            (creates Figure 9, E13, and E14)
source("7-2_Earnings.R")


# Print date and session information -------------------------------------------

print_session_and_date(run_start_time)


### END OF CODE ###
