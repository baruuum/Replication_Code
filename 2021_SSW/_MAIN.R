################################################################################
##
## Main Code to Create All Results 
##
################################################################################

# starting time
message(
    paste0("Running _MAIN.R on ", Sys.time(), "\n")
)


message("Checking required packages ...")

pkgs = sapply(
    c(
        "igraph",
        "here",
        "data.table",
        "rio",
        "RcppArmadillo"
      ),
    requireNamespace,
    quietly = TRUE
)

if (!all(pkgs)) {
    message(
        paste0(
            "Can't run script as following packages are not installed:\n\n",
            paste0(names(pkgs)[!pkgs], collapse = "\n"),
            "\nInstall packages and try again.\n\n"
            )
    )
} else {
    
    message("All packages installed!\n\n")
    
}
    

# load package
suppressPackageStartupMessages({
  
  library("here")
  library("igraph")
  library("data.table")
  
})

# sourcing & testing functions
message("Sourcing util functions ...")
source(here("R", "_utils.R"))

message("Testing util functions ...")
source(here("R", "test_code", "test_utils.R"))

message("Sourcing Procrustes transform function ...")
Rcpp::sourceCpp(here("src", "procrustes.cpp"))

message("Testing function for Procrustes transform ...")
source(here("R", "test_code", "test_procrustes.R"))

message("Sourcing functions for distance calculation ...")
Rcpp::sourceCpp(here("src", "dist_by_partition.cpp"))

message("Testing distance functions ...")
source(here("R", "test_code", "test_dist_by_partition.R"))

message("Sourcing functions for plotting ...")
source(here("R", "_plot_utils.R"))
message("Done!\n")

# sourcing R scripts
source(here("R", "01_Fall2019_replication.R"))
source(here("R", "02_Fall2020_analysis.R"))
source(here("R", "03_Table_19_20.R"))
source(here("R", "04_Update_table.R"))
source(here("R", "05_Plot_19.R"))
source(here("R", "06_Plot_20.R"))
source(here("R", "07_Plot_transform.R"))
source(here("R", "08_Clustering.R"))


# print time and session info
message(
    paste0("\n_MAIN.R done on ", Sys.time())
)

message("\n\nSession Information ----------------------------\n")

print_sessionInfo()


### END OF CODE ###
