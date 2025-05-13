#!/env/bin/Rscript

## Setting Up R Packages
## Author: Barum Park

DEBUG = FALSE

if (!DEBUG)
    source(".Rprofile")

# repositories
repos = c(
    CRAN = "https://cran.rstudio.com",
    DT = "https://Rdatatable.gitlab.io/data.table"
)
options(repos = repos)

# install from source when asked
options(install.packages.compile.from.source = "always")

# don't use cache
renv::settings[["use.cache"]](FALSE)

# restore renv environment
env_stats = renv::status()

if (!env_stats$synchronized)
    renv::restore()

## ------------------------------------------------------------------
## Check whether all necessary packages are present
## ------------------------------------------------------------------

# lib list
libs = c(
    "renv",
    "logger",
    "Rcpp",
    "doParallel",
    "doRNG",
    "igraph",
    "gridExtra",
    "ggplot2",
    "import",
    "RcppEigen",
    "RcppArmadillo"
)

installed_libs = sapply(libs, function(w) nchar(system.file(package = w)) > 0)
new_libs = libs[!installed_libs]

# CRAN packages
if (length(new_libs) > 0) {

    message("Installing CRAN packages ...")
    message(
        "Following pkgs will be installed ",
        paste0(new_libs, collapse = ", ")
    )

    install.packages(
        new_libs,  repos = "cran.rstudio.com"
    )

} else {

    message("All CRAN libraries already installed")

}

# separately intstall data.table (openMP issue)
message("Checking data.table package ...")
data_table_installed = nchar(system.file(package = "data.table")) > 0

if (!data_table_installed) {

    if (Sys.info()["sysname"] == "Darwin") {

        install.packages(
            "data.table",
            type = "source",
            repos = "https://Rdatatable.gitlab.io/data.table"
        )

    } else {

        install.packages("data.table", repos = "cran.rstudio.com")

    }

} else {

    message("data.table installed!")

}

# local packages (sbmob)
message("Checking sbmob package...")

sbmob_installed = requireNamespace("sbmob", quietly = TRUE)

if (!sbmob_installed) {

    install.packages(
        file.path("renv", "local", "sbmob_0.0.1.4.tar.gz"),
        type = "source",
        rebuild = TRUE,
        repos = NULL
    )

} else {

    if (sessionInfo()$loadedOnly$sbmob$Version != "0.0.1.4") {

        install.packages(
            file.path("renv", "local", "sbmob_0.0.1.4.tar.gz"),
            type = "source",
            rebuild = TRUE,
            repos = NULL
        )

    } else {

        message("Most recent version of sbmob installed!")

    }

}



## ------------------------------------------------------------------
## Create flag and exit
## ------------------------------------------------------------------

# create flag
if (all(sapply(c("sbmob", "data.table", libs), requireNamespace, quietly = TRUE))) {

    message("All packages installed!")

} else {

    stop("some pkgs couldn't be installed")

}

if (!DEBUG) {

    # renv::snapshot()
    import::from("src/R/utils.R", print_session)
    print_session()

}

### EOF ###