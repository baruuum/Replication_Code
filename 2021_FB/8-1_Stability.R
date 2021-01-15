###############################################################################
## 
## Stability of Mobility Classes
##
## Creator     : Barum Park
## Last Update : 08/14/2020
##
###############################################################################


## Basic Configurations -------------------------------------------------------

# load libraries
library("here")
library("data.table")
library("magrittr")
library("ggplot2")

if (!requireNamespace("purrr"))
    stop("Install the package purrr")


## Function Definitions --------------------------------------------------------

#' Function to get sequential mutual information
#' 
#' @param w_size integer; window size
#' @param norm string; how to normalize mutual information; must be one of "none", "min", "max", "arithmetic", "geometric", or "harmonic"; defaults to "none"
#' @param spec string; data specification: either FullOccs or NoNEC
#' @param adjust boolean; whether to calculate the adjusted mutual information; defaults to TRUE
#' @returns sequentially calculated mutual informations between partitions identified by the infomap algorithm
get_seq_mi = function(
    w_size, 
    spec = c("FullOccs", "NoNEC"), 
    norm = c(
        "none", 
        "min", 
        "max", 
        "arithmetic", 
        "geometric", 
        "harmonic"
    ), 
    adjust = TRUE
) {
    
    message(
        paste0("\nWorking on window size ", w_size, " ...")
    )
    
    # check arguments
    if (w_size %% 1 != 0)
        stop("w_size has to be an integer")
    
    if (!is.logical(adjust))
        stop("adjust has to be either TRUE or FALSE")
    
    spec = match.arg(spec, several.ok = FALSE)
    norm = match.arg(norm, several.ok = FALSE)
    
    # calculate mutual information
    message("Loading data ...")
    
    # load modules 
    res = readRDS(
        here(
            "data", 
            "moving_window", 
            paste0(
                "MV", w_size, "_", spec, "_complete", w_size, ".rds"
            )
        )
    )
    
    # years of 5-year moving windows
    years = gsub("midyear_", "", names(res)) %>%
        as.numeric
    
    # check whether years are ordered
    if (!all(diff(years) == 1))
        stop("years are not ordered subsequentially")
    
    # check occupation set
    occ_mismatch = purrr::map(res, function(w) {w$names}) %>%
        purrr::map(setdiff, .[[1]]) %>% 
        purrr::map_int(length) %>%
        sum %>%
        as.logical
    
    if (occ_mismatch)
        stop("occupations differ across years")
    
    # create list of data.tables with membership-occ 
    dat = purrr::imap(
        res, 
        function (x, y) {
            data.table(
                occ = x$names, 
                mem = x$membership,
                year = as.integer(gsub("midyear_", "", y))
            )
        }
    ) %>%
        # bind data.tables together (long-format data)
        rbindlist %>%
        # dcast into wide-format
        dcast(occ ~ year, value.var = "mem")
    
    # add `y` to column names of years
    setnames(
        dat, 
        c(
            names(dat)[1], 
            paste0("y", names(dat)[2:ncol(dat)])
        )
    )
    
    message("Calculating Mutual Information ...")
    
    # calculate mutual information across subsequent years
    mi_seq = purrr::map_dbl(
        seq(2L, length(years), 1L),
        function (yy) {
            
            y1 = years[yy - 1L]
            y2 = years[yy]
            
            mutual_info(
                table(
                    dat[, paste0("y", c(y1, y2)), with = FALSE]
                ),
                normalized = norm,
                adjusted = adjust
            )
        }
    )
    
    data.table(
        mi = mi_seq,
        end_year = years[2:length(years)],
        win = w_size
    )
    
}



## Start Analysis of Mobility Class Stability ----------------------------------

message("\nCalculating mutual information over subsequent years ...")

seq_mi = purrr::map(
    w_size_vec, 
    get_seq_mi, 
    spec = dat_spec,
    norm = "harmonic", 
    adjust = TRUE
) %>% 
    rbindlist

seq_mi[
    , win := factor(
        win, 
        levels = w_size_vec
    )
]

message("\nPlotting results (Figure E15)")
f_path = here("output", "figures", "Figure_E15.pdf")
d_path = here("output", "data", "Figure_E15.csv")
message(paste0("File path : ", f_path))
message(paste0("Data path : ", d_path))

# plot
pdf(f_path, width = 7.5, height = 5)
print(
    ggplot(
        seq_mi, 
        aes(x = end_year, y = mi, group = win, linetype = win)
    ) + 
        geom_line() +
        scale_linetype_manual(
            name = "Window Size",
            values = 3:1
        ) +
        labs(
            y = "Adjusted Mutual Information",
            x = "Year"
        ) + 
        ylim(.4, 1) +
        scale_x_continuous(
            breaks = seq(1989, 2015, 2)
        ) +
        fnb_theme
)
dev.off()

# write out data
fwrite(seq_mi, d_path)