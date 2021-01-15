###############################################################################
##
## Generating Three-period Data for the Analysis
##
##
## Creator     : Barum Park
## Last Update : 02/21/2020
##
## Notes:
## 1) Data are weighted by normalizing weights
## 2) NoNEC occupations are not included
## 3) Data generated for FullSamp specification
##
###############################################################################

message("\n\n\n***** Setting up three-period data *****")
message("Sample specification : FullSamp/NoNec/Weighted/Complete5\n")

if (!exists("p_vec"))
    stop("p_vec object cannot be found")

message("Loading data ...")
dat = fread(here("data", "dat_FullSamp_agg_NoNEC_complete5.csv"))
weights = fread(here("data", "obs_weights_FullSamp_NoNEC.csv"))

message("Weighting data by normalizing weights ...")
dat = merge(
    dat, weights, by = "int_year", all.x = TRUE
)[
        , N := N * obs_weights
][
        , obs_weights := NULL
]

# save labels
# message("Saving numeric_id -> occ_name mapping ...")
# fwrite(
#     map_labels(dat[, unique(occ1, occ2)]),
#     here("data", "labmap_3p_NoNEC_complete5.csv")
# )


message("Generating three-period data ... ")

# loop over periods
for (pp in p_vec) {
    
    # check whether elements are characters
    if (is.character(pp)) {
        
        # check whether element is a single year within data range
        if (as.numeric(pp) %in% 1989:2015) {

            fwrite(
                dat[int_year == as.numeric(pp)],
                here("data", 
                     paste0("dat_", pp, "_NoNEC_weighted_complete5.csv")
                )
            )

        } else {
            
            # calculate start and end year of window
            start_y = substr(pp, 1, 2) 
            start_y = ifelse(
                substr(start_y, 1, 1) %in% c("0", "1"),
                paste0("20", start_y),
                paste0("19", start_y)
            ) %>%
                as.numeric
            
            end_y = substr(pp, 3, 4)
            end_y = ifelse(
                substr(end_y,1,1) %in% c("0", "1"),
                paste0("20", end_y),
                paste0("19", end_y)
            ) %>%
                as.numeric
            
            # aggregate data over window and write out
            fwrite(
                dat[
                    int_year %in% start_y:end_y, 
                    .(N = sum(N)), 
                    by = list(occ1, occ2)
                ],
                here("data", 
                     paste0("dat_", pp, "_NoNEC_weighted_complete5.csv"))
            )
        
        }

    } else if (is.numeric(pp)) {
        
        fwrite(
            dat[int_year == pp],
            here("data", 
                 paste0("dat_", pp, "_NoNEC_weighted_complete5.csv"))
        )
            
    } else {
            
            stop("p_vec is neither a character nor a numeric vector")
            
    }
    
}

message("\nDone!")

### End of Code ###