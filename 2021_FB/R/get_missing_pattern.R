#' Get pattern of missing occupation across different year intervals
#' 
#' @param dat a \code{data.table} object containing the transitions
#' @param w_size moving window size
#' @param include_isolates boolean; if TRUE, isolates are treated as non-missing. Defaults to FALSE.
#' @return returns a \code{data.table} object with the missing pattern for each occupation
get_missing_pattern = function(
    dat, 
    w_size = c(1, 3, 5),
    include_isolates = FALSE) 
{
    
    # check dat
    if (!all(c("occ1", "occ2", "int_year") %in% names(dat)))
        stop("dat has to contain the columns occ1, occ2, and int_year")
    
    # check w_size
    if (!all(w_size %in% c(1, 3, 5)))
        stop("w_size has to be a subset of c(1, 3, 5)")
    
    # drop self-loops if include_isolates == FALSE
    if (!include_isolates) 
        dat = dat[occ1 != occ2]
    
    # occ list & measured years
    occs  = dat[, unique(c(occ1, occ2))]
    y_char = paste0(dat[, sort(unique(int_year))])
        
    # unique occs for each year
    m_dat = dat[
        , .(occ_label = unique(c(occ1, occ2))), by = int_year
    ] %>%
        dcast(occ_label ~ int_year, value.var = "int_year")
    
    # turn year_vals into zero (missing) and ones (present)
    m_dat[
        , 
        (y_char) := lapply(.SD, function(w) as.numeric(!is.na(w))), 
        .SDcols = y_char
    ][
        , 
        n_miss := length(y_char) - rowSums(.SD), 
        .SDcols = y_char
    ]
    
    # generate missing pattern
    m_dat[
        , m_pattern := apply(m_dat[, y_char, with = F],
                             1L, 
                             paste0, 
                             collapse = "")
    ]
    
    # count times occupation is missing for different window sizes
    
    for (w in w_size) {
        
        m_dat[, `:=`
            (
                paste0("m_count_win_", w),
                stringr::str_count(
                    m_pattern, paste0(rep("0", w), collapse="")
                )
            )
        ]
        
    }
    
    # return results
    return(m_dat)
    
}
