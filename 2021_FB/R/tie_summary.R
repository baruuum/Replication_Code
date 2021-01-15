#' Generating Tie Summaries
#' 
#' @param x \code{data.table} object
#' @param w_dat \code{data.table} object that contains weights to apply to different years
#' @param nonec boolean; if TRUE, not-elsewhere-classified occupations are dropped before summarizing results
#' @param win numeric; size of moving window for which summaries are generated. Must be an odd integer
#' @param trim boolean; if TRUE, end-years are trimmed before summarizing results. Otherwise, end-years are included even if window size is smaller than that specified
#' @details The \code{data.table} object \code{x} must contain the columns \code{occ1, occ2, int_year, N}, which represent, respectively, the sender occupation, receiver occupation, the interview year (so that \code{occ2} is the occupation the respondent currently occupies), and the number of respondents who followed this transition pattern. The `w_dat` object, on the other hand, should contain the columns \code{int_year} and \code{obs_weights} that contain information about the interview year and the weights to be applied for that year.
#' @return returns summaries of ties

tie_summary = function(
    x, 
    w_dat = NULL, 
    nonec  = FALSE,
    nec_regex = NULL,
    win    = 1L,
    trim   = TRUE,
    verbose = TRUE) 
{
   
    # check data format
    if (!is.data.table(x)) 
        stop("Input has to be data.table")
    
    if (!all(names(x) %in% c("occ1", "occ2", "int_year", "N"))) 
        stop("Input data has to contain columns occ1, occ2, int_year, N")
        
    # apply weights if specified
    if (!is.null(w_dat)) {
        
        if (verbose)
            message("Applying normalizing weights ...")
        
        if (!all(c("int_year", "obs_weights") %in% names(w_dat)))
            stop("w_dat does not contain columns int_year and obs_weights")
        
        dat = merge(
                x, 
                w_dat[, list(int_year, obs_weights)],
                by = "int_year"
        )

        dat[
            , N := N * obs_weights
        ][
            , obs_weights := NULL
        ]
        
    } else {
        
        if (verbose)
            message("Not using normalizing weights ...")
        
        dat = x
        
    }

    # drop nec occupations if specifeid
    if (nonec) {
        
        if (is.null(nec_regex))
            stop("need nec_regex when nonec is specified")
        
        if (verbose) 
            message("Dropping NEC occpuations ...")
        
        reg_vec = paste0(nec_regex, collapse="|")
        
        dat = dat[!grepl(reg_vec, occ1) & !grepl(reg_vec, occ2)]
        
    }

    # generate moving windows
    years = unique(dat[, int_year]) %>% 
        sort
    
    periods = sapply(years, function(w) {
        
        start = max(w - (win - 1)/2, min(years))
        end = min(w + (win - 1)/2, max(years))
        mid = w
        
        return(c(start, mid, end))
        
    }) %>% t

    # trim end-points?
    if (trim) {
        
        periods = periods[
            !duplicated(periods[,1], fromLast=T) & 
                !duplicated(periods[,3], fromLast=F),
            ]
        
    }

    # generate total obs. and between-occ obs within windows
    x_win = lapply(1:nrow(periods), function(y) {
        
            res = dat[
                int_year >= periods[y, 1] & int_year <= periods[y, 3],
                sum(N),
                by = list(occ1, occ2)
            ] %>%
            setnames("V1","N")
            
            res[, mid_year := periods[y, 2]]
            return(res)
        
        }) %>%
    rbindlist %>%
    setcolorder(c("occ1", "occ2", "mid_year", "N"))

    # generate total obs. and between occ obs
    y = merge(
        x_win[, sum(N), by = mid_year],
        x_win[occ1 != occ2, sum(N), mid_year],
        by = "mid_year",
        all = TRUE
    ) %>%
        setnames(c("mid_year", "ws", "nos"))
    
    # generate % between
    y[, rr := nos/ws] 
    
    # add number of non-isolated unique occs
    y = merge(y, 
        x_win[occ1 != occ2, length(unique(c(occ1, occ2))), by = mid_year],
        by="mid_year"
    ) %>%
        setnames(c("year",
                   "No. Observations",
                   "No. Between-Occupation Transitions",
                   "% Between-Occupation Transitions",
                   "No. Unique Non-isolated Occupations")
        )
    
    # change all columns to "double" format (for melting)
    y = y[, lapply(.SD, as.numeric)]
    
    # melt and add type-variable
    z = melt(y, id.vars = "year")
    z[
        , `:=`(
            weighted = ifelse(is.null(w_dat), F, T),
            nec = ifelse(nonec, 
                        "NEC Occupations Excluded", 
                        "All Occupations")
        )
    ]
    
    return(z)
}
