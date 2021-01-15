#' Bootstrapping confidence intervals of LinkRank modularity
#' 
#' @param dat a \code{data.table} object containing columns: the sender occupation (named \code{occ1}), the receiver occupation (named \code{occ2}), the year (named \code{int_year}), and the edge-weight (named \code{weight}). This should be an aggregated \code{data.table} from the raw transition data.
#' @param w_size odd integer; the size of the moving-windows to use in the analysis
#' @param n_cores integer; number of cores to use in parallel computing
#' @param reweight_by_self_loops boolean; whether the transitions should be weighted by the number of self-loops of each occupation
#' @param opts a character string containing additional options that should be feeded to the infomap algorithm
#' @param verbose integer; larger numbers produce more output
#' @param outer_loop_limit integer; number of times outer-most loop should be run to find optimal solution
#' @param core_loop_limit integer; number of times algorithm tries to allocate nodes into best possible module
#' @return returns a list of \code{igraph::communities} object
fit_infomap_mv = function(
    dat, 
    w_size,
    n_cores,
    reweight_by_self_loops = FALSE,
    opts = NULL,
    verbose = 0L,
    outer_loop_limit = 100,
    inner_loop_limit = 50)
{
    
    if (!requireNamespace("doParallel", quietly = TRUE))
        stop("need doParallel package to be installed")
    
    if (w_size %% 2 != 1)
        stop("w_size has to be an odd integer")
    
    if (!all(c("int_year", "weight", "occ1", "occ2") %in% names(dat)))
        stop("dat must have columns int_year, weight, occ1, and occ2")
    
    if (NROW(dat[occ1 != occ2]) == 0)
        stop("dat[occ1 != occ2] has zero rows")
    
    # get years
    years = dat$int_year %>% unique %>% sort
    
    # create matrix of periods to analyze
    periods = sapply(years, function(w) 
    {
        
        c(start = max(w - (w_size - 1) / 2, min(years)),
          mid   = w,
          end   = min(w + (w_size - 1)/2, max(years)))
        
    }
    ) %>% 
        t 
    
    periods = periods[
        !duplicated(periods[, "start"], fromLast = TRUE) & 
            !duplicated(periods[, "end"], fromLast = FALSE),
        ]
    
    if (verbose > 0) {
        
        message('Analyzed Periods:')
        tmp = apply(periods, 1L, function(w) paste0(w[1], '-', w[3]))
        message(paste0(tmp[1:5], collapse=', '))
        message('...')
        message(paste0(tmp[(length(tmp) - 4):length(tmp)], collapse=', '))
        
        message('\nStart Fitting Algorithm ...')
        
    }
    
    
    # if "reweighted" drop self-loops after reweighting
    if (reweight_by_self_loops) {
        
        if (verbose > 0)
            message("Reweighting out-degrees with self-loops ... ")
        
        # make a deep copy to prevent changes in original data
        df = copy(dat)
        
        # reweight ties
        df[, weight := weight / sum(weight), by = occ1]
        
        # check results
        tmp_test = df[, .(weight = sum(weight)), by = occ1]$weight %>%
            unique %>% all.equal(rep(1, length(.)))
        
        if (!tmp_test)
            stop("something went wrong with the reweighting!")
        
        df = df[occ1 != occ2]
        
    } else {
        
        df = dat[occ1 != occ2]
        
    }
    
    # delete "iterator" if there exists any object under the same name
    if (exists('ii')) rm(ii)
    
    # register parallel backend 
    # notes: unfortunately, all messages will be suppressed
    registerDoParallel(n_cores)
    
    # stop cluster when exiting function
    on.exit(stopImplicitCluster())
    
    # start loop 
    res = foreach(ii = seq_len(nrow(periods))) %dopar% {
    
        # get period
        pp = periods[ii, ]
        
        # slice data by year
        df_sub = df[
            int_year >= pp["start"] & int_year <= pp["end"], 
            .(weight = sum(weight)),
            by = list(occ1, occ2)
        ] 
        
        # generate graph (will create edge-attribute 'weight' automatically)
        g = igraph_from_df(df_sub[, .(occ1, occ2, weight)])
        
        # fit infomap
        p_res = fit_infomap(g = g, 
                            verbose  = 0, 
                            parallel = TRUE,
                            iterator = ii)
        
        return(p_res)
        
    }
    
    # assign names to results
    names(res) = paste0('midyear_', periods[, "mid"])
    
    return(res)
    
}