#' Bootstrapping confidence intervals of LinkRank modularity
#' 
#' @param dat a \code{data.table} object containing columns: the sender occupation (named \code{occ1}), the receiver occupation (named \code{occ2}), the year (named \code{int_year}), and the edge-weight (named \code{obs_weights}). This should *not* be an aggregated \code{data.table} but the raw transitions.
#' @param w_size odd integer; the size of the moving-windows to use in the analysis
#' @param n_cores integer; number of cores to use in parallel computing
#' @param n_resample integer; number of bootstrap resamples to use 
#' @param opts a character string containing additional options that should be feeded to the infomap algorithm
#' @param verbose integer; larger numbers produce more output
#' @param outer_loop_limit integer; number of times outer-most loop should be run to find optimal solution
#' @param core_loop_limit integer; number of times algorithm tries to allocate nodes into best possible module
#' @return returns a \code{data.table} object with the lower and upper bound of the bootrstrapped confidence interval of LinkRank modularity 
bootstrap_lrmod_mv = function(
    dat, 
    w_size,
    n_cores,
    n_resample,
    level = 0.95,
    opts = NULL,
    verbose = 0L,
    outer_loop_limit = 100,
    inner_loop_limit = 50)
{
    
    if (!requireNamespace("doParallel", quietly = TRUE))
        stop("need doParallel package to be installed")
    
    if (w_size %% 2 != 1)
        stop("w_size has to be an odd integer")
    
    if (!all(c("int_year", "occ1", "occ2", "obs_weights") %in% names(dat)))
        stop("dat must have columns int_year, obs_weights, occ1, and occ2")
    
    if (level <= 0 || level >= 1)
        stop("level has to be strictly between zero and one")
    
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
    
    # drop self-loops    
    df = dat[occ1 != occ2]

    # delete "iterator" if there exists any object under the same name
    if (exists('ii')) rm(ii)
    
    # register parallel backend 
    # notes: unfortunately, all messages will be suppressed
    registerDoParallel(n_cores)
    
    # stop cluster when exiting function
    on.exit(stopImplicitCluster())
    
    # matrix to store results
    q_mat = matrix(NA, nrow = nrow(periods), ncol = 2)
    colnames(q_mat) = c("lower", "upper")
    
    # start loop 
    for (ii in seq_len(nrow(periods))) {
        
        b_res = foreach (b = seq_len(n_resample), .combine = "c") %dorng% {
        
            # get period
            pp = periods[ii, ]
        
            # slice data by year
            df_sub = df[
                int_year >= pp["start"] & int_year <= pp["end"]
            ] 
        
            # resample (within each year)
            if (pp["end"] - pp["start"] > 0) {
                
                b_sizes = df_sub[
                    # calculate within year sample-size
                    , .(s_sizes = .N), by = int_year
                ][
                    # last index within year
                    , end_n := cumsum(s_sizes)
                ][
                    # first index within year minus one
                    , start_n_m1 := end_n - s_sizes
                ] %>%
                    # turn into matrix
                    as.matrix
                
                # sample with replacement within-year
                # note: for each year, index will start from 1
                b_samp = df_sub[
                    , .(samp = sample.int(.N, .N, replace = TRUE)), 
                    by = int_year
                ]
                
                # add sample-sizes to index to get the right bootstrap sample
                for (l in 1:NROW(b_sizes))
                    b_samp[
                        int_year == b_sizes[l, "int_year"],
                        samp := samp + b_sizes[l, "start_n_m1"]
                    ]
                
                # check bounds of indices
                tmp_df = b_samp[
                    , 
                    .(min = min(samp), max = max(samp)), 
                    by = int_year
                ]
                
                for (l in tmp_df$int_year) 
                    if (
                        tmp_df[int_year == l, min] < 
                        b_sizes[b_sizes[, "int_year"] == l, "start_n_m1"] + 1 ||
                        tmp_df[int_year == l, max] > 
                        b_sizes[b_sizes[, "int_year"] == l, "end_n"]
                    )
                        stop("bootstrap index out of limit")
                
                # check that within-year sample sizes are the same
                if (
                    !identical(
                        sort(df_sub[, .N, by = int_year]$N),
                        sort(df_sub[b_samp$samp, .N, by = int_year]$N)
                    )
                )
                    stop("sampling changed within-year sample sizes")
                
                # bootstrap sample
                b_dat = df_sub[
                    b_samp$samp,
                    .(weight = sum(obs_weights)),
                    by = .(occ1, occ2)
                ]
                
            } else {
                
                # sample with replacement
                b_samp = sample.int(
                    nrow(df_sub), nrow(df_sub), replace = TRUE
                )
                
                # bootstrap sample
                b_dat = df_sub[
                    b_samp, .(weight = sum(obs_weights)), by = .(occ1, occ2)
                ]
                
            }

            # generate graph (will create edge-attribute 'weight' automatically)
            g = igraph_from_df(b_dat[, .(occ1, occ2, weight)])
            
            # fit infomap
            i_res = fit_infomap(g = g, 
                              verbose  = 0, 
                              parallel = TRUE,
                              iterator = b)
            
            return(i_res$lr_modularity)
            
        }
        
        lower = 0.5 * (1 - level)
        upper = 1 - lower
        
        # store bootstrapped level% interval
        q_mat[ii, ] = quantile(b_res, probs = c(lower, upper))
        
        if (verbose > 0)
            message(
                paste0("Period ",
                       periods[ii, 1], "-", periods[ii, 3],
                       " done!")
            )
        
    }
    
    # create data.table to return
    res = data.table(q_mat)
    res[, mid_year := periods[, "mid"]]

    return(res)
    
}
    