#' Matching modules forward 
#' 
#' Starting at time t-1, labels are allocated to modules at time t based on the proportion of shared occupations across periods
#' 
#' @param mod_df a \code{data.table} object containing information regarding the modules across time. This object must contain columns with names \code{org, dest, props, count, period}
#' @param p_int an integer representing the period
#' @param threshold the proportion of shared occpations above which labels should be merged
#' @param verbose boolean; if TRUE, sends out messages about the process
match_modules_forward = function(mod_df, p_int, max_label, threshold = 0.5, verbose = FALSE) {

    
    if (!all(c("org", "dest", "props", "count", "period") %in% names(mod_df)))
        stop("mod_df has to contain columns org, dest, props, count, and period")
    
    if (verbose)
        message(paste0("Matching modules forwards for period ", p_int, " ..."))
    
    ## preliminary match ##
    
    # for each origin get destination that maximizes overlap
    tmp_df = mod_df[
        period == p_int,
        list(dest = dest[which.max(count)],
             count = count[which.max(count)]),
        by = org_labels
    ] %>%
        setnames("org_labels","tmp_labels")
    
    # if some destination is maximizer of more than one origin,
    # collapse labels separated by ".", given threshold threshold
    tmp_df = tmp_df[
        , 
        collapse_module_labels(.SD, threshold, include_count = FALSE), 
        by = dest
    ] %>% 
        setnames("V1", "dest_labels") %>%
        setorder(dest)
    
    if (any(tmp_df$threshold < threshold)) {
        
        no_match = which(tmp_df$threshold < threshold)
        message(
            paste0("No matching module with occupation share greater than ",
                   threshold, " found for module(s) ", 
                   paste0(no_match, collapse = ", "), "\n",
                   "threshold reduced to ", 
                   paste0(tmp_df[no_match]$threshold, collapse = ", "),
                   ifelse(length(no_match) > 1, ", respectively", "")
            )
        )
    }
    
    # remove threshold column
    tmp_df[, threshold := NULL]
                   
    
    ## single out destinations which are not matched ##
    
    # get full list of destinations
    max_dest = mod_df[period == p_int, max(dest,na.rm=T)]
    full_mod_seq = seq_len(max_dest)
    # missing destinations
    mod_mis = full_mod_seq[!(full_mod_seq %in% tmp_df$dest)]
    n_mis = length(mod_mis)
    
    # fill in missing destinations with new labels
    if (n_mis > 0)  {
        
        if (verbose)
            message("Assigning new labels to non-matched modules ...")
        
        new_max_label = max_label + n_mis
        tmp_df_2 = data.table(
            dest = mod_mis,
            dest_labels=(max_label + 1L):new_max_label
        )
        
        tmp_df = rbind(tmp_df, tmp_df_2) %>% 
            setorder(dest)
        
    } else {
        
        new_max_label = max_label
        
    }
    
    # match `dest` column with that of original dataset
    tmp_df_2 = tmp_df[match(mod_df[period == p_int]$dest, dest)]
    
    # check results
    if (!identical(tmp_df_2$dest, mod_df[period == p_int]$dest))
        stop("dest column matching failed")
    if (!identical(unique(tmp_df_2[order(dest)]), tmp_df[order(dest)]))
        stop("dest column matched but dest_label not matched")
    
    # add dest.labels column to original data.table
    mod_df[period == p_int, dest_labels := tmp_df_2$dest_labels]
    
    # return 
    return(list(data = mod_df, max_label=new_max_label))
    
}
