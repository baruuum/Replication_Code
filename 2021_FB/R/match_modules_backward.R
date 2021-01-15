#' Matching modules backward
#' 
#' Starting at time t+1, labels are allocated to modules at time t based on the proportion of shared occupations across periods
#' 
#' @param mod_df a \code{data.table} object containing information regarding the modules across time. This object must contain columns with names \code{org, dest, props, count, period}
#' @param p_int an integer representing the period
#' @param p_min an integer representing the first period in the analysis
#' @param threshold the proportion of shared occpations above which labels should be merged
#' @param verbose boolean; if TRUE, sends out messages about the process
match_modules_backward = function(mod_df, p_int, p_min, max_label, threshold = 0.5, verbose = FALSE) {
    
    if (!all(c("org", "dest", "props", "count", "period") %in% names(mod_df)))
        stop("mod_df has to contain columns org, dest, props, count, and period")
    
    if (verbose)
        message(paste0("Matching modules backward for period ", p_int, " ..."))
    
    ## preliminary match ##
    
    # for each destination get origins in period == p_int
    tmp_df = mod_df[
        period == p_int,
        list(tmp_labels = org_labels, count), 
        by = dest
    ] 
    
    # if some destination has more than one origin that has overlap greater than threshold, 
    # combine labels with "." separator
    if (all(!is.na(tmp_df$count))) {
        
        # if there are no NAs in "count" run
        tmp_df = tmp_df[
            , 
            collapse_module_labels(.SD, threshold, include_count = T), 
            by = dest
        ] %>%
            setnames(c("V1", "V3"), c("dest_tmp", "count")) %>%
            setorder(dest_tmp, -count)
        
    } else { # if there ARE NAs, this implies that there is an unmatched cluster
        
        message(paste0("Backwards Matching: Cluster with NA counts ",
                       "in mapping period ", p_int-1,
                       " to ", p_int, " detected \n",
                       "This should only happen if there exists a ",
                       "cluster that was not mapped to any other ",
                       "in this time interval"))
        
        miss_mod = tmp_df[which(is.na(tmp_df$count)), dest]
        
        tmp_df = tmp_df[
            !is.na(count),
            collapse_module_labels(.SD, threshold, include_count = T), 
            by = dest
        ] %>%
            setnames(c("V1", "V3"), c("dest_tmp", "count")) 
            
        
        for (cc in seq_along(miss_mod)) {
            
            tmp_df = rbind(tmp_df, list(miss_mod[cc], max_label + 1,NA))
            max_label = max_label + 1
        }
        
        setorder(tmp_df, dest_tmp, -count)
        
    }
    
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
    
    # remove threshold variable
    tmp_df[, threshold := NULL]
    
    # if there are clusters that share the same dest_labels
    # (which means that most of the occupations in that cluster have the same
    # origin(s) ) allocate sub-labels sorted by cluster size
    
    # generate sub-labels (recall that tmp_df is ordered by dest_label)
    tmp_df[, tmp_sub := seq_len(.N), by = dest_tmp]
    
    # generate preliminary labels
    tmp_df[
        , dest_labels := ifelse(
            tmp_sub==1, 
            paste0(dest_tmp), 
            paste0(dest_tmp, "(", tmp_sub, ")")
        )
    ]
    
    # single-out sublabels which are not direct childrens and assign new label
    if (p_int > p_min) {
        
        # get dest_labels from past period
        o_labs = mod_df[period == (p_int - 1), dest_labels]
        # single out root-module with overlapping sub-labels
        o_root = tmp_df[dest_labels %in% o_labs & tmp_sub != 1, unique(dest_tmp)]
        
        if (length(o_root) > 0) {
            
            # get first sub-label
            tmp_sub = purrr::map(o_root, function(w) 
                {
                
                    if (grepl("\\(", w)) 
                        w = gsub("\\(", "[(]", w) %>% 
                            gsub("\\)", "[)]", .)
                    
                    tmp_str = tmp_df[, dest_labels]
                    res = ifelse(
                        grepl(paste0("^", w, "[(](.*?)[)].*$"), tmp_str),
                        sub(paste0("^", w, "[(](.*?)[)].*$"), "\\1", tmp_str),
                        "nomatch"
                    )
                    
                    res[res != "nomatch"]
                }
            )
            
            # get maximum number of sublabels for root
            m_l = purrr::map_dbl(tmp_sub, ~ max(as.numeric(.x)))
            
            # add new sub-labels
            for (vv in seq_along(m_l)) {
                
                tmp_df[
                    dest_labels %in% o_labs & dest_tmp == o_root[vv] & tmp_sub != 1, 
                    dest_labels := paste0(dest_tmp, "(", seq_len(.N) + m_l[vv], ")")
                ]
            }
        }
    }
    
    
    # if there is overlap in the tmp_labels, relable them
    if (length(tmp_df[, unique(dest_labels)]) != length(tmp_df[, unique(dest)])) {
        
        while(length(tmp_df[, unique(dest_labels)]) != length(tmp_df[, unique(dest)])) {
            
            message("!! Caution: reached second stage of relabeling !!")
            # get duplicated labels (dup==2)
            tmp_df[, dup:=nrow(.SD), by = dest_labels]
            
            # order mod_dfa by tmp_sub
            # note: when temp.sup==1, the destination is the "max-count" child
            tmp_df = tmp_df[order(tmp_sub)]
            
            # generate second sub-label
            tmp_df[dup >1 ,tmp_sub_2 := seq_len(.N), by = dest_labels]
            
            # add second sub-label
            tmp_df[dup>1, dest_labels:=ifelse(tmp_sub_2 == 1,
                                              dest_labels,
                                              paste0(dest_labels, "(", tmp_sub_2, ")"))
            ]
            
            # recover old order
            tmp_df[order(dest_tmp, -count)]
            
        }
    }
    
    # remove unnecessary columns and sort by dest
    tmp_df = tmp_df[, list(dest, dest_labels)] %>%
        setorder(dest)
    
    
    ## single out destinations which are not matched ##
    
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
    
    ## return results ##
    
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
