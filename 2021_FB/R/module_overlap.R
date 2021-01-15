#" Matching modules over time and calculates degrees of overlap
#" 
#" Matches modules over consecutive periods and calculates the overlap in occupations
#" 
#" @param mod_list a list of modules detected over time. The first element of the list is assumed to be detected modules of the first period, the second element that of the second period, and so on
#" @param forward boolean; if TRUE, modules are matched starting from t = 1 *to* t = 2, and so on. Otherwise, modules are match from t = 2 *to* t = 1, t = 3 *to* t = 2, and so on. See details.
#" @return a \code{data.table} that contains 1) module labels at time \code{t}, 2) module labels at time \code{t + 1}, 3) proportion of overlapping occupations, 4) number of overlapping occupations, 5) and the period \code{t}.
#" @details  matching forwards the period starts at time \code{1} and ends with \code{T - 1}, where \code{T} is the last period under analysis; matching backwards starts at time \code{2} and ends with \code{T}. The reason for this is that matching forwards matches modules at period \code{t} (i.e., ancestors) to those at time \code{t + 1} (childern), while matching backwards matches modules at time \code{t + 1} (childrens) with those at time \code{t} (i.e., ancestors)
module_overlap = function(mod_list, forward = TRUE) {
    
    # number of periods
    n_periods = length(mod_list)
    
    dat = purrr::map(seq_len(n_periods - 1L), function(k) 
        {
        
            # matching forwards or backwards?
            if (forward) {
                
                p1 = mod_list[[k]]
                p2 = mod_list[[k + 1]]
                
            } else {
                
                p1 = mod_list[[k + 1]]
                p2 = mod_list[[k]]
                
            }
        
            ov_dat = purrr::map(seq_along(p1), function(v) 
            {
            
                # match modules and calculate overlap
                props = purrr::map_dbl(seq_along(p2), function(w) 
                {
                    mean(p1[[v]] %in% p2[[w]])
                })
                
                counts = purrr::map_int(seq_along(p2), function(w)
                {
                    sum(p1[[v]] %in% p2[[w]])
                })
                
                # which pairs have non-zero overlaps?
                non_zeros = which(counts > 0)
            
                # return data.table where the first column is the number of module
                # "v"(k) second column are the modules at time point k+1 with
                # which "v" shares at least one occupation and the third
                # column contains the proportions (counts) of overlapping occupations
                
                if (length(non_zeros) > 0) {
                    
                    if (forward) {

                        res = data.table(org   = rep(v, length(non_zeros)),
                                         dest  = non_zeros,
                                         props = props[non_zeros],
                                         count = counts[non_zeros]) 
                    
                    }  else {
                    
                        res = data.table(org   = non_zeros,
                                         dest  = rep(v, length(non_zeros)),
                                         props = props[non_zeros],
                                         count = counts[non_zeros])
                    } 
                    
                } else {
                    
                    res = data.table(org = NA, dest = v, props = NA, count = NA)
                    
                }
                
                return(res)

            }
            
        ) %>% 
            rbindlist
            
        ov_dat$period = if (forward) k else k + 1
        
        # check results
        if (forward) {
            
            if (sum(seq_along(p2) %in% ov_dat$dest) != length(p2)) {
                
                message(paste0("Some modules in period ", k + 1,
                               " are not matched at all (Forward matching)"))
                message("Occupations for which no match could be found are the following :")
                no_match_mod = which(seq_along(p2) %in% ov_dat$dest == F)
                
                for (cc in no_match_mod) {
                    
                    message(paste0("Cluster #", cc))
                    
                    no_match_occs = p2[[cc]] %>% sort
                    
                    message(paste0(no_match_occs, collapse="\n"))
                    message("\n")
                    
                    # fill destination with missing values
                    ov_dat = rbind(ov_dat, list(NA, cc, NA, NA, k))
                    
                }
                
            }
            
        } else {
            
            if (sum(seq_along(p2) %in% ov_dat$org) != length(p2)) {
                
                message(paste0("Some clusters in period ", k,
                               " are not matched at all (Backward matching)"))
                message("Occupations for which no match could be found are the following :")
                
                no_match_mod = which(seq_along(p2) %in% ov_dat$org == F)
                
                for (cc in no_match_mod) {
                    
                    message(paste0("Cluster #", cc))
                    
                    no_match_occs = p2[[cc]] %>% sort
    
                    message(paste0(no_match_occs, collapse="\n"))
                    message("\n")
                    
                    # fill origin with missing values
                    ov_dat = rbind(ov_dat, list(cc, NA, NA, NA, k + 1))
                }
                
            }
        }
        
        return(ov_dat)
        
    }) %>% 
        rbindlist
    
    return(dat)
    
}
