#' Collapsing origin/destination module labels together based on given threshold
#' 
#' @param mod_df_sub a \code{data.table} object
#' @param threshold threshold value for minimum proportion of shared occupations
#' @param include_count boolean; if TRUE, the number of shared occupations are returned as well
#' @details 1) If there are no modules that share more than a \code{threshold} proportion of occupations, \code{threshold} is reduced by 0.01 until the criterion is met. 2) If two module labels are collapsed together, while \code{include_count == TRUE}, then the sum of the count of share occupations is returned. 
collapse_module_labels = function(mod_df_sub, threshold, include_count = FALSE) {
    
    # sort by count
    x = mod_df_sub[order(-count)]
    
    # calculate share of occupation shared
    pr = x$count / sum(x$count)
    
    # reduce threshold if there is no module with overlapping share bigger than threshold
    while (all(pr < threshold)) 
        threshold = threshold - 0.01

    
    if (include_count) {
        
        res = x[
            which(pr >= threshold)
        ][
            , list(paste0(tmp_labels, collapse = "."), threshold, sum(count))
        ]
        
    } else {
        
        res = x[
            which(pr >= threshold)
        ][
            , list(paste0(tmp_labels, collapse = "."), threshold)
        ]
        
    }
    
    return(res)
    
}
