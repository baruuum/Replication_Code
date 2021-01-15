#' Function to extract modularity measures from list of \code{communities} objects into a \code{data.table}
#' 
#' @param mv_list a list of \code{comminities} objects
#' @param what what to extract. Options are "both", "lr" (LinkRank modularity), and "w" (Weighted modularity)
#' @return returns a \code{data.table} object of extracted modularity measures
mv_to_modularity_dt = function(mv_list, what = c("both", "lr", "w"))
{
    
    what = match.arg(what, several.ok = FALSE)
    
    res = switch(
        what,
        both = purrr::map(
                    mv_list, 
                    ~ c(.x$lr_modularity, .x$w_modularity)
               ) %>%
               do.call("rbind", .) %>%
               data.table %>%
               setnames(c("lr_modularity", "w_modularity")), 
        lr   = purrr::map_dbl(mv_list, ~.x$lr_modularity) %>%
               data.table %>%
               setnames("lr_modularity"),
        w    = purrr::map_dbl(mv_list, ~ .x$w_modularity) %>%
               data.table %>%
               setnames("w_modularity")
    )
    
    
    if (!is.null(names(mv_list))) {
        
        res[, mid_year := as.integer(gsub("midyear_", "", names(mv_list)))]
        
    } else {
        
        message("mv_list has no name attribute ... years column not generated")
        
    }
    
    return(res)
    
}