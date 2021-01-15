#' Get p_vec and create labels
#' 
#' @param p_vec character vector, each element of which is a string with 4 chars
#' @param full  boolean; if TRUE creates full years
#' @param cut_val value that determines whether to add a "19" or a "20" to the labels
#' @return returns period labels by adding a "-" between the second and third char
gen_p_labs = function(x, full = FALSE, cut_val = 5) 
{
    l1 = substr(x, 1, 2)
    l2 = substr(x, 3, 4)
    
    if (!full)
        return(paste0(l1, "-", l2))
    
    l1 = ifelse(substr(l1, 1, 1) > cut_val, paste0("19", l1), paste0("20", l1))
    l2 = ifelse(substr(l2, 1, 1) > cut_val, paste0("19", l2), paste0("20", l2))
    
    return(paste0(l1, "-", l2))
    
}