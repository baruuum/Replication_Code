#' Extract r-squared value from \code{lm} object
#' 
#' @param x an \code{lm} object
#' @return returns the r-squared value
get_r_squared = function(x)
{
    
    if (class(x) != "lm")
        stop("get_r_squared works only on lm objects")
    
    # get r-squared
    res = unlist(summary(x)["r.squared"])
    
    # drop name attribute
    names(res) = NULL
    
    # return
    return(res)
    
}