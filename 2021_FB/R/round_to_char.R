#' Round numeric objecto to string while keeping trailing zeros
#' 
#' Wrapper function to round a numeric value (or vector) to character string (or vector) for printing purposes keeping trailing zeros
#' 
#' @param x a numeric vector
#' @param digits integer; number of decimals to keep
#' @return character string/vector with \code{x} rounded to \code{digit} digits while keeping trailing zeros
round_to_char = function(x, digits = 1L) 
{
    
    if (!is.null(dim(x)))
        stop("dimension of x has to be NULL")
    
    if (digits %% 1 != 0) 
        stop("digits has to be an integer value")
    
    return(sprintf(paste0("%.", digits, "f"), round(x, digits)))
    
}