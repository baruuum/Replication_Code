#' Change first character of string to upper case
#' 
#' @param x string to modify
#' @param rest_to_lower boolean; if TRUE, changes all other characters to lower case
#' @return returns \code{x} with the first character changed to upper-case
upper_first_char = function(x, rest_to_lower = FALSE) 
{
    
    x1 = toupper(substr(x, 1, 1))
    x2 = if (rest_to_lower) {
            tolower(substr(x, 2, nchar(x)))
        } else {
            substr(x, 2, nchar(x))
        }
    
    return(paste0(x1, x2))

}