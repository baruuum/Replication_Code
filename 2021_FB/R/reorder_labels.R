#' Reorder categories according to frequency
#' 
#' Takes a integer, character, or factor vector and reorder the labels according to the frequency with which they appear.
#' 
#' @param x integer or character vector, or factor variable
#' @param decreasing boolean; if TRUE, orders in decreasing order, otherwise in increasing order
#' @return returns \code{x} with labels ordered according to frequencies
#' @details It is assumed that the labels of \code{x} have an inherent ordering. \code{reorder_labels} reorders the labels such that the first label is assigned to the category with the highest frequency, the second label to the second most frequent category, and so on.
reorder_labels = function(x, decreasing = TRUE)
{
    
    # get class of x
    x_class = class(x)
    
    if (x_class == "character") {    
        
        freq = table(x)
        labs = names(freq)
        l_tab = labs[order(freq, decreasing = decreasing)]
        
        return(labs[match(x, l_tab)])
        
    } else if (x_class == "integer") {
        
        match(x, order(table(x), decreasing = decreasing))
        
    } else if (x_class == "factor") {
        
        freq = table(x)
        labs = names(freq)[order(freq, decreasing = decreasing)]
        
        return(factor(x, levels = labs))
        
        
    } else {
        
        stop("x must be either a character or integer vector, or a factor variable")
        
    }
    
}