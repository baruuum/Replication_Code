#' Create a mapping between factor/character vector and corresponding numeric values
#' 
#' @param x a \code{factor} or \code{character} vector
#' @return returns a two dimensional \code{data.table}, where the first column is the numeric value and the second column the corresponding label
map_labels = function(x) {
    
    if (!requireNamespace("gdata", quietly = TRUE))
        stop("requires gdata package to be installed")
    
    lmap = gdata::mapLevels(x)
    
    lab_mapping = data.table(
        id   = as.integer(lmap), 
        label = names(lmap)
    )
    
    return(lab_mapping)
    
}