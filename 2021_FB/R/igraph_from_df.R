#' Create \code{igraph} object from \code{data.table}
#' 
#' Creates an \code{igraph} object from \code{data.table} with the option to save the mapping from the occupation names to the numeric ids
#' 
#' @param x a \code{data.table/data.frame} object; this object should have three columns containing information regarding the sender, receiver, and the weight, respectively
#' @param save_labels boolean; saves mapping from labels in \code{x} to numeric ids used in the graph
#' @param file file path where the mapping should be saved; if \code{save_labels == TRUE}, this option must be specified
#' @return returns a graph created from \code{x}
igraph_from_df = function(x, save_labels = FALSE, file = NULL)
{
    
    # check data structure
    if (ncol(x) != 3L) 
        stop("x must have three columns: sender, receiver, weight")
    
    if (!is.numeric(x[[3]]))
        stop("third column of dat has to be numeric")
    
    for (j in 1:2)
        if (!is.character(x[[j]]))
            if (!all(x[[j]] %% 1 == 0))
                stop(paste0("column ", j, " of x has to be either character or integer"))
    

    # check output specifications
    if (save_labels && is.null(file))
        stop("save_labels == TRUE but no file_path is specified")
    
    # create igraph object
    names(x) = c("sender", "receiver", "weight")
    g = graph_from_data_frame(x)
    
    if (save_labels)
        fwrite(
            x = data.table(
                occ_names = attr(V(g), "names"), 
                occ_id = V(g)
            ),
            file = file
        )
    
    return(g)

}
