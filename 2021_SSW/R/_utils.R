################################################################################
##
## Utilities
##
################################################################################

#' Reformate numbers into strings
#' 
#' @param x object to reformat
#' @param digits number of decimal points to display
#' @return \code{x} as a string with numbers rounded to \code{digits} decimal points
format_str = function(x, digits) {
    
    # round to third decimal and round
    res = format(
            round(x, digits), 
            trim = T, 
            digits = digits, 
            decimal.mark = ".", 
            big.mark=","
        ) 
        
    # drop trailing zeros for (for integers)
    return(gsub("\\.000", "", res))
    
}

#' Print session info as message
#' 
#' @return prints current session info as a \code{message}
print_sessionInfo = function() {
    
    message(
        paste0(
            capture.output(
                print(sessionInfo())
            ), 
            collapse = "\n"
        )
    )
    
}

#' Update denominator for vertex related statistics
#' 
#' Updates summary network measures based on new total count of nodes
#' 
#' @param x the summary statistic to update
#' @param old_n old vertex count
#' @param new_n new vertex count
#' @return returns updated summary measure
update_denom_node = function(x, old_n, new_n) {
    
    (x * old_n) / new_n
    
}


#' Update denominator for edge related statistics
#'
#' Updates summary network measures based on new total count of nodes
#'
#' @param x the summary statistic to update
#' @param old_n old vertex count; should be an integer when \code{bipartite == FALSE} and an integer vector of 2 (one for each mode) when \code{bipartite == TRUE}
#' @param new_n new vertex count; should be an integer when \code{bipartite == FALSE} and an integer vector of 2 (one for each mode) when \code{bipartite == TRUE}
#' @param bipartite if TRUE treats graph as bipartite
#' @return returns updated summary measure
update_denom_edge = function(x, old_n, new_n, bipartite) {

    if (bipartite) {

        stopifnot(length(new_n) == 2L && length(old_n) == 2L)

    } else {

        stopifnot(length(new_n) == 1L && length(old_n) == 1L)

    }

    if (bipartite) {

        return(x * prod(old_n) / prod(new_n))

    }

    # total of possible edges (assuming symmetric ties)
    ope = 0.5 * old_n * (old_n - 1)
    npe = 0.5 * new_n * (new_n - 1)

    return(x * ope / npe)

}


#' #' Update denominator for edge related statistics
#' #' 
#' #' Updates summary network measures based on new total count of nodes (treating bipartite graphs as simple graphs as well)
#' #' 
#' #' @param x the summary statistic to update
#' #' @param old_n old vertex count
#' #' @param new_n new vertex count
#' #' @return returns updated summary measure
#' update_denom_edge = function(x, old_n, new_n) {
#'     
#'     # total of possible edges (assuming symmetric ties)
#'     ope = 0.5 * old_n * (old_n - 1) 
#'     npe = 0.5 * new_n * (new_n - 1)
#'     
#'     return(x * ope / npe)
#'     
#' }

#' Update betweenness centralization of a graph 
#' 
#' Updates betweenness centralization of graph when isolated nodes are added
#' 
#' @param x a list object created by \code{igraph::centr_betw(g, directed = FALSE)} where \code{g} is the original graph 
#' @param new_n new node count of graph 
#' @return returns an updated betweenness centralization index of a graph \code{g} with \code{new_n - igraph::vcount(g)} isolated nodes added
update_centr_betw = function(x, new_n) {
    
    # check arguments
    stopifnot(
        length(x) == 3L,
        all(names(x) %in% c("res", "centralization", "theoretical_max"))
    )
    
    # old betweenness scores
    old_betw = x$res
    
    # number of (isolated) nodes to add
    n_iso = new_n - length(old_betw)
    
    # new betweenness score vector
    new_betw = c(old_betw, rep(0.0, n_iso))
    
    # check
    stopifnot(length(new_betw) == new_n)
    
    # maximum possible score
    th_max = choose(new_n - 1L, 2L) * (new_n - 1L)
    
    # return new centralization score
    return(sum(max(new_betw) - new_betw) / th_max)
    
}
