#' Drop module from partition
#' 
#' Drops a module from a partition
#' 
#' @param x integer corresponding to the module number that should be dropped
#' @param partition a partition found by the \code{fit_infomap} function
#' @param keep boolean; if TRUE, the function drops all other modules except \code{x}
#' @return returns a list containing the graph and module-memberships vector of the reduced graph, the number of the dropped module, and the ids of the dropped occupations
drop_module = function(x, partition, keep = FALSE) 
{
    
    # get graph, memberships, and vertex sequence
    g = partition$graph
    m = membership(partition)
    V = V(g)
    
    if (!(x %in% m))
        stop("x cannot be found in membership vector")

    if (x > length(partition)) 
        stop("x is larger than the number of modules in partition")
    
    # vertices to drop
    drop_v = if (keep)  V[m != x] else V[m == x]
    n_drop = length(drop_v)
    
    # new network
    new_g = delete_vertices(g, drop_v)
    new_m = if (keep) m[m == x] else m[m != x]
    
    # check sizes 
    if (vcount(new_g) != vcount(g) - n_drop)
        stop("mismatch in graph size after dropping module")
    
    if (length(new_m) != length(m) - n_drop)
        stop("mismatch in membership vector length after dropping module")
    
    # check dropped occs
    if (keep) {
        
        if (sum(unique(m[names(m) %in% names(drop_v)]) == x) > 0)
            stop("occupations from wrong module are dropped")
        
    } else {
        
        if (unique(m[names(m) %in% names(drop_v)]) != x)
            stop("occupations from wrong module are dropped")
        
    }
    
    return(
        list(graph           = new_g,
             membership      = new_m,
             dropped_cluster = x,
             dropped_occs    = names(drop_v))
    )
}
