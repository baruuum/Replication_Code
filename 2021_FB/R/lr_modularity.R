#' LinkRank Modularity
#' 
#' Calculates the LinkRank modularity (Kim et al. 2010) of a graph partition
#' 
#' @param g a \code{igraph} object
#' @param partition a graph partition; should be an \code{integer} or \code{character} vector of memberships or a \code{igraph::communities} object
#' @param damping damping factor (1 - teleportation prob.)
#' @param pr_algo algorithm to calculate Perron vector; should be one of "prpack", "arpack", and "power"; defaults to "prpack".
#' @param weight if \code{NULL} and \code{g} has a \code{weight} edge-attribute, then it is used. If \code{weights} is a numerical vector, with length equal \code{igraph::ecount(g)}, then it will be used even if the object \code{g} has a \code{weight} edge-attribute. If \code{weight} is \code{NA}, then no weights will be used, regardless of the attributes of \code{g}.
lr_modularity = function(
    g,
    partition, 
    damping = .85, 
    pr_algo = "prpack",
    weight = NULL
) {
    
    # check args
    if (!igraph::is.igraph(g)) 
        stop('graph is not an igraph object')
    
    if (damping > 1 | damping < 0) 
        stop('damping factor has to be between zero and one')
    
    # get algorithm name to calculate Perron vector
    pr_algo = match.arg(pr_algo, c('prpack','arpack','power'))
    
    # no of nodes
    n = igraph::vcount(g)

    # get membership vector
    if (class(partition) == "communities") {
        
        pp = igraph::membership(partition)
        
    } else if (is.integer(partition)) {
      
        pp = partition
      
    } else if (is.character(partition)) {
      
        pp = as.integer(factor(partition))
      
    } else {
      
        stop("partition has to be a communities object, an integer vector, or a character vector")
      
    }

    
    # check dimensions
    if (length(pp) != n) 
        stop("Length of membership vector differs from number of nodes")
    
    # get adjacency matrix & out-degree
    if (is.vector(weight) & length(weight) > 1) {
        
        # check args
        if (igraph::ecount(g) != length(weight))
            stop("weight differes in length from ecount")
        if (!is.numeric(weight))
            stop("weight must be NA, NULL, or a numeric vector")
        if (any(weight < 0))
            stop("weight vector has to be positive")
        
        igraph::edge_attr(g, "tmp") = weight
        A = igraph::get.adjacency(g, type = "both", attr = "tmp")
        out_deg = igraph::strength(g, mode = "out", weights = weight)
        
    } else if (is.null(weight)) {
        
        if ("weight" %in% igraph::edge_attr_names(g)) {
            
            if (any(igraph::E(g)$weight < 0))
                stop("weight edge_attribute has to be positive")
            A = igraph::get.adjacency(g, type = "both", attr = "weight")
            out_deg = igraph::strength(g, mode = "out")
            
        }  else {
            
            A = igraph::get.adjacency(g, type = "both")
            out_deg = igraph::degree(g, mode = "out")
            
        }
        
    } else if (is.na(weight)) {
        
        A = igraph::get.adjacency(g, type = "both")
        out_deg = igraph::degree(g, mode = "out")
        
    } else {
        
        stop("weight option has to be NA, NULL, or a numeric vector")
        
    }
    
    # dead-end nodes
    dangling = out_deg == 0
    
    # row-normalize A (recycle vector)
    G.temp = A / out_deg

    # set rows for dead-end nodes to zero
    if (sum(dangling) > 0) 
        G.temp[dangling,] = 0

    # add teleportation probabilities
    Tmat <- Matrix::Matrix(1 / n * (damping * dangling + 1 - damping), 
                           nrow = n, ncol = n)
    G = damping * G.temp + Tmat
    
    # get Perron vector (PageRank)
    p_vec = igraph::page_rank(
            g, damping = damping, algo = pr_algo, weights = weight
        )$vector
    
    # LinkRank matrix
    Q = G * p_vec -  tcrossprod(p_vec)

    # get LinkRank Modularity by summing over within-community weights
    res = .adj_sum_partition(as.matrix(Q), pp)
    
    return(res)
  
}

