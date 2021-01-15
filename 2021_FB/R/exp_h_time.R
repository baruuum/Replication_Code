#' Function to calculate expected hitting times of first-order homogenous Markov process
#' 
#' @param X row-normalized adjacency matrix
#' @param org numeric identifier of the origin
#' @param dest vector of numeric identifiers of destinations
#' @return vector of expected hitting times
exp_h_time = function(X, org, dest) {
    
    r_names = rownames(X)

    if (any(r_names != colnames(X)))
        stop("row and column names differ")
    
    if (org %in% r_names == F)
        stop("origin not found")
    
    if (!all(dest %in% r_names))
        stop("destination not found")
    
    if (
        !isTRUE(
            all.equal(
                rowSums(X), 
                rep(1.0, nrow(X)), 
                check.attributes = FALSE
            )
        )
    )
        stop("Rows of X do not sum to one")
    
    # reorder matrix
    X_re = X[
        c(org, r_names[r_names != org]),
        c(org, r_names[r_names != org])
    ]
    new_names = rownames(X_re)
    
    res = purrr::map_dbl(
        dest, 
        function(v) 
        {
            Q = X_re[new_names != v, new_names != v]
            H = solve(diag(nrow(Q)) - Q)
            
            return(sum(H[1, ]))
            
        }
    )
    
    return(res)
    
}
