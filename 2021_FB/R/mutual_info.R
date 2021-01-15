#' Mutual Information
#' 
#' Calculates Variants of the mutual information of the rows and the columns of a cross table
#' 
#' @param x a cross table of frequencies; has to be a integer matrix or table object
#' @param normalized a string that specifies how the mutual information should be normalized. Must be one of either "none", "min", "max", "arithmetic", "geometric", or "harmonic." 
#' @param adjusted if TRUE, calculates the adjusted version based on the hypergeometric model of randomness. Defaults to FALSE
#' @details The \code{normalized} option specifies how the row and column entropies are used to normalize the mutual information. For example, with \code{"min"}, the normalized mutual information (NMI) would be \eqn{I(R, C) / \min[H(R), H(C)]}, where \eqn{I(R, C)} is the mutual information between the row and the column, and \eqn{H(R)} and \eqn{H(C)}, respectively, are entropies of the row and column marginals. 
#' When \code{adjusted} is set to \code{TRUE}, the adjusted mutual information, as suggested by Vinh et al. (2010), is returned. If \code{what == "none"} and \code{adjusted == TRUE}, the function returns \eqn{I(R,C) - E[I(R,C)]}.
#' @references Nguyen Xuan Vinh, Julien Epps, and James Bailey. 2010. "Information Theoretic Measures for Clusterings Comparison: Variants, Properties, Normalization and Correction for Chance," \emph{Journal of Machine Learning Research} 11: 2837-2854.
#' @examples 
#' data(sampson3, package = "btoolbox")
#' 
#' info_part = igraph::cluster_infomap(sampson3)
#' groups = igraph::vertex_attr(sampson3, "group")
#' print(tab <- table(groups, info_part$membership))
#' 
#' mutual_info(tab, normalize = "geometric", adjusted = TRUE)
#' @export
mutual_info = function(x, normalized = "none", adjusted = FALSE)
{
    
    # check input
    if (class(x) == "table")
        x = as.matrix(x)
    
    if (typeof(x) != "integer") 
        stop("x has to be of integer type")
    
    what = match.arg(normalized, 
                     choices = c("none", "min", "max", 
                                 "arithmetic", "geometric", "harmonic"),
                     several.ok = FALSE)
    
    # get mutual information
    mi = .mutual_info(x)
    
    if (what == "none") 
        if (!adjusted) { return(mi) } else { return(mi - .emi_int(x))}
    
    # marginal entropies
    erow = .entropy(rowSums(x))
    ecol = .entropy(colSums(x))
    
    # normalizing constant
    norm = switch(what,
                  min = min(erow, ecol),
                  max = max(erow, ecol),
                  arithmetic = 0.5 * (erow + ecol),
                  geometric  = sqrt(erow * ecol),
                  harmonic   = 2.0 * erow * ecol / (erow + ecol)
    )
    
    if (!adjusted)
        return(mi / norm)
    
    # expected mutual information
    emi = .emi_int(x)
    
    return((mi - emi) / (norm - emi))
    
}