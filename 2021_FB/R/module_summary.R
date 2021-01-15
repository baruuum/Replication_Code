#' Summarize detected modules
#' 
#' Summarize occupations within each module, most within-module-central occupations, size of modules, and the connectedness of the subgraph within each module
#' 
#' @param partition a \code{communities} object created through the \code{fit_infomap} function
#' @param mapping a \code{data.table} object that contains the mapping from \code{occ_id} to \code{occ_name} according to which labels should be standardized
#' @param verbose integer indicating how much output should be produced; higher values create more output
#' @return returns a summary of the clusters
#' @detail the returned \code{list} object has two elements. \code{central_occs} summarizes the most central occupation within each module and some basic information about the modules. \code{full_list} enumerates all occupations included in each module. The \code{full_list$occ_id} vector contains the numeric lables of each occupation as provided by the \code{mapping} argument.
module_summary = function(partition, mapping, verbose = 0) 
{
    
    ## number of communities
    n_modules = length(partition)
    
    res = purrr::map(seq_len(n_modules), function(w) {
        
        if (verbose > 0)
            message(paste0("Working on module ", w," ..."))
        
        # new network by dropping community w
        m_reduced = drop_module(w, partition, keep = T)
        g_reduced = m_reduced$graph
        
        # community size
        m_size = length(m_reduced$membership)
        
        # construct list of centrality measures
        max_list = vector("list", 8L)
        names(max_list) = c("closeness",
                            "eigenvector",
                            "pagerank",
                            "in_strength", 
                            "out_strength", 
                            "in_degree",
                            "out_degree", 
                            "betweenness")
        
        if (m_size == 1) {
            
            res = matrix(c(names(m_reduced$membership), NA, NA, NA, NA), nrow = 1)
            
        } else {
        
            e_weight = E(g_reduced)$weight
            
            max_list[[1]] = closeness(g_reduced,
                                      weights = e_weight)
            max_list[[2]] = eigen_centrality(g_reduced, 
                                             directed = T, 
                                             weights = e_weight
                                             )$vector
            max_list[[3]] = page.rank(g_reduced, 
                                      directed = T, 
                                      weights=e_weight
                            )$vector
            max_list[[4]] = strength(g_reduced, 
                                     mode = "in",
                                     weights = e_weight) 
            max_list[[5]] = strength(g_reduced, 
                                     mode="out",
                                     weights=e_weight)
            max_list[[6]] = degree(g_reduced, mode = "in") 
            max_list[[7]] = degree(g_reduced, mode = "out")
            max_list[[8]] = betweenness(g_reduced, 
                                        directed = T, 
                                        weights = e_weight) 
            
            if (sum(purrr::map_chr(max_list, class) != "numeric") > 0) 
                stop("non-numeric vector in max_list")
    
            # flag whether graph is strongly connected
            connected = if (is.connected(g_reduced, mode = "strong")) {
                "strongly connected" 
            } else if (is.connected(g_reduced, mode = "weak")) {
                "weakly connected"
            } else {
                "disconnected"
            }
                
            # get occupation that have maximum centrality
            max_occs = purrr::map_chr(max_list, ~ names(which.max(.x)))
            # get unique occupations 
            unique_occs = unique(max_occs)
            
            # generate matrix of results
            res = purrr::map(unique_occs, function(x) 
                {
                    occ_name = x 
                    centrality = paste0(names(max_list)[max_occs == x], collapse=", ")
                    return(c(x, centrality, w, m_size, connected))
                }
            ) %>%
                do.call(rbind, .)
        }
        
        colnames(res) = c("occ_name", "max_centrality", "module",
                          "module_size", "connected")
        
        # heuristically order by length of max_cent
        if (m_size > 1)
            res = res[order(-nchar(res[, 2])), , drop = FALSE]
        
        if (verbose > 1) {
            
            if (NROW(res) == 1) {
                df_to_message(
                    as.data.table(t(res[, c("occ_name", "module", "module_size")]))
                )
            } else {
                
                df_to_message(
                    as.data.table(res[, c("occ_name", "module", "module_size")])
                )
                
            }
            
            message("\n")
            
        }
        
        # create data.table of vertices in module
        v_list = data.table(
            occ_id   = mapping[mapping$occ_label %in% names(V(g_reduced))]$occ_id,
            occ_name = names(V(g_reduced))
        )
        
        # check for missing values
        if (any(is.na(v_list))) 
            stop("missing values in vertex list")

        return(list(central_occs = res, full_list = v_list))
        
    }) 
    
    names(res) = paste0("module_", seq_len(n_modules))
    
    return(res)
    
}