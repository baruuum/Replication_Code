#' Restructure module summary
#' 
#' Restructures an object created by \code{module_summary} into a list of \code{data.table}s
#' 
#' @param mod_summary a summary of modules created by the \code{module_summary} function
#' @return returns a list of \code{data.table}s
module_summary_to_dt_list = function(mod_summary) {
    
    # no. cols of summary
    n_cols = ncol(mod_summary[[1]]$central_occs)
    # no. cols of vertex list
    n_cols_v = ncol(mod_summary[[1]]$full_list)
    
    # generate excel-friendly format
    xlsx_sum = purrr::map(mod_summary, function(w) {
        
        n_row_v = nrow(w$full_list)
        tmp_mat = matrix(
            NA, 
            nrow = n_row_v, 
            ncol = n_cols - n_cols_v
        )
        
        res_mat = cbind(
            as.matrix(w$full_list), 
            tmp_mat
        ) %>% 
            rbind(
                w$central_occs, 
                rep(NA, n_cols),
                c('Full List', rep(NA, n_cols - 1L)),
                rep(NA, n_cols),
                .
            )
        rownames(res_mat) = rep("", nrow(res_mat))
        
        return(res_mat)
        
    })
    
    return(purrr::map(xlsx_sum, as.data.table))
    
}
