#' Print data.frame as message
#' 
#' Print \code{data.frame} or \code{data.table} object as a message. 
#' 
#' @param x \code{data.frame} or \code{data.table} object
#' @param digits number of significant digits to be printed; defaults to 3
#' @param row_names boolean; if TRUE, row names of \code{x} are printed. Defaults to FALSE
#' @param col_names boolean; if TRUE, column names of \code{x} are printed. Defaults to TRUE
#' @return prints out the content of the data.table as a \code{message}
df_to_message = function(
    x, 
    digits = 3L, 
    row_names = FALSE, 
    col_names = TRUE, 
    abbr_strings = TRUE) 
{
    
    y = as.data.frame(
        purrr::map_if(
            x, 
            ~ class(.x) == "numeric", 
            ~ round(.x, digits)
        )
    )
    
    if (!col_names) names(y) = NULL
    
    message(
        paste(
            utils::capture.output(
                print(y, row.names = row_names, digits = digits)
            ),
            collapse = "\n"
        )
    )
    
}
