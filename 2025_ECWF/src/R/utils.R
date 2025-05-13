## ------------------------------------------------------------------
## General utility functions
## ------------------------------------------------------------------

#' Transpose after sapply
#' @return transposed results of \code{sapply}
tsapply = function(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE) {

    t(sapply(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE))

}

#' Print session info as message
#' @return NULL
print_session = function() {

    message(
        paste0("\n\nSession Information --------------\n\n"),
        paste0(
            capture.output(sessionInfo()),
            collapse = "\n"
        )
    )

}

#' print tables as message
#' @return NULL
print_table = function(x) {
    message(
        paste0(
            capture.output(x),
            collapse = "\n"
        )
    )
}

## ------------------------------------------------------------------
## Math functions
## ------------------------------------------------------------------

# inv-logit function
inv_logit = function(x) {1 / (1 + exp(-x))}

# get minimum and maximum of vector
minmax = function(w) { c(min(w), max(w)) }



## ------------------------------------------------------------------
## Data functions
## ------------------------------------------------------------------

glass_table = function() {

    matrix(
        c(
            50, 19, 26, 8, 7, 11, 6, 2,
            16, 40, 34, 18, 11, 20, 8, 3,
            12, 35, 65, 66, 35, 88, 23, 21,
            11, 20, 58, 110, 40, 183, 64, 32,
            2, 8, 12, 23, 25, 46, 28, 12,
            12, 28, 102, 162, 90, 554, 230, 177,
            0, 6, 19, 40, 21, 158, 143, 71,
            0, 3, 14, 32, 15, 126, 91, 106
        ),
        nrow = 8,
        ncol = 8,
        byrow = T
    )
    
}

## ------------------------------------------------------------------
## Utility functions for tables / matrices
## ------------------------------------------------------------------

#' convert table to matrix
#' 
#' @param table a \code{table} object
#' @return returns a \code{matrix} object
tab_to_mat = function(x) {
    matrix(x, ncol = ncol(x), dimnames = dimnames(x))
}

#' convert matrix into (long-form) data.table
#' 
#' @param x a \code{matrix} object
#' @return a 3-column \code{data.table}
mat_to_dt = function(x) {
    
    dt = data.table::data.table(
        row = rep(seq_len(nrow(x)), ncol(x)), 
        col = rep(seq_len(ncol(x)), each = nrow(x)), 
        value = c(x)
    )
    
    if (!is.null(dimnames(x))) {
        
        dnames = names(dimnames(x))
        data.table::setnames(dt, c("row", "col"), c(dnames[1], dnames[2]))
        
    }
    
    return(dt)
    
}

#' convert data.frame or matrix to LaTeX table
#'
#' @param tab the matrix
#' @param rownames boolean; whether to include the rownames
#' @param colnames boolean; whether to include the colnames
#' @param rowtitle string; title for rows
#' @param coltitle string; title for columns
#' @param below_caption string; a caption that is added below the table. The entered text will be wrapped in the \caption*{text} after the table.
#' @param below_caption_size string; the size of the caption text. Defaults to footnotesize.
#' @param digits the number of digits to print
#' @return returns a string of LaTeX code
mat_to_latex = function(
    tab,
    rownames = TRUE,
    colnames = TRUE,
    rowtitle = "",
    coltitle = "",
    below_caption = "",
    below_caption_size = "footnotesize",
    digits = 0,
    keep_trailing_zeros = TRUE
) {

    isdf = inherits(tab, "data.frame")

    if (isdf)  {

        if (inherits(tab, "data.table"))
            tab = as.data.frame(tab)

    } else {

        if (typeof(tab) == "character" && digits > 0) {

            warning(
                "Forcing tab into a numeric matrix. ",
                "If you want to avoid this, set digits to 0."
            )

            class(tab) = "numeric"

        }

    }

    if (digits %% 1 != 0)
        stop("digits has to be an integer")

    if (isdf) {

        if (all(rownames(tab) == 1:nrow(tab)))
            rownames = FALSE

    } else {
        
        if (is.null(rownames(tab)))
            rownames = FALSE

    }

    if (is.null(colnames(tab)))
        colnames = FALSE

    if ((!rownames || !colnames) && (rowtitle != "" || coltitle != "")) {
        warning(
            "No titles supported without row and column names. ",
            "Consider using captions instead."
        )
    }

    rr = dim(tab)[1] + colnames
    cc = dim(tab)[2] + rownames

    # round to digit
    if (digits > 0) {
        
        if (isdf) {

            classes = sapply(tab, class)
            nums = classes %in% c("numeric", "integer")
            tab[, nums] = round(tab[, nums], digits)

        } else {

            tab = round(tab, digits)

        }

    } 
        

    name_row = if (rownames) rownames(tab) else ""
    name_col = if (colnames) colnames(tab) else ""

    header = paste0("\\begin{tabular}{l", strrep("r", cc - 1L), "} \n", "\\hline", "\n")

    if (coltitle != "") {

        ctitle = paste0(" & ", "\\multicolumn{", cc - 1L, "}{c}{",
            coltitle, "} \\\\ \\cline{2-", cc, "} \n"
        )

        header = paste0(header, ctitle)

    }

    if (rownames && colnames) {

        first_line = paste0(
            paste0(rowtitle, " & "),
            paste0(name_col, collapse = " & "), " \\\\", "\n",  "\\hline"
        )

    } else if (colnames && !rownames) {

        first_line = paste0(
            paste0(name_col, collapse = " & "),
            " \\\\", "\n",  "\\hline"
        )

    } else {

        first_line = ""

    }

    header = paste0(header, first_line)

    body = if (rownames) cbind(name_row, tab) else tab

    if (isdf)
        body = as.matrix(body)

    # add trailing zeros
    if (digits > 0 && keep_trailing_zeros)
        body = gsub(
            paste0("(\\.\\d{", digits - 1L, "})(?!\\d)"),
            "\\10",
            body,
            perl = TRUE
        )

    # paste body together
    body = apply(
        body, 1L, function(l) paste0(paste0(l, collapse = " & "), " \\\\")
    ) |> paste0(collapse = " \n")

    footer = "\\hline\n\\end{tabular}"

    if (below_caption != "")
        footer = paste0(
            footer, " \n",
            "\\caption*{\\", below_caption_size, "%\n", below_caption, "}"
        )

    res = paste0(c(header, body, footer), collapse = "\n")
    return(res)

}



## ------------------------------------------------------------------
## Formatting functions
## ------------------------------------------------------------------

# adding trailing zeros to parameters
format_params = function(w) {

    sapply(w, function(x) {

        if (grepl("\\.", x)) {

            y = strsplit(x, "\\.")[[1]]

            if (nchar(y[2]) == 1)
                y[2] = paste0(y[2], "0")

            paste0(y, collapse = ".")

        } else paste0(x, ".00")

    })

}

#' Round numeric object to string while keeping trailing zeros
#'
#' @param x a numeric vector
#' @param digits integer; number of decimals to keep
#' @return character string/vector with \code{x} rounded to \code{digit} digits while keeping trailing zeros
round_to_char = function(x, digits = 1L)
{

    if (!is.null(dim(x)))
        stop("dimension of x has to be NULL")

    if (digits %% 1 != 0)
        stop("digits has to be an integer value")

    return(sprintf(paste0("%.", digits, "f"), round(x, digits)))

}

#' Change each word to upper case
#' 
#' @param x string
#' @return \code{x} with the first character of each word captalized
upfirst = function(x) {

    sapply(strsplit(x, " ")[[1]], function(w) {

        paste0(
            toupper(substr(w, 1, 1)),
            substr(w, 2, nchar(w))
        )

    }) |>
        paste(collapse = " ")

}

### EOF ###