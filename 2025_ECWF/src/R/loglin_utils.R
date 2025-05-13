## ------------------------------------------------------------------
## Utility functions for log-linear modeling
## ------------------------------------------------------------------

requireNamespace("sbmob")
requireNamespace("data.table")

#' Goodman's homogeneity test
#' 
#' @param x index of rows/columns in mobility table to collapse
#' @param data the mobility table (needs to be a matrix)
#' @return returns the GOF X2 and G2 statistics
collapsed_fit_single = function(x, data) {

    # check args
    stopifnot(
        all(x %% 1 == 0),           # should be integer
        inherits(data, "matrix"),
        nrow(data) == ncol(data)
    )

    if (length(x) == 1) {

        fit_tab = matrix(
            c(0, 0, 0, 0, 1, 1),
            nrow = 2L,
            ncol = 3L
        )

        colnames(fit_tab) = c("statistic", "df", "p-value")
        rownames(fit_tab) = c("G2", "X2")

        return(fit_tab)

    }

    longdat = sbmob::mobmat_to_dat(data, index = 1)
    longdat = longdat[(i %in% x | j %in% x)]
    longdat[, diags := (i == j) * i]

    fit = sbmob:::.sparse_glm(
        longdat$y,
        Matrix::sparse.model.matrix(
            as.formula(y ~ factor(i) + factor(j) + factor(diags)), 
            data = longdat
        ),
        type = 1,
        tol = 1e-7,
        maxit = 50,
        verbose = 0
    )

    fit_tab = matrix(NA, nrow = 2L, ncol = 3L)
    fit_tab[1, 1] = fit$deviance
    fit_tab[1, 2] = fit$df.residual
    fit_tab[1, 3] = pchisq(fit_tab[1, 1], df = fit_tab[1, 2], lower.tail = F)
    fit_tab[2, 1] = sum((longdat$y - fit$fitted.values)^2 / fit$fitted.values)
    fit_tab[2, 2] = fit_tab[1, 2]
    fit_tab[2, 3] = pchisq(fit_tab[2, 1], df = fit_tab[2, 2], lower.tail = F)
    colnames(fit_tab) = c("statistic", "df", "p-value")
    rownames(fit_tab) = c("G2", "X2")

    return(fit_tab)

}

#' Goodman's homogeneity test
#' 
#' @param x index of rows/columns in mobility table to collapse; might be a list of indices, when multiple rows/columns are collapsed or a single vector
#' @param data the mobility table (needs to be a matrix)
#' @return returns the GOF X2 and G2 statistics
collapsed_fit = function(x, data) {

    if (inherits(x, "list")) {

        res = lapply(x, function(w) {
            as.data.table(collapsed_fit_single(w, data))[
                , `:=`(
                    collapsed = paste0(w, collapse = ", "),
                    type = c("G2", "X2")
                )]
        }) 

        res = do.call(`rbind`, res)

    } else {
    
        res = as.data.table(collapsed_fit_single(x, data))[
            , `:=`(
                collapsed = paste0(x, collapse = ", "),
                type = c("G2", "X2")
            )
        ]

    }
    
    res[
        df == 0, 
        `:=`(
            statistic = NA,
            `p-value` = NA,
            df = NA
        )
    ]
    
    return(res)
    
}

### EOF ###