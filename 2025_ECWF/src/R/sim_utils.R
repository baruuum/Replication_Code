## ------------------------------------------------------------------
## Utility functions for simulation runs
## ------------------------------------------------------------------

#' generating image matrices
#'
#' @param type type of image matric to create
#' @param K number of blocks
#' @param lo_val low-value of image matrix
#' @param hi_val high-value of image matrix
#' @return returns the element-wise logarithm of image matrix
gen_sim_lpsi = function(
    type = c("symm", "cycl", "hier"),
    K,
    lo_val = -1.0,
    hi_val = 1.0
) {

    type = match.arg(type)

    # populate
    lpsi = matrix(lo_val, K, K)

    if (type == "symm") {

        diag(lpsi) = hi_val

    } else if (type == "cycl") {

        for (k in 1:(K - 1))
            lpsi[k, k + 1] = hi_val

        lpsi[K, 1] = hi_val

    } else {

        for (k in 1:K)
            for (l in 1:k) 
                if (k >= l) 
                    lpsi[k, l] = hi_val

    }

    return(lpsi)

}

#' generating class proportions
#'
#' @param a base meausre
#' @param K number of blocks
#' @return returns the log-proportion of blocks
gen_sim_lpi = function(a, K) {

    p = a^(1:K / 2)
    log(p / sum(p))

}

# function to generate initial values
gen_init_spec = function(i) {

    stopifnot(i > 0, i %% 1 == 0)

    specs = expand.grid(
        type = c("simple", "dc", "bib"), norm = c("", "_norm")
    ) |>
        apply(1L, paste0, collapse = "")
    n_specs = length(specs)

    if (i > n_specs)
        return(c("random", "default"))

    return(c("spectral", specs[i]))

}

### EOF ###