
##
## Functions
##

## Functions to Check Convergence and PPC ---------------------------------------

get_div = function(fit_dat) {

    ## FUNCTION TO PRINT # OF DIVERGENT TRANSITIONS
    ##
    ## INPUT  :
    ##   fit_dat = list with element "fit" (should be a stanfit object)
    ##
    ## OUTPUT : number of divergent transitions per chain
    
    library(rstan)
    
    sp = get_sampler_params(fit_dat$fit, inc_warmup = F)
    sapply(sp, function(z) mean(z[,"divergent__"]))

}
get_div = compiler::cmpfun(get_div)


gen_pred_dist = function(
    fit_dat,
    fit_info = NULL,
    fit_data = NULL,
    n_pred = 50,
    raw = F,
    rand_mix = F
) {

    ## FUNCTION TO GENERATE POST. PRED. DISTRIBUTION
    ##
    ## INPUT  :
    ##   fit_dat  = list with element "fit" (should be stanfit object)
    ##              or data.table object (which is also a list)
    ##   fit_info = model info (if fit_dat is data.table)
    ##   fit_data = model data (if fit_dat is data.table)
    ##   n_pred   = number of draws to draw from ppd
    ##   raw      = whether raw or truncated predictions should be drawn
    ##   rand_mix = whether fitted model is random mixing model
    ##
    ## OUTPUT :
    ##   list with 2 elements:
    ##     1) observed outcome that was fitted
    ##     2) matrix of dimension (N, n_pred), where N is the total number
    ##        of item-responses. Each column is a draw from the ppd
    
    
    library("rstan")
    library("data.table")
    
    if (!is.list(fit_dat))
        stop("fit_dat needs to be a list!")
    
    # parameters needed to draw from ppd
    xb_pars = c("alpha","gamma","phi","p","d")
    
    if (rand_mix)
        xb_pars = xb_pars[xb_pars %in% c("d","phi") == F]

    if ("fit" %in% names(fit_dat) == F) {
        
        message(
            paste0(
                "Element fit not found in fit_dat object !\n",
                "Treating fit_dat as data.table of posterior draws ..."
            )
        )

        if (is.null(fit_info))
            stop("Need fit_info!")
        if (is.null(fit_data))
            stop("Need fit_data!")
    
        post_samps = as.matrix(fit_dat)
        n_inds = fit_data$n_inds
        n_items = fit_data$n_items
        dd = fit_data$long_dat
    
    }  else {

        post_samps = as.matrix(fit_dat$fit, pars = xb_pars)
        n_inds = fit_dat$data$n_inds
        n_items = fit_dat$data$n_items
        dd = fit_dat$data$long_dat

    }

    # if n_pred is not specified use all posterior draws
    if (is.null(n_pred))
        n_pred = nrow(post_samps)
    
    # send out message
    message(
        paste0("Generating ",
            n_pred,
            " draws from posterior predictive distribution ..."
        )
    )
    
    samps = sample(1:nrow(post_samps), n_pred, replace = F)
    post_samps = post_samps[samps, ]

    # generate samples from posterior predictive dist.
    preds = lapply(1:n_pred, function(w) {

        # get gammas
        gamma = post_samps[
            w, grep("^gamma\\[", colnames(post_samps), value = T)
        ]
        # add ids
        gamma = cbind(1:n_inds, gamma)
        colnames(gamma) = c("id","gamma")

        # get population shares
        p = post_samps[
            w, grep("^p\\[", colnames(post_samps), value = T)
        ]
        # add item no.
        p = cbind(1:n_items, p)
        colnames(p) = c("item","p")

        # merge with data
        dd = merge(dd, p, by = "item", sort = F)
        dd = merge(dd, gamma, by = "id", sort = F)
        if (!rand_mix) {
            # distance
            dd = cbind(
                dd,
                d = post_samps[
                    w, grep("^d\\[", colnames(post_samps), value = T)
                ]
            )
        }
        
        # add alpha
        dd[,alpha:=post_samps[w,"alpha"]]
        # generate linear predictor
        if (rand_mix) {

            # linear predictor
            dd[, lambda := alpha + gamma + log(p)]
    
            # draw from ppd
            reps = rpois(nrow(dd), lambda = exp(dd$lambda))

        } else {

            # linear predictor
            dd[,lambda := alpha + gamma + log(p) - d]
    
            # draw from ppd
            reps = rnbinom(nrow(dd),
                           mu = exp(dd$lambda),
                           size = post_samps[w, "phi"]
            )

        }


        # if raw == T, return latent counts
        if (raw) return(reps)

        # otherwise return truncated counts
        y = ifelse(
            reps == 0, 
            0,
            ifelse(
                reps == 1, 
                1,
                ifelse(
                    reps > 1 & reps < 6, 
                    2,
                    ifelse(
                        reps > 5 & reps < 11, 
                        3, 
                        4
                    )
                )
            )
        )
         
        return(y)

    }) 
    preds = do.call(cbind, preds)

    return(list(outcome = dd$response, pred = preds))

}
gen_pred_dist = compiler::cmpfun(gen_pred_dist)


conv_check = function(
    fit_dat,
    n_samp = 30,
    inc_warmup = F,
    a_level = .6
) {
    
    library("here")
    
    # temporary path
    conv_path = here(
        "results", 
        paste0("dim", fit_dat$model_info$n_dim), 
        fit_dat$model_info$model_name, 
        "convergence"
    )

    if (!dir.exists(conv_path)) {
        
        message("Creating directory to store results ... ")
        dir.create(conv_path, recursive = T)
        
    }
    
    par_dat = data.table(
        pars = c(fit_dat$model_info$pars, "lp__")
    )
    
    dims = lapply(
        fit_dat$fit@par_dims, 
        function(w) {
            if (length(w) == 0) {
                c(1, 0)
            } else if (length(w) == 1) {
                c(w, 0)
            } else {
                w
            }
        }
    ) 
    dims = do.call("rbind", dims) 
    dims = data.table(rownames(dims), dims)
    setnames(dims, c("pars", "dim1", "dim2"))
    
    par_dat = merge(par_dat,
                    dims,
                    by = "pars",
                    all.x = T)
    
    for (pp in 1:nrow(par_dat)) {
    
        p_vec = unlist(par_dat[pp], use.names = FALSE)
        
        message(paste0("Creating traceplots for ", p_vec[1]," ..."))
        
        if (as.numeric(p_vec[3]) == 0) {
            tot_pars = as.numeric(p_vec[2])
        } else {
            tot_pars = as.numeric(p_vec[2]) * as.numeric(p_vec[3])
        }
        
        if (tot_pars <= n_samp) {
        
            pdf(paste0(conv_path, "/", p_vec[1],".pdf"), width = 14, height = 10)
            print(
                traceplot(
                    fit_dat$fit,
                    pars = p_vec[1],
                    inc_warmup = inc_warmup,
                    alpha = a_level
                )
            )
            dev.off()
        
        } else if (tot_pars > n_samp & p_vec[3] == 0) {
        
            message(
                 paste0(
                     "As total number of parameters in ",
                     p_vec[1],
                     " exceeds ",
                     n_samp,
                     ", sampling ",
                     n_samp,
                     " parameters for convergence checking ..."
                )
            )
            s_pars = sort(sample(1:p_vec[2], n_samp, replace = F))
            
            pdf(paste0(conv_path, "/", p_vec[1],".pdf"), width = 14, height = 10)
            print(
                traceplot(
                    fit_dat$fit,
                    pars = paste0(p_vec[1], "[", s_pars, "]"), 
                    inc_warmup = inc_warmup,
                    alpha = a_level)
            )
            dev.off()

        } else {
    
            message(
                paste0(
                    "As total number of parameters in ",
                    p_vec[1],
                    " exceeds ",
                    n_samp,
                    ", sampling ",
                    min(n_samp, p_vec[2]),
                    " parameters for convergence checking ",
                    " (for each dimension) ..."
                )
            )
    
		    s_pars = sample(1:p_vec[2], min(n_samp, p_vec[2]), replace = F)
    
            for (dd in 1:p_vec[3]) {
    
                pdf(paste0(conv_path, "/", p_vec[1],".", dd, ".pdf"), width = 14, height = 10)
                print(
                    traceplot(
                        fit_dat$fit,
                        pars=paste0(p_vec[1], "[", s_pars, ",", dd,"]"), 
                        inc_warmup = inc_warmup,
        				alpha = a_level
        			)
                )
                dev.off()
        
            }
    
        }
    
    }

    library(bayesplot)
    ggplot2::theme_set(theme_bw())
    
    ### Rhat distribution
    message("Plotting Diagnostic plots ... ")
    
    pdf(paste0(conv_path, "/","Rhat.pdf"),
       width=14,
       height=10)
    print(mcmc_rhat_hist(rhat(fit_dat$fit)))
    dev.off()
    
    pdf(paste0(conv_path, "/","Neff.pdf"),
       width=14,
       height=10)
    print(mcmc_neff_hist(neff_ratio(fit_dat$fit)))
    dev.off()
    
    # get log-posterior and NUTS params
    lp = log_posterior(fit_dat$fit)
    np = nuts_params(fit_dat$fit)
    
    # look where the divergence happanes if any
    pdf(paste0(conv_path, "/","Div.pdf"),
       width=14,height=10)
    print(mcmc_nuts_divergence(np, lp))
    dev.off()
    
    # all hists. should look similar
    pdf(paste0(conv_path, "/","Energy.pdf"),
       width=14,height=10)
    print(mcmc_nuts_energy(np))
    dev.off()
    
    # tree depth should remain withihn 0 or 1
    pdf(paste0(conv_path, "/","TD.pdf"),
       width=14,height=10)
    print(mcmc_nuts_treedepth(np, lp))
    dev.off()

}
conv_check = compiler::cmpfun(conv_check)

## Functions for Plotting -------------------------------------------------------

rotate_pos = function(pos, return_matrix=F) {

    if ("matrix" %in% class(pos) == F) {
        message("Forcing position data into matrix format")
        x = as.matrix(pos)
    } else x = pos
    
    c_names = !is.null(colnames(pos))
    if (c_names) {
    n_names = paste0("r_", colnames(x))
    message(paste0("New dimension names are : ",
                   paste0(n_names, collapse = ",")))
    }
    
    # column center matrix
    c_means = colMeans(x)
    x = sweep(x, 2, c_means)
    
    # get SVD
    SVD = svd(x)
    
    # right-singular vectors
    R = SVD$v
    # singular values
    S = SVD$d
    
    new_pos = x %*% R
    if (c_names)
        colnames(new.pos) = n_names
    
    if (return_matrix)
        return(list(new_pos = new_pos,R = R, S = S, means = c_means))
    
    return(new_pos)

}
rotate_pos = compiler::cmpfun(rotate_pos)


# vectorized row-wise distance
vec_pdist = function(A, B) {
    
    an = apply(A, 1, function(rvec) crossprod(rvec,rvec))
    bn = apply(B, 1, function(rvec) crossprod(rvec,rvec))
    
    m = nrow(A)
    n = nrow(B)
    
    tmp = matrix(rep(an, n), nrow=m)
    tmp = tmp +  matrix(rep(bn, m), nrow = m, byrow = TRUE)
    sqrt( tmp - 2 * tcrossprod(A,B) )
    
}
vec_pdist = compiler::cmpfun(vec_pdist)


# get positions in social space
gen_positions = function(samps, which = c("i", "j"), n_dim = 3) {

    p = match.arg(which, c("i", "j"), several.ok = F)
    n_samps = nrow(samps)

    pos = samps[
        , grepl(paste0("pos\\_", p,"\\["),names(samps)), with = F
    ] 
    pos = as.matrix(pos)
    
    lapply(
        1:n_samps,
        function (w) matrix(pos[w, ], ncol = n_dim)
    )

}
gen_positions = compiler::cmpfun(gen_positions)

# distance to individuals
gen_agg_dists = function(
    samps,
    weighted = F,
    labs = NULL,
    stat = NULL,
    log_scale = F,
    sub_samp = NULL
) {

    n_samps = nrow(samps)
    
    if (!is.null(stat))
        stat = match.arg(
            stat,
            c("mean", "median", "sd", "var", "mad", "iqr"),
            several.ok = F
        )
    
    # get individual positions
    pos_i = gen_positions(samps, "i")
    
    if (!is.null(sub_samp)) {
    
    if (weighted)
    stop("weighted and sub_samp not supported yet!")
    
    message("creating distances for sub-sample of individuals...")
    pos_i = lapply(pos_i,`[`,sub_samp,)
    
    }
    
    # get group positions
    pos_j = gen_positions(samps, "j")
    
    # gregariousnes
    if (weighted) {
    
        gamma_star = samps[, grepl("gamma\\[",names(samps)), with = F] 
        gamma_star = as.matrix(gamma_star)
        alpha = samps[, alpha]
        
        gamma = lapply(
            1:n_samps, function (w) {
                # get gammas
                g = exp(gamma_star[w,] + alpha[w])
                # normalize to sum to sample size
                g / mean(g)
            }
        )
        
        # combine into list
        comb_list = mapply(list, pos_i, pos_j, gamma, SIMPLIFY = F)
    
    } else {
    
        comb_list = mapply(list, pos_i, pos_j, SIMPLIFY = F)
    
    }
    
    # generate distances
    if (weighted) {
    
        ij_dists = lapply(
            comb_list, 
            function (w) {
                vec_pdist(
                    sweep(w[[1]], 1, w[[3]], FUN = "/"),
                    w[[2]]
                )
            }
        )
        
    } else {
    
        ij_dists = lapply(
            comb_list, 
            function (w) {
                vec_pdist(w[[1]], w[[2]])
            }
        )
    
        if (log_scale)
            ij_dists = lapply(ij_dists, function(w) log(w) / 2)
    
    }
    
    if (!is.null(stat)) {
    
        sum_dists = switch(
            stat,
            mean   = lapply(ij_dists, colMeans),
            median = lapply(ij_dists, function (w) apply(w, 2, median)),
            sd     = lapply(ij_dists, function (w) apply(w, 2, sd)),
            var    = lapply(ij_dists, function (w) apply(w, 2, var)),
            mad    = lapply(ij_dists, function (w) apply(w, 2, mad)),
            iqr    = lapply(ij_dists, function (w) apply(w, 2, IQR))
        ) 
        
        sum_dists = as.data.table(do.call(rbind, sum_dists))
        sum_dists[, draw := 1:n_samps]

        if (!is.null(labs))
            setnames(sum_dists, c(labs, "draw"))
    
        return(sum_dists)
    
    }
    
    
    # create data.table
    res = do.call(rbind, ij_dists) 
    res = data.table(
        cbind(rep(1:length(ij_dists), each = data$n_inds))
    )
    
    if (!is.null(labs))
        setnames(res, c(data$item_sum$short_labs, "draw"))
    
    return(res)

}
gen_agg_dists = compiler::cmpfun(gen_agg_dists)
