## ------------------------------------------------------------------
## Setup
## ------------------------------------------------------------------

library("here")
library("data.table")

# best fitting model
n_dim = 3

# model specs
dir_fn = matrix(NA, nrow = 6, ncol = 2)
dir_fn[1, ] = c("dim0", "randmix")
dir_fn[2, ] = c("dim0", "overdispersed")
for (ii in 2:5) 
    dir_fn[ii + 1, ] = c(paste0("dim", ii), "social_space")



## ------------------------------------------------------------------
## Get min Neff and maximum R-hat
## ------------------------------------------------------------------

message("Calculating Neff and Rhat for all models ...")
# get neff and Rhat
conv_list = rbindlist(
    lapply(
        1:nrow(dir_fn),
        function(w) {
            
            # dir name
            dn = dir_fn[w, 1]
            # model name
            mn = dir_fn[w, 2]
            
            message(
                paste0(
                    "Getting stats for ",
                    dn, "-", mn, " model ..."
                )
            )
            
            # get results
            mod = readRDS(here("results", dn, mn, paste0(mn, ".rds")))
            
            # get n_eff and Rhat
            res = as.data.table(summary(mod$fit)$summary[, c("n_eff", "Rhat")])
            res[, model := paste0(dir_fn[w, ], collapse = "_")]
            
            return(res)
        }
    ) 
)

message(
    paste0(
        "Maximum Rhat across all models is: ", round(max(conv_list$Rhat, na.rm = T), 5)
    )
)

message(
    paste0(
        "Minimum Neff across all models is: ", round(min(conv_list$n_eff, na.rm = T), 5)
    )
)

# save and remove results
saveRDS(conv_list, here("data", "conv_stats.rds"))
rm(conv_list)


## ------------------------------------------------------------------
## Summarize conv stats for final model
## ------------------------------------------------------------------

message("Summarizing convergence stats for final model in detail ...")
# load final model
fit = readRDS(
    here(
        "results", 
        paste0("dim", n_dim), 
        "social_space", 
        "social_space.rds"
    )
)
p_fit = summary(fit$fit)$summary

# all var names
p_names = rownames(p_fit)

# drop auxiliary vars
p_names = p_names[!grepl("star", p_names)]

# don't get distances and drop brackets
nn = gsub("\\[.*\\]", "", p_names[!grepl("^d\\[", p_names)])

# keep only unique
nn = unique(nn)

# no lp and log-lik
nn = nn[!grepl("lp__|log_lik|Rho_i", nn)]

# deal with corr mat separately
rho_names = grep("^Rho_i", p_names, value = T)
inx1 = as.integer(gsub(paste0("(Rho_i\\[)(\\d)(,.*)"), "\\2", rho_names))
inx2 = as.integer(gsub(paste0("(Rho_i\\[\\d,)(\\d)(.*)"), "\\2", rho_names))
off_diag = rho_names[inx1 < inx2] 

# combine var names
nn = c(nn, off_diag)

# create summary matrix
res_tab = lapply(nn, function(w) {
    
    if (grepl("Rho", w)) {
        
        sel = p_names %in% w
        
    } else if (w %in% c("pos_i", "pos_j", "p")) {
        
        sel = grepl(paste0("^", w, "\\["), p_names) 
        
    } else {
        
        sel = grepl(paste0("^", w), p_names)
        
    }
    
    mat = p_fit[sel, ]
    
    if (is.vector(mat)) {
        
        res = cbind(
            rep(w,2), c("n_eff", "Rhat"), 
            matrix(rep("",12), nrow = 2),
            gsub("(.*?\\.\\d{2})(.*)", "\\1", as.character(mat[c("n_eff", "Rhat")]))
        ) 
        colnames(res) = c("Parameter", "Stat","Min", "5%", "25%", "50%", "75%", "95%","Max")
        
        return(res)
        
    }
    
    # fixed coefficients will have Rhat = NaN and n_eff = n.samps
    mat = mat[rowSums(is.na(mat)) == 0, c("n_eff", "Rhat")]
    
    res = apply(mat, 2, quantile, prob = c(0,.05, .25, .5, .75, .95, 1))
    res = apply(res, 2, function(z) {
        gsub("(.*?\\.\\d{2})(.*)", "\\1", as.character(z))
    }) 
    
    res = t(res)
    res = cbind(rep(w, 2), c("n_eff", "Rhat"), res)
    colnames(res) = c("Parameter", "Stat","Min", "5%", "25%", "50%", "75%", "95%","Max")
    rownames(res) = NULL

    return(res)
    
}) 
res_tab = do.call(rbind, res_tab)

message("Creating LaTeX table ...")

# transform into LaTeX code
res_tab = res_tab[order(rowSums(res_tab == "")), ]
res_tab[, 1] = gsub("Rho", "Sigma", res_tab[, 1])
res_tab[, 1] = gsub("^pos\\_i", "theta_i", res_tab[, 1])
res_tab[, 1] = gsub("^pos\\_j", "xi_j", res_tab[, 1])
res_tab[, 1] = gsub("^p$", "pi_j", res_tab[, 1])
res_tab[, 1] = gsub("\\_gamma", "\\_\\\\gamma", res_tab[, 1])
res_tab[, 1] = gsub("\\[","\\{", res_tab[, 1])
res_tab[, 1] = gsub("\\]","\\}", res_tab[, 1])
res_tab[, 1] = gsub("Sigma_i","Sigma_", res_tab[, 1])
res_tab[, 1] = paste0("$\\", res_tab[, 1],"$") 
colnames(res_tab) = gsub("%","\\\\%", colnames(res_tab))

# order
ord = c("theta", "xi", "pi", "alpha", "[^_]\\\\gamma", "sigma", "phi", "Sigma")
ord_names = unlist(lapply(ord, function(w) grep(w, res_tab[, 1], value = T)))
ord_names = unique(ord_names)


# rhat
rhat_tab = res_tab[res_tab[, 2] == "Rhat", -2]
rhat_tab = rhat_tab[match(ord_names, rhat_tab[, 1]), ]
rhat_tab = rbind(colnames(rhat_tab), rep("", ncol(rhat_tab)), rhat_tab)
rhat_tab = apply(rhat_tab, 1, paste0, collapse = "&")
rhat_tab = gsub(
    "(\\&\\&\\&\\&\\&\\&)(\\d+\\.\\d+)", 
    "\\\\multicolumn{7}{c}{\\2}", 
    rhat_tab
)
rhat_tab = c(
    rhat_tab[1:2],
    "\\multicolumn{8}{l}{\\it Potential Scale Reduction Factor, $\\hat{R}$}",
    rhat_tab[3:length(rhat_tab)]
)
rhat_tab = paste0(rhat_tab, collapse="\\\\ \n")
rhat_tab = paste0("\\begin{tabular}{p{3cm}rrrrrrr} \\hline \n", rhat_tab)
# rhat_tab = paste0(rhat_tab, "\\\\ \\hline \n \\end{tabular}")
rhat_tab = paste0(rhat_tab, "\\\\ \n&&&&&&& \\\\")
rhat_tab = sub("&Max\\\\\\\\", "&Max \\\\\\\\ \\\\hline", rhat_tab)

# neff
neff_tab = res_tab[res_tab[, 2] == "n_eff", -2] 
neff_tab = neff_tab[match(ord_names, neff_tab[, 1]), ]
neff_tab = rbind(colnames(neff_tab), neff_tab)
neff_tab = apply(neff_tab, 1, paste0, collapse = "&")
neff_tab = gsub(
    "(\\&\\&\\&\\&\\&\\&)(\\d+\\.\\d+)", 
    "\\\\multicolumn{7}{c}{\\2}", neff_tab
)
neff_tab = paste0(neff_tab, collapse = "\\\\ \n")
neff_tab = paste0("\\multicolumn{8}{l}{\\it Effective Sample Sizes} \\\\ \n", neff_tab)
neff_tab = paste0(neff_tab, "\\\\ \\hline \n \\end{tabular}")

# combine 
conv_tab = paste0(rhat_tab, neff_tab, collapse = "\n")

# change gamma into log-gamma (and also for alpha)
conv_tab = gsub("\\\\gamma", "\\\\log \\\\gamma_i", conv_tab)
conv_tab = gsub("\\\\alpha", "\\\\log \\\\alpha", conv_tab)


# write into file
capture.output(
    cat(conv_tab),
    file = here("output", paste0("convstats_dim", n_dim, ".txt"))
)

rm(
    conv_tab, dir_fn, fit, ii, inx1, inx2, n_dim, neff_tab, nn, off_diag,
    ord, ord_names, p_fit, p_names, res_tab, rhat_tab, rho_names
)


### END OF CODE ###