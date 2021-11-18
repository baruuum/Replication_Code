
## ------------------------------------------------------------------
## Setup
## ------------------------------------------------------------------

library("here")
library("data.table")
library("cmdstanr")
library("rstan")
library("loo")

stopifnot(
    exists("n_cores"),
    exists("n_dim"),
    exists("model_name"),
    exists("min_resp"),
    exists("v_order"),
    exists("model_seed"),
    exists("n_chains"),
    exists("iter_sampling"),
    exists("iter_warmup"),
    exists("refresh"),
    exists("init"),
    exists("max_treedepth"),
    exists("adapt_delta"),
    exists("step_size")
)

mcmc_opts = list()
mcmc_opts$chains = n_chains
mcmc_opts$parallel_chains = mcmc_opts$chains
mcmc_opts$threads_per_chain = floor(n_cores / mcmc_opts$parallel_chains)
mcmc_opts$iter_sampling = iter_sampling
mcmc_opts$iter_warmup = iter_warmup
mcmc_opts$refresh = refresh
mcmc_opts$seed = model_seed
mcmc_opts$init = init
mcmc_opts$max_treedepth = max_treedepth
mcmc_opts$adapt_delta = adapt_delta
mcmc_opts$step_size = step_size


set_cmdstan_path(here("cmdstan", "cmdstan-2.27.0"))
cmdstan_ver = cmdstan_version()


## ------------------------------------------------------------------
## Load data
## ------------------------------------------------------------------

# path to save results
new_path = paste0(here("results", paste0("dim", n_dim)))
if (!dir.exists(new_path)) {
    message(paste0("Creating directory ",new_path))
    dir.create(new_path, recursive=T)
}

# read in functions
source(here("scripts", "0_Functions.R"))

# create sub directory to save model
save_dir = here("results", paste0("dim", n_dim), model_name)
if (!dir.exists(save_dir)) {
  
  message(
    paste0("Creating directory to save results into ",
           save_dir, " ...")
  )
  dir.create(save_dir,
             recursive=T)
}

# load data
dat_list = readRDS(here("data", "acq_dat.rds"))
dat = copy(dat_list$org_dat)
org_n = nrow(dat)

# get isolates
iso = rowSums(dat[, -1], na.rm = T) == 0

if (sum(iso) > 0) {
  
  message(paste0("Removing ", sum(iso)," isolates ..."))
  dat = dat[!iso, ]
  
}

# drop obs?
if (min_resp > 0) {
  
    # keep obs. with more than 6 valid responses
    # note: "id" is always non-missing
    message(paste0("Dropping respondents with less than ",
                   min_resp,
                   " valid responses .."))
    
    message(paste0("No. of obs. of original dataset : ",
                   org_n))
    
    lt_n_resp = rowSums(!is.na(dat)) < min_resp
    dat = dat[!lt_n_resp, ]
    new_n = nrow(dat)
    message(paste0("No. of obs. who are isolates : ",
                   sum(iso)))
    message(paste0("No. of obs. with response < ", min_resp,
                   " : ", sum(lt_n_resp)))
    message(paste0("No. of dropped obs. : ",
                   org_n - new_n))
    message(paste0("No. of obs. used in analysis : ",
                   new_n))
    
    # update ind ids
    dat_list$ind_sum = data.table(new_id = 1:nrow(dat),
                                  old_id = dat[, id])

}


# change order of variables?
if (!is.null(v_order)) {
  
    message("\nChanging order of variables .. ")
    oo = paste0("acq", v_order)

    # change order
    nn = names(dat)
    setcolorder(dat, c("id", oo, setdiff(nn, c("id", oo))))

    message(paste0("New column order is now : \n",
                   paste0(names(dat)[-1], collapse=", ")))

    # update item-name correspondence
    dat_list$item_sum = data.table(item_no=1:(ncol(dat) - 1),
                                   labs = names(dat)[-1])

} else {
    
    message("Variable order not specified!")
    message("Proceeding without reordering ...")

}

message("\nGenerating data for STAN ...\n")

# rename variables and ids (needed for STAN)
setnames(dat, c("id",1:(ncol(dat) - 1)))
dat[, id := 1:nrow(dat)]

### Reshape into long format

# melt
long_dat = melt(dat, id.vars = "id") 
setcolorder(long_dat, c("id", "variable", "value")) 
setnames(long_dat, c("id","item","response"))
setorder(long_dat, id, item)

# change item to numeric & drop NAs
long_dat = long_dat[
    , item := as.numeric(item)
][
    !is.na(response)
]

# check whether every respondent is there
stopifnot(all.equal(long_dat[, sort(unique(id))], 1:long_dat[, max(id)]))

# save data
dat_list$wide_dat = dat
dat_list$long_dat = long_dat
dat_list$n_obs  = nrow(dat_list$long_dat)
dat_list$n_inds = nrow(dat_list$wide_dat)


message(paste0("No. of Respondents : ", dat_list$n_inds))
message(paste0("No. of Items : ", dat_list$n_items))
message(paste0("No. of Item-responses : ", dat_list$n_obs))


# population estimates
ppp = fread(here("data", "pop_est.csv"))
# drop source column
ppp[, source := NULL]

# add population estimates to data.list
dat_list$item_sum = merge(dat_list$item_sum,
                          ppp,
                          by = "labs",
                          sort = F)
setcolorder(dat_list$item_sum, c("item_no", "labs", "prop", "sds"))

message("\nPrior means and std. dev. for groups")
print(as.matrix(dat_list$item_sum[, .(labs, prop, sds)]))


### start running ###

message("\n\nStart fitting Stan model ...")
message(paste0("Fitting ", n_dim," dimensions ...\n"))

message("\nCompiling model ...")

cmd_file = here("stan_code", paste0(model_name, ".stan"))
mod = cmdstan_model(
    stan_file = cmd_file, 
    force_recompile = TRUE,
    pedantic = TRUE,
    dir = here(
        "results", 
        paste0("dim", n_dim), 
        model_name
    ),
    cpp_options = list(stan_threads = TRUE)
)

message("\nModel Code :\n")
cat(paste0("file name : ", model_name, ".stan"))
mod$print()

# data to feed into stan
stan_dat = list(
    N = dat_list$n_obs,
    I = dat_list$n_inds,
    J = dat_list$n_items,
    D = n_dim,
    ii = dat_list$long_dat[, id],
    jj = dat_list$long_dat[, item],
    y = dat_list$long_dat[, response],
    pops = dat_list$item_sum[, prop],
    sds = dat_list$item_sum[, sds]
)

fit = mod$sample(
    data = stan_dat,
    seed = mcmc_opts$seed,
    chains = mcmc_opts$chains,
    parallel_chains = mcmc_opts$parallel_chains,
    threads_per_chain = mcmc_opts$threads_per_chain,
    iter_warmup = mcmc_opts$iter_warmup,
    iter_sampling = mcmc_opts$iter_sampling,
    refresh = mcmc_opts$refresh,
    init = mcmc_opts$init,
    max_treedepth = mcmc_opts$max_treedepth,
    adapt_delta = mcmc_opts$adapt_delta,
    step_size = mcmc_opts$step_size
)

# couple of checks
fit$cmdstan_diagnose()

# save fit object
fit$save_object(file = here(
        "results", 
        paste0("dim", n_dim), 
        model_name, 
        paste0(model_name, "_cmdstan.rds")
    )
)


# load list of relevant pars
pars = readRDS(here("data", "pars.rds"))
pars = switch(
    model_name,
    randmix         = pars$rm,
    overdispersed   = pars$overdispersed,
    social_space    = pars$social_space
)


# information about model
model_info = list(model_name, n_dim, pars, mcmc_opts)
names(model_info) = c("model_name", "n_dim", "pars", "mcmc_opts")

# create object to return
res = list(data = dat_list, model_info = model_info)

# add rstan object
res$fit = rstan::read_stan_csv(fit$output_files())

message("Saving results ...")
saveRDS(
    res, 
    here(
        "results", 
        paste0("dim", n_dim), 
        model_name, 
        paste0(model_name, ".rds")
    )
)


message("divergence (per chain)")
try(get_div(res))
try(conv_check(res))

# WAIC and LOO
ll = extract_log_lik(res$fit)
Waic = waic(ll)
Loo = loo(ll, cores = 1L)

message("WAIC and LOO")
print(Waic)
print(Loo)

saveRDS(Waic, here("results", paste0("dim", n_dim), model_name, "waic.rds"))
saveRDS(Loo, here("results", paste0("dim", n_dim), model_name, "loo.rds"))

### END OF CODE ###
        
  
