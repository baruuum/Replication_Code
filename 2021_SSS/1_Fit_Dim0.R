## ------------------------------------------------------------------
## Fit 0-Dimensional Models
## ------------------------------------------------------------------

library("here")

n_cores = 24
n_dim = 0
model_name = "randmix"
v_order = NULL
model_seed = 325696

min_resp = 7
n_chains = 2
iter_sampling = 4000
iter_warmup = 3000
refresh = 1000
init = .5
max_treedepth = 10
adapt_delta = .8
step_size = .1

# fit random mixing model
source(here("scripts", "2-2_Fit_Models.R"), echo = TRUE)

model_name = "overdispersed"
model_seed = 9238473

# fit overdispersed model
source(here("scripts", "2-2_Fit_Models.R"), echo = TRUE)


## ------------------------------------------------------------------
## Print Session Info.
## ------------------------------------------------------------------

cat("\n\n---- Session Info ----\n")
print(sessionInfo())

### END OF CODE ###
