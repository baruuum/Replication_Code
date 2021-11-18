## ------------------------------------------------------------------
## Fit 5-Dimensional Model
## ------------------------------------------------------------------

library("here")

n_cores = 24
n_dim = 5
model_name = "social_space"
v_order = c("black", "lib", "unemp", "goatt", "cohab", "prisn")
model_seed = 2352361

min_resp = 7
n_chains = 2
iter_sampling = 4000
iter_warmup = 3000
refresh = 1000
init = .1
max_treedepth = 12
adapt_delta = .99
step_size = .1

# fit random mixing model
source(here("scripts", "2-2_Fit_Models.R"), echo = TRUE)


## ------------------------------------------------------------------
## Print Session Info.
## ------------------------------------------------------------------

cat("\n\n---- Session Info ----\n")
print(sessionInfo())


### END OF CODE ###
