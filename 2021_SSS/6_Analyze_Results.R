## ------------------------------------------------------------------
## Setup
## ------------------------------------------------------------------

library("here")

## ------------------------------------------------------------------
## Convergence and Predictive Fit
## ------------------------------------------------------------------

# Model selection
message("\n\n----------- Running 2-3_Analysis_GOF.R...\n\n")

# input:
#   1. WAIC results: here("results", "{dim}", "{model}", "waic.rds")
#   2. LOOIC results: here("results", "{dim}", "{model}", "loo.rds")
#   3. Final model: here("results", "dim3", "social_space", "social_space.rds")
#   4. Random mixing model: here("results", "dim0", "randmix", "randmix.rds")
#   4. Recoded predictors: here("data", "predictors.rds")
# output:
#   1. WAIC and LOOIC (LaTeX code): here("output", "goftab.txt")
#   2. Group-labels to be used later: here("data", "labels_short_long.rds")
#   3. Item data with labels added: here("data", "item_sum_w_labs.csv")
#   4. Individual data: here("data", "ind_sum.csv")
#   4. Posterior samples (Dim3): here("data", "post_samples.csv")
#   4. Posterior predictive checks for final model: here("output", "plots", "ppc.pdf")
source(here("scripts", "2-3_Analysis_GOF.R"))


# Convergence statistics
message("\n\n----------- Running 2-4_Conv_Stats.R...\n\n")

# input:
#   1. Fitted models: here("results", "{dim}", "{model}", "{modelname}.rds")
# output:
#   1. Convergence stats for all models: here("data", "conv_stats.csv")
#   2. Convergence stats for final model: here("output", "convstats_dim3.txt"))
source(here("scripts", "2-4_Conv_Stats.R"))



## ------------------------------------------------------------------
## Analyze Results
## ------------------------------------------------------------------

# Group-distances and salience
message("\n\n----------- Running 3-1_Analysis_Salience_and_Dist.R...\n\n")

# input:
#   1. posterior samples (dim 3): here("data", "post_samples.csv")
#   2. ind-level data: here("data", "ind_sum.csv")
#   3. item-level data: here("data", "item_sum_w_labs.csv")
#   4. recoded predictors: here("data", "predictors.rds")
# output:
#   1. plot of salience estimates: here("output", "plots", "salience.pdf")
#   2. group-distance heatmap: here("output", "plots", "dist_heat.pdf")
source(here("scripts", "3-1_Analysis_Salience_and_Dist.R"))


# Space 
message("\n\n----------- Running 3-2_Analysis_Space.R...\n\n")

# input:
#   1. posterior samples (dim 3): here("data", "post_samples.csv")
#   2. ind-level data: here("data", "ind_sum.csv")
#   3. item-level data: here("data", "item_sum_w_labs.csv")
#   4. recoded predictors: here("data", "predictors.rds")
# output:
#   1. 3d-plot of space: here("output", "plots", "space3d.rds")
#   2. first two dims: here("output", "plots", "dim12.pdf")
#   3. density overlap across subgroups:
#       race: here("output", "plots", "dim12_race.pdf")
#       family income: here("output", "plots", "dim12_inc.pdf")
#       ideology: here("output", "plots", "dim12_ideo.pdf")
#       religiosity: here("output", "plots", "dim12_rel.pdf")
source(here("scripts", "3-2_Analysis_Space.R"))


# Segregation
message("\n\n----------- Running 3-2_Analysis_Dvals.R...\n\n")

# input:
#   1. posterior samples (dim 3): here("data", "post_samples.csv")
#   2. ind-level data: here("data", "ind_sum.csv")
#   3. item-level data: here("data", "item_sum_w_labs.csv")
# output:
#   1. D-vals (selected group): here("output", "plots", "dvals.pdf")
#   2. D-vals (all groups, heatmap): here("output", "plots", "dvals_all_heat.pdf")
source(here("scripts", "3-3_Analysis_Dvals.R"))


## ------------------------------------------------------------------
## Print Session Info.
## ------------------------------------------------------------------

cat("\n\n---- Session Info ----\n")
print(sessionInfo())
