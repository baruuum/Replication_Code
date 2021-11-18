## ------------------------------------------------------------------
## Setup 
## ------------------------------------------------------------------

# load packages
library("here")
library("data.table")

# setup functions
source(here("scripts", "0_Functions.R"))

## ------------------------------------------------------------------
## Predictive Fit (LOOIC & WAIC)
## ------------------------------------------------------------------

message("Calculating LOOIC and WAIC ...")
# models to consider
mods = rbind(
   c("dim0", "randmix"),
   c("dim0", "overdispersed"),
   t(
      sapply(2:5, function(w) c(paste0("dim", w), "social_space"))
   )
)

# get fit statistics
gof_list = lapply(
   1:nrow(mods), 
   function(w) {
      v = mods[w, ]
      list(
         waic = readRDS(here("results", v[1], v[2], "waic.rds")),
         loo  = readRDS(here("results", v[1], v[2], "loo.rds"))
      )
   })
names(gof_list) = c(
   "Random Mixing", 
   "Group-specific Dispersion\\tnote{a}", 
   paste0("Social Space, ", 2:5, " Dimensions"))

message("Creating LaTeX table ...")
# get looic and waic estimates
gof_tab = cbind(
   loo  = sapply(gof_list, function(w) w[["loo"]]$estimates["looic", "Estimate"]),
   waic = sapply(gof_list, function(w) w[["waic"]]$estimates["waic", "Estimate"])
)

# reorder
gof_tab = gof_tab[nrow(gof_tab):1, ]

# add rownames to table and turn table into character
gof_char = cbind(rownames(gof_tab), round(gof_tab, 1))
rownames(gof_char) = NULL

# make LaTeX table
header = "\\begin{tabular}{p{6.5cm}rr}"
body = c(
   "\\hline",
   "Model & LOOIC & WAIC \\\\",
   "\\hline",
   apply(
      gof_char, 
      1L, 
      function(w) paste0(paste0(w, collapse = " & "), " \\\\")
   ),
   "\\hline"
)
footer = "\\end{tabular}"

latex_tab = paste0(
   c(header, body, footer), collapse = "\n"
)

# save output
cat(latex_tab, file = here("output", "goftab.txt"), append = FALSE)


## ------------------------------------------------------------------
## Posterior Predictive Checks
## ------------------------------------------------------------------

message("Starting posterior predictive checks for final model ...")

# set random seed
set.seed(4233765)

# get best model
min_stats = apply(gof_tab, 2L, which.min)
if (min_stats[1] != min_stats[2])
   warning("Waic and LOOIC disagree; choosing model based on LOOIC ...")

# minimizer
f_mod = rownames(gof_tab)[min_stats[1]]

# get model dimensions
n_dim = suppressWarnings(as.integer(sub("(^.*)(\\d)(.*$)", "\\2", f_mod)))
n_dim = ifelse(is.na(n_dim), 0L, n_dim)

# check
if (n_dim != 3)
   stop("something has changed: final model is not social-space-3")

# generate directory, if necessary
if (!dir.exists(here("output", "plots"))) dir.create(here("output", "plots"))

# get fitted model & data
fit = readRDS(here("results", paste0("dim", n_dim), "social_space", "social_space.rds"))
data = fit$data
model_info = fit$model_info
post_samps = as.data.table(fit$fit)

# read predictors
predictors = readRDS(here("data", "predictors.rds"))

# generate labels
lab_mat = cbind(
   labs = c(
      "acqblack",
      "acqlib",
      "acqunemp",
      "acqprisn",
      "acqhome",
      "acqasian",
      "acqhisp",
      "acqwhite",
      "acqgay",
      "acqcohab",
      "acqgoatt",
      "acqnoatt",
      "acqcon"
   ),
   long_labs = c(
      "Black",
      "Strongly Liberal",
      "Unemployed",
      "In Prison",
      "Own 2nd Home",
      "Asian",
      "Hispanic",
      "White",
      "Gay/Lesbian",
      "Cohabiting Women",
      "Relig. Regulary",
      "Relig. Rarely/Never",
      "Strongly Conservative"
   ),
   short_labs = c(
      "Black",
      "Lib",
      "Unemp",
      "Prison",
      "2nd. Home",
      "Asian",
      "Hispanic",
      "White",
      "Gay/Les",
      "Cohab",
      "Rel. Reg",
      "Rel. Never",
      "Cons"
   )
)
saveRDS(lab_mat, here("data", "labels_short_long.rds"))


# new names for the groups in data
data$item_sum[
   , long_labs := lab_mat[
      match(data$item_sum$labs, lab_mat[,"labs"]), 
      "long_labs"]
][
   , short_labs := lab_mat[
      match(data$item_sum$labs, lab_mat[,"labs"]), 
      "short_labs"
   ]
]

# save posterior samples
fwrite(post_samps, here("data", "post_samples.csv"))
fwrite(data$item_sum, here("data", "item_sum_w_labs.csv"))
fwrite(data$ind_sum, here("data", "ind_sum.csv"))

# order for plotting
g_order = c(
   "Rel. Reg",
   "Rel. Never",
   "Prison",
   "2nd. Home",
   "White",
   "Black",
   "Hispanic",
   "Asian",
   "Lib",
   "Cons",  
   "Cohab",
   "Unemp",
   "Gay/Les"
)


p_dat = merge(data$ind_sum, predictors, by.x = "old_id", by.y = "id") 
setorder(p_dat, new_id)
   


## ------------------------------------------------------------------
##  Posterior Predictive Checks & Comparison with RM Model
## ------------------------------------------------------------------


# number of draws from ppd
n_pred = 300

### Social Space Model ###

# get ppd draws
preds = gen_pred_dist(
   fit_dat = post_samps,
   fit_info = model_info,
   fit_data = data,
   n_pred = n_pred,
   raw = F,
   rand_mix = F)

# merge with item classifications
pred_dat = melt(
   cbind(data$long_dat[, "item"], preds$pred), 
   id.vars = "item"
)

pred_dat[, value := ifelse(value %in% 1:3, 3, value)]

# calculate proportion of each response for each item
pred_dat_n = pred_dat[, .N, by = list(item, variable, value)]
pred_dat_n[, props := N / sum(N), by = list(item, variable)]
pred_dat_n[, model := "Social Space (3 Dimensions)"]


### Random Mixing Model ###

# get fitted model & data
rm_fit = readRDS(here("results", "dim0", "randmix", "randmix.rds"))
rm_data = rm_fit$data
rm_model_info = rm_fit$model_info
rm_post_samps = as.data.table(rm_fit$fit)

rm_preds = gen_pred_dist(
   fit_dat = rm_post_samps,
   fit_info = rm_model_info,
   fit_data = rm_data,
   n_pred = n_pred,
   raw = F,
   rand_mix = T
)


# merge with item classifications
rm_pred_dat = melt(
   cbind(rm_data$long_dat[, "item"], rm_preds$pred),
   id.vars = "item"
)
rm_pred_dat[, value := ifelse(value %in% 1:3, 3, value)]

# calculate proportion of each response for each item
rm_pred_dat_n = rm_pred_dat[, .N, by = list(item, variable, value)]
rm_pred_dat_n[, props := N / sum(N), by = list(item, variable)]
rm_pred_dat_n[, model := "Random Mixing"]

# reorder item numbers to match space data
rm_i = rm_data$item_sum[, .(item_no, labs)]
setnames(rm_i, "item_no", "rm_item")
match_dat = merge(
   data$item_sum[, c("item_no", "labs")], 
   rm_i, by = "labs"
)
match_dat[, labs := NULL]

rm_pred_dat_n = merge(rm_pred_dat_n, match_dat, by.x = "item", by.y = "rm_item")
rm_pred_dat_n[, `:=`(item = item_no, item_no = NULL)]


### Merge Model Predictions and Plot ###

# merge
comb_q_dat = rbind(pred_dat_n, rm_pred_dat_n)

# change outcome variable to factor
comb_q_dat[
   , response := factor(
      value,                   
      levels = c(0, 3, 4),
      labels = c("None", "1-10", "Over 10")
   )
][
   , value:= NULL
]


# add long labels
comb_q_dat = merge(
   comb_q_dat, 
   data$item_sum[, c("item_no", "long_labs")], 
   by.x = "item",
   by.y = "item_no"
)[ , item := NULL]


# get data distribution
dat_p = data$long_dat[
   , response := ifelse(response %in% 1:3, 3, response)
][
   # get items and responses
   ,list(item,response)
][
   # calculate count
   , .N, by=list(item, response)
][
   # calculate proportions
   , probs := N/sum(N), by=item
][
   # drop counts
   , N := NULL
][
   # turn response into factor
   , response:=factor(
      response,                   
      levels=c(0,3,4),
      labels=c("None","1-10","Over 10")
   )
]

# add long labels
dat_p = merge(
   dat_p,
   data$item_sum[,.(item_no, long_labs)],
   by.x = "item",
   by.y = "item_no"
)[, item := NULL]


comb_q_dat[, long_labs := factor(
   long_labs, levels = data$item_sum[match(g_order, short_labs), long_labs]
)]

dat_p[, long_labs := factor(
   long_labs, levels = data$item_sum[match(g_order, short_labs), long_labs]
)]


# plot
pdf(here("output", "plots", "ppc.pdf"), width=10, height=7)
print(
   ggplot(comb_q_dat, aes(x = response)) +
   geom_col(
      data = dat_p,
      aes(y = probs),
      col = "white",
      fill = "grey",
      alpha = .5
   ) +
   geom_violin(
      aes(y = props,
          fill = model,
          color = model,
          group = interaction(model ,response)),
      position = position_dodge(.75),
      trim = F,
      scale = "width",
      width = .3,
      size = .5
   ) +
   scale_color_manual(name = "", values = c("grey50", "black")) +
   scale_fill_manual(name ="", values = c("grey50", "black")) +
   labs(x = "Number of Ties", y = "Proportion") +
   facet_wrap( ~ long_labs) +
   theme_bw() +
   theme(
      legend.position = c(.4, .05),
      legend.key = element_rect(colour = "transparent", fill = "white"),
      axis.text.x=element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      strip.background = element_blank()
   ) 
)
dev.off()

rm(
   preds, pred_dat, pred_dat_n, rm_data, rm_model_info, rm_post_samps, 
   rm_preds, rm_pred_dat, rm_pred_dat_n, rm_i, comb_q_dat, dat_p,
   match_dat, gof_list, gof_char, latex_tab, lab_mat, body, data, 
   f_mod, fit, footer, g_order, gof_tab, header, min_stats, 
   model_info, mods, n_dim, n_pred, p_dat, post_samps, predictors, rm_fit
)


### END OF CODE ###