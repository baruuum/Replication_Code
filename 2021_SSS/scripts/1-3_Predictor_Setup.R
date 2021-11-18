
## ------------------------------------------------------------------
## Set up
## ------------------------------------------------------------------

# load packages
library("here")
library("data.table")

# load data if necessary
if (!exists("gss_06_valid")) {
   gss_06_valid = fread(here("data","gss_2006.csv"),
                         na.strings=c("NA",""))
}

# variables from GSS used
d_vars = c(
   "id",
   "race",
   "hispanic",
   "coninc",
   "attend",
   "polviews",
   "eth1",
   "eth2",
   "eth3",
   "wrkstat"
)

# predictors
p_vars = d_vars[-1]

# subset data
a_dat = gss_06_valid[,..d_vars]


## ------------------------------------------------------------------
## Recode predictors
## ------------------------------------------------------------------


# race
hisp_org = c("mexico", "puerto rico", "spain", "west indies", "other spanish")
hisp_dum = a_dat$hispanic != "not hispanic"
hisp_dum[is.na(a_dat$hispanic)] =
   a_dat$eth1[is.na(a_dat$hispanic)] %in% hisp_org |
   a_dat$eth2[is.na(a_dat$hispanic)] %in% hisp_org |
   a_dat$eth3[is.na(a_dat$hispanic)] %in% hisp_org

a_dat[race == "white", race_fact := 1]
a_dat[race == "black", race_fact := 4]
a_dat[race == "other", race_fact := 3]
a_dat[hisp_dum & race_fact != 4, race_fact := 2]
a_dat[
   , race_fact := factor(
      race_fact, 
      labels = c(
         "White",
         "Hispanic",
         "Other",
         "Black"
      )
   )
]


# check
# table(a_dat$race, a_dat$race_fact, useNA = "ifany")
# table(a_dat$hispanic, a_dat$race_fact)
new_vars = c("race","race_fact")

# family income 
a_dat[, inc_fact := cut(
   coninc, 
   breaks=quantile(coninc, probs = seq(0, 1, 1/3), na.rm=T), include.lowest = T)
]
a_dat[, inc_fact := factor(inc_fact, labels = c("lowest", "middle", "highest"))]
new_vars = rbind(new_vars, c("coninc", "inc_fact"))

# attending religious service
a_dat[, att_fact := dplyr::recode(
   attend, 
   "every week" = "Regularly",
   "more thn once wk" = "Regularly",
   "nrly every week" = "Regularly",
   "2-3x a month" = "At least once a month",
   "once a month" = "At least once a month",
   "never" = "Never/Rarely",
   "lt once a year" = "Never/Rarely",
   "once a year" = "Never/Rarely",
   "sevrl times a yr" = "Never/Rarely")
]
a_dat[, att_fact := factor(
   att_fact, 
   levels = c(
      "Regularly",
      "At least once a month",
      "Never/Rarely")
   )
]
new_vars = rbind(new_vars,c("attend", "att_fact"))

# lib-con
a_dat[, sym_ideo := dplyr::recode(
   polviews,
   "extremely liberal" = 1,
   "liberal" = 2,
   "slightly liberal" = 3,
   "moderate" = 4,
   "slghtly conservative" = 5,
   "conservative" = 6,
   "extrmly conservative" = 7
)]
new_vars = rbind(new_vars, c("polviews","sym_ideo"))

a_dat[
   ,sym_3 := ifelse(sym_ideo %in% 1:3, 1, ifelse(sym_ideo == 4, 2, 3))]
a_dat[
   ,sym_3 := factor(
      sym_3, 
      levels = c(2, 1, 3), 
      labels = c("Moderate", "Liberal", "Conservative")
   )
]
new_vars = rbind(new_vars, c("polviews","sym_3"))

# unemployed
a_dat[, unemp := ifelse(wrkstat %in% c("unempl, laid off"), 1, 0)]
new_vars = rbind(new_vars, c("wrkstat", "unemp"))

rownames(new_vars) = NULL
colnames(new_vars) = c("org", "derived")

# save
saveRDS(new_vars, here("data", "recoded_vars.rds"))
saveRDS(a_dat[, c("id", new_vars[, 2]), with = FALSE], here("data", "predictors.rds"))

## END of CODE ##