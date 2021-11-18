## ------------------------------------------------------------------
## Setup
## ------------------------------------------------------------------

library("here")
library("data.table")
library("survey")

## ------------------------------------------------------------------
## Read and Save Raw data
## ------------------------------------------------------------------

if ("gss_2006.csv" %in% dir(here("data")) == F) {

  
    # read cumulative data
    raw_gss_cum = readstata13::read.dta13(here("rawdata","GSS7214_R3.DTA"))
    setDT(raw_gss_cum)
    
    # select year
    gss_06 = raw_gss_cum[year == 2006, ]
    # indicator for empty columns
    use_cols = names(gss_06)[colSums(!is.na(gss_06)) > 0]
    # drop empty columns
    gss_06_valid = gss_06[, ..use_cols]
    # save as csv
    data.table::fwrite(gss_06_valid, here("data", "gss_2006.csv"))
    
    # drop unnecessary files
    rm(raw_gss_cum, gss_06)

} else {

    # read in data
    gss_06_valid = data.table::fread(
        here("data", "gss_2006.csv"),
        na.string = c("", "NA")
    )

}

# retain sampling weights adjusting for
#   1) non-response sub-sampling
#   2) no. of adults in households
#   3) differential non-response across areas
pop_weights = gss_06_valid[, .(id, wtssnr)]
data.table::fwrite(pop_weights, here("data", "gss_weights.csv"))
rm(pop_weights)

## Get Population Estimates ----------------------------------------------------

# symbolic ideology
w_dat = gss_06_valid[, .(id, wtssnr, polviews, attend)]
w_dat[, ext_lib := ifelse(polviews == "extremely liberal", 1, 0)]
w_dat[, lib := ifelse(polviews %in% c("extremely liberal", "liberal"), 1, 0)]
w_dat[, ext_con := ifelse(polviews == "extrmly conservative", 1, 0)]
w_dat[, con := ifelse(polviews %in% c("extrmly conservative", "conservative"), 1, 0)]
w_dat[is.na(polviews), `:=`(ext_lib = NA, lib = NA, ext_con = NA, con = NA)]

# rel attendance
w_dat[, goatt := ifelse(attend %in% c("every week", "more thn once wk", "nrly every week"), 1, 0)]
w_dat[, noatt := ifelse(attend %in% c("lt once a year", "never", "once a year"), 1, 0)]
w_dat[is.na(attend), `:=`(goatt = NA, noatt = NA)]

# incorporate survey design
w_dat = svydesign(ids= ~1, data = w_dat, weights = w_dat$wtssnr)
# get estimates
ext_lib = prop.table(survey::svytable(~ ext_lib, w_dat))
# print(ext_lib)
# ext_lib
# 0          1
# 0.97010131 0.02989869
lib = prop.table(svytable(~lib, w_dat))
# print(lib)
# lib
# 0         1
# 0.8542491 0.1457509
ext_con = prop.table(svytable(~ext_con, w_dat))
# print(ext_con)
# ext_con
# 0          1
# 0.96381922 0.03618078
con = prop.table(svytable(~con, w_dat))
# print(con)
# con
# 0         1
# 0.8044394 0.1955606
goatt = prop.table(svytable(~goatt,w_dat))
# print(goatt)
# goatt
# 0         1
# 0.6073836 0.3926164
noatt = prop.table(svytable(~noatt, w_dat))
# print(noatt)
# noatt
# 0        1
# 0.461646 0.538354

# collect estimates and save
gss_pop_est = c(ext_lib[2],
                lib[2],
                ext_con[2],
                con[2],
                goatt[2],
                noatt[2])
names(gss_pop_est) = c("ext_lib",
                        "lib",
                        "ext_con",
                        "con",
                        "goatt",
                        "noatt")
write.csv(t(as.matrix(gss_pop_est)),
          here("data", "gss_pop_est.csv"),
          row.names = F)

# drop unnecessary files
rm(gss_06_valid, gss_pop_est, w_dat, ext_lib, lib, ext_con,con, goatt, noatt)


### END OF CODE ###
