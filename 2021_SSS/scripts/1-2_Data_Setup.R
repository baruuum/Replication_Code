
## ------------------------------------------------------------------
## Set up
## ------------------------------------------------------------------

# Libraries
library("here")
library("data.table")

# variables to analyze
acqties = c("acqunemp",
            "acqhome",
            "acqprisn",
            "acqasian",
            "acqblack",
            "acqhisp",
            "acqwhite",
            "acqgay",
            "acqcohab",
            "acqgoatt",
            "acqnoatt",
            "acqlib",
            "acqcon")



## ------------------------------------------------------------------
## Generate data for analysis
## ------------------------------------------------------------------

# load data if necessary
gss_06_valid = fread(here("data", "gss_2006.csv"), na.strings = c("NA",""))

## add id variable to variable list
vars = c("id", acqties)

# copy data
dat = gss_06_valid[,..vars]

# drop rows with only missings (id is always nonmissing)
dat = dat[rowSums(!is.na(dat)) > 1]

# cache dimensions
old_dim = dim(dat)
old_order = names(dat)

# variable classes
cc = sapply(dat, class)
# get character variables
nn = names(dat)[cc == "character" | cc == "factor"]

# recode character variables
if (length(nn) > 0) {
   
   # check characters
   stopifnot(all(grepl("acq", nn)))
   
   dat[
      , (nn) := lapply(.SD, function(w) {
            dplyr::recode(
               w, 
               "0" = 0, 
               "1" = 1,
               "2-5" = 2,
               "6-10" = 3, 
               "more than 10" = 4
            )
         }
      ), 
      .SDcols = nn
  ]
   
}

# check dimensions
if (!all.equal(old_dim, dim(dat))) {
   stop("Dimensions of data changed due recoding!")
}

# check order
if (!all.equal(old_order, names(dat))) {
   stop("Order of variables changed!")
}

# mapping of variable name to item number
item_sum = data.table(item_no = 1:(ncol(dat)-1), 
                      labs = names(dat)[2:ncol(dat)])
res_sum = data.table(new.id = 1:nrow(dat), 
                      old_id = dat[, id])


# save data
message("Saving data ... ")
saveRDS(list(
   org_dat = dat,
   item_sum = item_sum,
   ind.sum = res_sum,
   n_inds = nrow(dat),
   n_items = ncol(dat)-1
   ),
   here("data", "acq_dat.rds")
)

## remove datasets 
rm(dat, item_sum, res_sum)

### END OF CODE ###
