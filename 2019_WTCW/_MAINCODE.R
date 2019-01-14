###############################################################################
##                                                                           ##
##  Polarization and Secular Trends, Main Code                               ##
##                                                                           ##
###############################################################################


## Basic Setup ----------------------------------------------------------------

# clear environment
rm(list=ls())

# specify base directory
wd.base <- "working directory here!"

# specify the number of cores to use in the computation
n.cores <- 6

# set paths
if (!exists('code.path'))
   code.path <- paste0(wd.base, 'code/')
if (!exists('raw.data.path')) 
   raw.data.path <- paste0(wd.base, 'raw_data/')
if (!exists('data.path'))
   data.path <- paste0(wd.base, 'data/')
if (!exists('graphics.path')) 
   graphics.path <- paste0(wd.base, 'plots/')
if (!exists('stan.path'))
   stan.path <- paste0(wd.base, 'stan/')
if (!exists('tables.path'))
   tables.path <- paste0(wd.base,'tables/')

# create directories
if (!dir.exists(data.path)) { 
   # directory to store results
   message(paste0('Creating Directory ~/data/ into ',
                  wd.base)
           )
	dir.create(data.path, recursive=T)
}

if (!dir.exists(graphics.path)) {
   # directory to store graphics
   message(paste0('Creating Directory ~/plots/ into ',
                  wd.base)
           )
   dir.create(graphics.path, recursive=T)
}

if (!dir.exists(stan.path)) {
   # directory for STAN results
   message(paste0('Creating Directory ~/stan/ into ',
                  wd.base)
	        )
	dir.create(stan.path, recursive=T)
}

if (!dir.exists(tables.path)) {
   # directory for tables
   message(paste0('Creating Directory ~/tables/ into ', wd.base))
   dir.create(tables.path)
}



## Install Needed Libraries ---------------------------------------------------

# libraries needed for replication
needed.libs <- c('dplyr',
                 'data.table',
                 'dtplyr',
                 'ggplot2',
                 'rstan',
                 'loo',
                 'car',
                 'survey',
                 'cowplot',
                 'xtable',
                 'gridExtra',
                 'xlsx',
                 'doParallel')

# libraries not installed
need.inst <- needed.libs[
   !(needed.libs %in% installed.packages()[,'Package'])
]

# install libraries
for (ii in need.inst) {
   install.packages(ii, repos='http://cran.rstudio.com')
}

# check whether installation was successful (and load)
check.inst <- sapply(needed.libs, 
                     function(w) {
                        suppressMessages(require(w, character.only=T))
                     })

if (sum(check.inst) != length(need.inst) &
    sum(check.inst) != length(needed.libs)) {
   stop(
      paste0('Some packages could not be installed. ',
             'This might cause the code to crash. ',
             'Try to install the following packages manually :\n\n  ',
             paste0(names(check.inst[check.inst == F]), 
                    collapse=', ')
      )
   )
}



## Load Function Definitions and Set Options ----------------------------------

# load functions
source(paste0(code.path, '0.Functions.R'))

# create list of objects NOT to remove during analysis
paths.and.funs <- c(
   # paths
   grep('\\.path|wd\\.base', ls(), value=T), 
   # defined functions
   fun.names,
   # number of cores used in parallel processing
   'n.cores',
   # this object itself
   'paths.and.funs'
)

# set R seed
set.seed(19841987)
# load seeds for STAN
stan.seeds <- fread(paste0(raw.data.path, 'stan.seeds.csv'))



## Hypothetical Graphs --------------------------------------------------------

source(paste0(code.path, '0-1.Illustrations.R'))



# Preparing Data for Analysis ------------------------------------------------

## Prepare NES data
source(paste0(code.path, '1-1.Prep_Data_NES.R'))

### Prepare GSS data
source(paste0(code.path, '1-2.Prep_Data_GSS.R'))

### Generate %lib data and merge
source(paste0(code.path, '1-3.Gen_Lib_Data.R'))

# remove unnecessary objects
rm(list = ls()[ls() %in% paths.and.funs == F])
invisible(gc())



## Plotting Descriptive Trends ------------------------------------------------

source(paste0(code.path, '2.Descriptive_Trends.R'))

# remove unnecessary objects
rm(list = ls()[ls() %in% paths.and.funs == F])
invisible(gc())



## Compile STAN Models and Prepare Data ---------------------------------------

### Compiling STAN models

source(paste0(code.path, '3-1.Compile_STAN.R'))

### Prepare Data

# generate STAN data
long.dat <- readRDS(
   paste0(data.path, 'comb.dat.rds')
) %>%
   melt(
      id.vars = c(
         'year',
         'i.vars.no',
         'i.vars.label',
         'i.vars.class',
         'data.source'
      ),
      variable.name = 'pid',
      value.name = 'pr.agree'
   )

# drop aggregate props & missings
long.dat <- long.dat[pid != 'props' & !is.na(pr.agree)]
# scale years (into decades)
long.dat[, c.year := (year - 1994) / 10]
long.dat[, c.year.2 := c.year ^ 2]
long.dat[, c.year.3 := c.year ^ 3]
# dummies for partisan groups
long.dat[, ind := ifelse(pid == 'props.i', 1, 0)]
long.dat[, dem := ifelse(pid == 'props.d', 1, 0)]

# order & save
long.dat <- long.dat[order(i.vars.no, pid)]
saveRDS(long.dat, paste0(data.path, 'long.comb.dat.rds'))



# Fit Models -----------------------------------------------------------------

# set basic STAN parameters
options(mc.cores = n.cores)
n.warmup <- 2000
n.iter <- 3000
n.refresh <- 0

# tuning parameters in the case of divergent transitions

# add iterations (warmup is increased by same number)
add.iter <- 2000
# adapt delta
a.delta <- .99
# initial stepsize
s.size <- .01
# maximum treedepth
m.depth <- 12


## Run STAN 

source(paste0(code.path, '3-2.Model_Fitting.R'))

### Check for divergence and refit

for (domain in 1:4) {
   source(paste0(code.path, '3-3.Check_Div_&_Refit.R'))
}

# remove unnecessary objects
rm(list = ls()[ls() %in% paths.and.funs == F])
invisible(gc())

### Compare model fit with WAIC and LOO

# number of digits to round up to in tables
dig <- 2

# run code
source(paste0(code.path, '3-4.Model_Comparison.R'))



## Summarize Models -----------------------------------------------------------

# selected models (in order, economic, civil rights, moral, foreign)
s.models <- c('quad.bn', 'cubic.bn', 'cubic.bn', 'linear.bn')
# set digits to be displayed in tables
dig <- 3
# number of draws for posterior predictive checks
n.ppc <- 50

# load reshaped data
long.dat <- readRDS(
   paste0(data.path, 'long.comb.dat.rds')
)

# Extract posterior samples & posterior predictive checks
for (domain in 1:4) {
   source(paste0(code.path, '4-1.Extract_Samps_&_PPC.R'))
}

# summary tables
source(paste0(code.path, '4-2.Model_Tables.R'))

# plots
source(paste0(code.path, '4-3.Model_Plots.R'))

# remove objects
rm(list = ls()[ls() %in% paths.and.funs == F])
invisible(gc())



## Results for Appendix -------------------------------------------------------

# Analyzing only questions on "absolute" (vs. relative) opinion in the 
# economic domain
source(paste0(code.path, '5-1.APP_Econ_Refit.R'))

# Regression models adjusting for population change
source(paste0(code.path, '5-2.APP_Comp_Changes.R'))

rm(list = ls()[ls() %in% paths.and.funs == F])
invisible(gc())



## Numbers Cited in Text ------------------------------------------------------


source(paste0(code.path, '6-1.Nums_in_Text.R'))


## Session Info ---------------------------------------------------------------

sessionInfo()


#### END OF CODE ####



