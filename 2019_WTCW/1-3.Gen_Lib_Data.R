###############################################################################
##                                                                           ##
## Generate %Liberal Dataset                                                 ##
##                                                                           ##
##                                                                           ##
## Data Used:                                                                ##
##                                                                           ##
## 1) nes.merged.n.recoded.72.16.rds                                         ##
## 2) gss.recoded.dat                                                        ##
##                                                                           ##
##                                                                           ##
## Data Output:                                                              ##
##                                                                           ##
## 1) comb.dat.rds                                                           ##
##                                                                           ##
###############################################################################

## Basic Setup ----------------------------------------------------------------

# check base directory
if (!exists('wd.base'))
   stop('Need wd.base!')

# set paths
if (!exists('code.path'))
   code.path <- paste0(wd.base, 'code/')
if (!exists('data.path'))
   data.path <- paste0(wd.base, 'data/')
if (!exists('raw.data.path')) 
   raw.data.path <- paste0(wd.base, 'raw_data/')
if (!exists('graphics.path')) 
   graphics.path <- paste0(wd.base, 'plots/')
if (!exists('stan.path'))
   stan.path <- paste0(wd.base, 'stan/')

# load functions if necessary
if (!exists('gen.summary')) 
   source(paste0(code.path, '0.Functions.R'))


message('\nGenerating Dataset of %Liberal Responses ----------')

## Reading in Data to merge ---------------------------------------------------

message('Loading NES and GSS data ...')
# read in recoded data
gss.dat <- readRDS(paste0(data.path, 'gss.recoded.rds'))
gss.var.info <- readRDS(paste0(data.path, 'gss.var.sum.rds'))

nes.dat <- readRDS(paste0(data.path, 'nes.merged.n.recoded.72.16.rds'))
nes.var.info <- readRDS(paste0(data.path, 'nes.var.sum.rds'))

## Generate % Liberal Data ----------------------------------------------------

message('Working on NES Data ...')

### NES

# gen dataset
nes.pr.lib <- nes.dat[
   , gen.summary(.SD, 
                 var.list = nes.var.info$i.vars.label, 
                 weight.var='weight'),
     by = year]

# add other information to data
nes.pr.lib <- merge(nes.pr.lib,
                    nes.var.info,
                    by = 'i.vars.label',
                    all.x = T)
nes.pr.lib[, data.source := 'NES']

# save
saveRDS(nes.pr.lib, paste0(data.path, 'nes.pr.lib.rds'))


### GSS

message('Working on GSS Data ...')

gss.pr.lib <- gss.dat[
   , gen.summary(.SD,
                 var.list = gss.var.info$i.vars.label,
                 weight.var = 'weight'),
   by = year]
gss.pr.lib <- merge(gss.pr.lib,
                    gss.var.info,
                    by = 'i.vars.label',
                    all.x = T)
gss.pr.lib[, data.source := 'GSS']

saveRDS(gss.pr.lib, paste0(data.path, 'gss.pr.lib.rds'))


### Merge datasets

message('Combining and Saving Data ...')

# check if column names agree
stopifnot(
   setequal(names(nes.pr.lib), names(gss.pr.lib))
)

# rename abortion variables
nes.pr.lib[ i.vars.label == 'abortion', i.vars.label := 'abortion.nes']
gss.pr.lib[ i.vars.label == 'abortion', i.vars.label := 'abortion.gss']
           
# reorder columns and merge
comb.dat <- rbind(nes.pr.lib, gss.pr.lib[,names(nes.pr.lib),with=F])

# save data
saveRDS(comb.dat, paste0(data.path, 'comb.dat.rds'))

message('Done!\n')

#### END OF CODE ####