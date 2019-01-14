###############################################################################
##                                                                           ##
## Data Preparation (NES)                                                    ##
##                                                                           ##
##                                                                           ##
## Data Used: 1) Cumulative NES 1948-2012                                    ##
##            2) NES 2016                                                    ##
##                                                                           ##
##                                                                           ##
## Raw Data Files: anes_timeseries_cdf_rawdata.txt (1948-2012)               ##
##                 anes_timeseries_2016_rawdata.txt (2016)                   ##
## (Data files can be downloaded from "http://www.electionstudies.org")      ##
##                                                                           ##
## Notes: Internet samples are excluded from the analysis                    ##
##                                                                           ##
##                                                                           ##
## Final Data Output:                                                        ##
##                                                                           ##
## 1) Merged and recoded data: 'nes.merged.n.recoded.72.16.rds'              ##
## 2) Variable information: 'nes.var.sum.rds'                                ##
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


message('\n\nPreparing NES Data --------------------')

## Reading in ANES, 1948-2012 -------------------------------------------------

### Read in cumulative data
message('Loading Cumulative Datafile ...')
cum4812 <- fread(
   paste0(
      raw.data.path,
      "anes_timeseries_cdf_rawdata.txt"
      ),
   header = T, 
   sep = "|")

# drop internet sample of 2012
cum4812 <- cum4812[VCF0017 != 4, ]

# combine abortion issue
# note: opinion towards abortion is measured in two different versions.
#       1) The "by law" phrase was introduced in 1980
#       2) We use the version without "by law" up until 1978
#       3) From 1980 onwards, we use the "by law" version
cum4812[VCF0004 < 1980, VCF0838 := VCF0837]

# selecting vars to be used

dat.cum <- cum4812[,
                    list(
                         VCF0009x, VCF0813,VCF0814,VCF0816,
                         VCF0817,VCF0830,VCF0867a,VCF9013,
                         VCF9014,VCF9015,VCF9016,VCF9017,
                         VCF9018,VCF9037,VCF9039,VCF9040,
                         VCF9041,VCF9042,VCF0806,VCF0809,
                         VCF0839,VCF0879a,VCF0886,VCF0887,
                         VCF0889,VCF0890,VCF0891,VCF0893,
                         VCF0894,VCF9046,VCF9047,VCF9049,
                         VCF9050,VCF9131,VCF9132,VCF9133,
                         VCF0811,VCF0841,VCF0843,VCF0888,
                         VCF0892,VCF9048,VCF0834,VCF0838,
                         VCF0851,VCF0852,VCF0853,VCF0854,
                         VCF0876a,VCF0877a,VCF0878,VCF9043,
                         VCF0110,VCF0114,VCF0301,VCF0113,
                         VCF0101,VCF0105a,VCF0104,VCF0004,
                         VCF0006, VCF0112
                      )
                    ]


# drop pre-1972 years 
dat.cum <- dat.cum[VCF0004 > 1970,]

# save dataset into working directory
saveRDS(dat.cum, paste0(data.path, 'nes.7212.var.select.rds'))


## Prepare 2016 NES for Merger ------------------------------------------------

message('Loading 2016 NES ...')
# load raw data
dat.16 <- fread(
   paste0(
      raw.data.path,
      "anes_timeseries_2016_rawdata.txt"
      ), 
   header = T, 
   sep = "|")

# drop internet sample (and drop survey mode var)
dat.16 <- dat.16[V160501 != 2, ]
dat.16[, V160501 := NULL]

# selecting variables needed
dat.16 <- dat.16[,
                 list( 
                    V160102f, V161198, V162238x, V162243, 
                    V162245, V162244, V162246, V162212, 
                    V162211, V162214, V162213, V161184, 
                    V161189, V161178, V162157, V161211, 
                    V161210, V161206, V161209, V161212, 
                    V161205, V162185, V162184, V162183, 
                    V161181, V161208, V161207, V161232, 
                    V162208, V162207, V162210, V162209, 
                    V161229x, V161230, V161158x, V161270,
                    V161361x, V163001a, V161267, V161310x, 
                    V161342, V160001
                    )
                 ]

# set missing values (all values below zero)
dat.16 <- dat.16[, lapply(.SD, function(w) ifelse(w < 0, NA, w))]


### recode variables to match cumulative data

# immigration (changte to 3-category for comparability with 2000)
dat.16$V162157 <- recode(dat.16$V162157, "2=1; 4=5")
# discrimination against gays
dat.16$V161229x <- recode(dat.16$V161229x, "1=1;2=2;3=4;4=5")
# adoption, gay
dat.16$V161230	<- recode(dat.16$V161230, "2=5")


# recode "haven't thought much about it" to missing
dat.16$V161198 <- recode(dat.16$V161198, '99=NA')
dat.16$V161184 <- recode(dat.16$V161184, '99=NA')
dat.16$V161189 <- recode(dat.16$V161189, '99=NA')
dat.16$V161178 <- recode(dat.16$V161178, '99=NA')
dat.16$V161181 <- recode(dat.16$V161181, '99=NA')
# federal spending variables
dat.16$V161211 <- recode(dat.16$V161211, '2=3; 3=2')
dat.16$V161210 <- recode(dat.16$V161210, '2=3; 3=2')
dat.16$V161206 <- recode(dat.16$V161206, '2=3; 3=2')
dat.16$V161209 <- recode(dat.16$V161209, '2=3; 3=2')
dat.16$V161212 <- recode(dat.16$V161212, '2=3; 3=2')
dat.16$V161205 <- recode(dat.16$V161205, '2=3; 3=2')
dat.16$V161208 <- recode(dat.16$V161208, '2=3; 3=2')
dat.16$V161207 <- recode(dat.16$V161207, '2=3; 3=2')
# abortion (drop "other" responses for consistency)
dat.16$V161232 <- recode(dat.16$V161232, '5=NA')
# census south:
c.south <- c(10, 11, 12, 13, 24, 37, 45, 51, 54, 1, 21, 28, 47, 5, 22, 40, 48)
dat.16$c.south <- as.numeric(dat.16$V163001a %in% c.south)
dat.16[, V163001a := NULL]

# generate survey year
dat.16$year <- 2016

# family income
# 1.  0 to 16 percentile 
# 2.  17 to 33 percentile 
# 3.  34 to 67 percentile 
# 4.  68 to 95 percentile 
# 5.  96 to 100 percentile 
dat.16$V161361x <- recode(dat.16$V161361x, 
                         '1:5=1; 6:11=2; 12:20=3; 21:26=4; 27:28=5')

# education
dat.16$V161270 <- recode(dat.16$V161270, 
                         '1:7=1; c(8,9)=2;10:12=3;13:16=4; c(90,95)=NA')


### rename variables to match cumulative datafile

# call in codebook (cross-walk sheet)
c.book <- read.xlsx(
   paste0(
      wd.base,
      'codebook/variable_summary_final.xlsx'
   ), 
   sheetName='NES_Crosswalk_4812_16',
   stringsAsFactors=F) 
# note: 'NA' will not be recognized by R as a missing value!
#       referencing should be done by strings

c.book <- as.data.table(c.book)[,
                 .(Variable.Name,
                   Cumulative.Dataset,
                   NES.2016..Name.)
                 ] %>%
         setnames(
            c('varname','cumdat','nes16')
         )
# edit abortion var
c.book[varname=='abortion', cumdat:='VCF0838']

# get nes16 vars appearing also in the cumulative dataset as matrix
matched.vars <- c.book[nes16 != 'NA', 
                       list(cumdat,nes16)
                       ] %>%
   as.matrix

# order variables in dataset according to nes16 column and 
# assign new names in the cumdat column

setcolorder(dat.16, matched.vars[, 'nes16'])
setnames(dat.16, matched.vars[, 'cumdat'])

# save
saveRDS(dat.16, 
        paste0(
           data.path,
           'nes.16.recoded.rds'
           )
       )


## Merge 2016 to Cumulative Data File -----------------------------------------

message('Appending 2016 NES to Cumulative File ...')

# read recoded cumulative data file
dat.72.12 <- readRDS(
   paste0(
      data.path,
      'nes.7212.var.select.rds')
)

# check that cumulative data file has all variables in the 16 dat
if (
   setdiff(
      names(dat.16), 
      names(dat.72.12)
   ) %>%
   length != 0
) stop('Variable mistmatch between NES 16 and Cum.Data!')


# compare columns and add empty one
not.in <- names(dat.72.12)[names(dat.72.12) %in% names(dat.16)==F]
for (nn in not.in) {
   dat.16[,(nn):= NA]
}
# match column order
setcolorder(dat.16, names(dat.72.12))

# recode census south into dummy (as in NES 16)
dat.72.12[, VCF0112 := ifelse(VCF0112 == 3, 1, 0)]

# combine datasets
dat.72.16 <- rbind(dat.72.12, dat.16)

# save
saveRDS(
   dat.72.16, 
   paste0(
      data.path,
      'nes.merged.72.16.rds'
      )
   )


## Recode and Rename Variables ------------------------------------------------

message('Recoding and Renaming Variables ...')

# original dimension of dataset (for later use)
org.dim <- dim(dat.72.16)

# issue variables 
dat.72.16[, blacks.pos.change := recode(VCF0813, "c(0,9)=NA")]
dat.72.16[, civil.rights.too.fast := recode(VCF0814, "c(0,9)=NA")]
dat.72.16[, school.integration := recode(VCF0816, "1=0; 2=1; c(0,9)=NA")]
dat.72.16[, school.busing := recode(VCF0817, "c(0,9)=NA")]
dat.72.16[, aid.to.blacks := recode(VCF0830, "c(0,9)=NA")]
dat.72.16[, affirmative.action := recode(VCF0867a, "4=3; 5=4; c(7,8,9)=NA")]
dat.72.16[, ensure.equal.opp := recode(VCF9013, "c(8,9)=NA")]
dat.72.16[, equal.rights.too.far := 
             recode(VCF9014, "1=5; 2=4; 4=2; 5=1; c(8,9)=NA")]
dat.72.16[, equal.chances := recode(VCF9015, "c(8,9)=NA")]
dat.72.16[, unequal.chances := recode(VCF9016, 
                                      "1=5; 2=4; 4=2; 5=1; c(8,9)=NA")]
dat.72.16[, worry.abt.equality := recode(VCF9017, 
                                         "1=5; 2=4; 4=2; 5=1; c(8,9)=NA")]
dat.72.16[, equal.treatment := recode(VCF9018, "c(8,9)=NA")]
dat.72.16[, fair.jobs.4.blacks := recode(VCF9037, "1=0; 5=1; c(0,9)=NA")]
dat.72.16[, diff.blacks.succeed := recode(VCF9039, "c(8,9)=NA")]
dat.72.16[, no.favor.blacks := recode(VCF9040, 
                                      "1=5; 2=4; 4=2; 5=1; c(8,9)=NA")]
dat.72.16[, blacks.try.harder := recode(VCF9041, 
                                        "1=5; 2=4; 4=2; 5=1; c(8,9)=NA")]
dat.72.16[, blacks.deserve.more := recode(VCF9042, "c(8,9)=NA")]
dat.72.16[, health.insurance := recode(VCF0806, "c(0,9)=NA")]
dat.72.16[, guar.jobs.n.income := recode(VCF0809, "c(0,9)=NA")]
dat.72.16[, gov.spend.services := 
             recode(VCF0839, "1=7; 2=6; 3=5; 5=3; 6=2; 7=1; c(0,9)=NA")]
dat.72.16[, immigration.level := recode(VCF0879a, "3=2; 5=3; c(8,9)=NA")]
dat.72.16[, FS.poor := recode(VCF0886, "c(8,9)=NA")]
dat.72.16[, FS.childcare := recode(VCF0887, "c(8,9)=NA")]
dat.72.16[, FS.aids := recode(VCF0889, "c(8,9)=NA")]
dat.72.16[, FS.public.schools := recode(VCF0890, "c(8,9)=NA")]
dat.72.16[, FS.aid.4.college := recode(VCF0891, "c(8,9)=NA")]
dat.72.16[, FS.homeless := recode(VCF0893, "c(8,9)=NA")]
dat.72.16[, FS.welfare := recode(VCF0894, "c(8,9)=NA")]
dat.72.16[, FS.food.stamps := recode(VCF9046, "7=3; c(8,9)=NA")]
dat.72.16[, FS.environment := recode(VCF9047, "7=3; c(8,9)=NA")]
dat.72.16[, FS.soc.security := recode(VCF9049, "7=3; c(8,9)=NA")]
dat.72.16[, FS.assistance.blacks := recode(VCF9050, "7=3; c(8,9)=NA")]
dat.72.16[, less.government := recode(VCF9131, "2=0; c(8,9)=NA")]
dat.72.16[, gov.VS.free.market := recode(VCF9132, "1=0; 2=1; c(8,9)=NA")]
dat.72.16[, gov.too.involved := recode(VCF9133, "2=0; c(8,9)=NA")]
dat.72.16[, urban.unrest := recode(VCF0811, "c(0,9)=NA")]
dat.72.16[, ussr.coop := recode(VCF0841, "c(0,9)=NA")]
dat.72.16[, defense.spending := recode(VCF0843, "c(0,9)=NA")]
dat.72.16[, FS.crime := recode(VCF0888, "c(8,9)=NA")]
dat.72.16[, FS.foreign.aid := recode(VCF0892, "c(8,9)=NA")]
dat.72.16[, FS.space.science := recode(VCF9048, "7=3; c(8,9)=NA")]
dat.72.16[, women.role := recode(VCF0834, "c(0,9)=NA")]
dat.72.16[, abortion := recode(VCF0838, "1=4; 2=3; 3=2; 4=1; c(0,5,9)=NA")]
dat.72.16[, new.lifestyles := recode(VCF0851, 
                                     "1=5; 2=4; 4=2; 5=1; c(8,9)=NA")]
dat.72.16[, moral.behavior := recode(VCF0852, "c(8,9)=NA")]
dat.72.16[, traditional.values := recode(VCF0853, 
                                         "1=5; 2=4; 4=2; 5=1; c(8,9)=NA")]
dat.72.16[, tolerance.diff.values := recode(VCF0854, "c(8,9)=NA")]
dat.72.16[, gay.discrimination := recode(VCF0876a, "4=3; 5=4; c(7,9)=NA")]
dat.72.16[, gay.military := recode(VCF0877a, "4=3; 5=4; c(7,8,9)=NA")]
dat.72.16[, gay.adoption := recode(VCF0878, "1=0; 5=1; c(8,9)=NA")]
dat.72.16[, school.prayer := recode(VCF9043, "c(0,9)=NA")]

# non-issue vars

dat.72.16[, educ.cont := recode(VCF0110, "0=NA")]
dat.72.16[, fam.inc.cont := recode(VCF0114, "0=NA")]
dat.72.16[, age := recode(VCF0101, "0=NA")]
dat.72.16[, black := recode(VCF0105a, "1=0; 2=1; 3:7=0;9=NA")]
dat.72.16[, female := recode(VCF0104, "1=0;2=1")]
dat.72.16[, year := VCF0004]
dat.72.16[, id := VCF0006]
dat.72.16[, pid := recode(VCF0301, "0=NA")]
dat.72.16[, weight := VCF0009x]

### dichotomizing non-issue variables

# educ: 1='at least some college', 0='high school diploma or lower'
dat.72.16[, educ := recode(VCF0110, "c(3,4)=1; c(1,2)=0; c(0)=NA")]

# fam.inc: 0='below the 67th percentile', 1='68-100 percentile'
dat.72.16[, fam.inc := recode(VCF0114, "c(1,2)=1; 3=2; c(4,5)=3; c(0)=NA")]

# c.south: 0=non-south, 1=south
dat.72.16[, c.south := VCF0112]

# pid.3: 1= Dem, 2=Ind, 3=Rep
# note: 'leaners' are included into partisan categories
dat.72.16[, pid.3 := recode(pid, "c(1,2,3)=1; 4=2; c(5,6,7)=3")]

### Drop raw (unrecoded vars) and save
to.keep <- c.book$varname

# drop unnecessary variables
res.dat <- dat.72.16[, ..to.keep]

message('Saving Data ...')

# save dataset #
saveRDS(res.dat, 
        paste0(
           data.path,
           'nes.merged.n.recoded.72.16.rds'
           )
)

### Save variable information

message('Saving Variable Information ...')

# get variable information
var.info <- read.xlsx(
   paste0(
      wd.base,
      'codebook/variable_summary_final.xlsx'
   ), 
   sheetName='NES_Summary',
   stringsAsFactors=F) 

# use only variable label and class
var.info <- as.data.table(var.info)[,
                                .(Variable.Name,
                                  Classification)
                                ] %>%
   setnames(
      c('i.vars.label','i.vars.class')
   )
var.info <- var.info[
      i.vars.class %in% c('Party Identification',
                          'Control',
                          'Year',
                          'ID') == F
      ]
var.info[, i.vars.class := recode(i.vars.class,
                                  "'Economics'=1;
                                  'Civil Rights'=2;
                                  'Morality'=3;
                                  'Foreign Policy'=4")]

# column number in nes dataset
var.info[, i.vars.no := match(var.info$i.vars.label, names(res.dat))]

# check exhaustiveness and order
stopifnot (
   sum(
      var.info$i.vars.no!=1:max(var.info$i.vars.no)
   ) == 0
) 

saveRDS(
   var.info,
   paste0(
      data.path,
      'nes.var.sum.rds'
   )
)

message('Done!\n')

#### END OF CODE ####
