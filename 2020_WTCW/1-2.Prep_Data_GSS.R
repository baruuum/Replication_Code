###############################################################################
##                                                                           ##
## Data Preparation GSS                                                      ##
##                                                                           ##
##                                                                           ##
## Data Used: Cumulative GSS 1972-2012                                       ##
##                                                                           ##
##                                                                           ##
## Raw Data File: 36797-0001-Data.tsv                                        ##
## (Data files can be downloaded from "http://gss.norc.org")                 ##
##                                                                           ##
##                                                                           ##
## Data Output:                                                              ##
##                                                                           ##
## 1) Recoded data: 'gss.recoded.72.16.rds'                                  ##
## 2) Variable information: 'gss.var.sum.rds'                                ## ##                                                                           ##
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

message('\n\nPreparing GSS Data --------------------')

## Reading in GSS, 1972-2012 --------------------------------------------------

message('Loading GSS Data ...')

# read in raw data
gss <- fread(
   paste0(
      raw.data.path,
      '36797-0001-Data.tsv'
      ),
   header=T
   )
# change var. names to lower case
names(gss) <- tolower(names(gss))

# moral variables in replicating core
moral.vars.raw <- c('prayer','letdie1','suicide1','suicide2',
                    'suicide3','suicide4','pillok','divlaw',
                    'spanking','marhomo','sexeduc','premarsx',
                    'teensex','xmarsex','homosex','pornlaw',
                    'spkhomo','colhomo','libhomo','cappun',
                    'grass','abdefect','abnomore','abhlth',
                    'abpoor','abrape','absingle','abany',
                    'fepol','fechld','fepresch','fefam',
                    'fehire','fejobaff','discaffm','discaffw')

# demographics
d.vars <- c('partyid', 'degree', 'coninc', 'region', 'age', 'race', 'sex')

# select variables
vars.2.use <- c('id', 'wtssnr', 'year', d.vars, moral.vars.raw)
gss.dat <- gss[, ..vars.2.use]

# save
saveRDS(
   gss.dat,
   paste0(
      data.path,
      'gss.var.select.rds'
   )
)

## Recoding variables ---------------------------------------------------------

message('Recoding and Renaming Variables ...')

# moral issues
gss.dat[, prayer := recode(prayer, 'c(0,8,9)=NA')]
gss.dat[, letdie1 := recode(letdie1, 'c(0,8,9)=NA')]
gss.dat[, suicide1 := recode(suicide1, 'c(0,8,9)=NA')]
gss.dat[, suicide2 := recode(suicide2, 'c(0,8,9)=NA')]
gss.dat[, suicide3 := recode(suicide3, 'c(0,8,9)=NA')]
gss.dat[, suicide4 := recode(suicide4, 'c(0,8,9)=NA')]
gss.dat[, pillok := recode(pillok, 'c(0,8,9)=NA')]
gss.dat[, divlaw := recode(divlaw, '2=3;3=2;c(0,8,9)=NA')]
gss.dat[, spanking := recode(spanking, '1=4;2=3;3=2;4=1;c(0,8,9)=NA')]
gss.dat[, marhomo := recode(marhomo, 'c(0,8,9)=NA')]
gss.dat[, sexeduc := recode(sexeduc, 'c(0,3,8,9)=NA')]
gss.dat[, premarsx := recode(premarsx, '1=4;2=3;3=2;4=1;c(0,8,9)=NA')]
gss.dat[, teensex := recode(teensex, '1=4;2=3;3=2;4=1;c(0,8,9)=NA')]
gss.dat[, xmarsex := recode(xmarsex, '1=4;2=3;3=2;4=1;c(0,8,9)=NA')]
gss.dat[, homosex := recode(homosex, '1=4;2=3;3=2;4=1;c(0,5,8,9)=NA')]
gss.dat[, pornlaw := recode(pornlaw, '1=2; c(2,3)=1;c(0,8,9)=NA')]
gss.dat[, spkhomo := recode(spkhomo, 'c(0,8,9)=NA')]
gss.dat[, colhomo := recode(colhomo, 'c(0,8,9)=NA')]
gss.dat[, libhomo := recode(libhomo, '1=2;2=1;c(0,8,9)=NA')]
gss.dat[, cappun := recode(cappun, '1=2;2=1;c(0,8,9)=NA')]
gss.dat[, grass := recode(grass, 'c(0,8,9)=NA')]
gss.dat[, abdefect := recode(abdefect, 'c(0,8,9)=NA')]
gss.dat[, abnomore := recode(abnomore, 'c(0,8,9)=NA')]
gss.dat[, abhlth := recode(abhlth, 'c(0,8,9)=NA')]
gss.dat[, abpoor := recode(abpoor, 'c(0,8,9)=NA')]
gss.dat[, abrape := recode(abrape, 'c(0,8,9)=NA')]
gss.dat[, absingle := recode(absingle, 'c(0,8,9)=NA')]
gss.dat[, abany := recode(abany, 'c(0,8,9)=NA')]
gss.dat[, fepol := recode(fepol, '1=3;2=1;8=2;c(0,9)=NA')]
gss.dat[, fechld := recode(fechld, 'c(0,8,9)=NA')]
gss.dat[, fepresch := recode(fepresch, '1=4;2=3;3=2;4=1;c(0,8,9)=NA')]
gss.dat[, fefam := recode(fefam, '1=4;2=3;3=2;4=1;c(0,8,9)=NA')]
gss.dat[, fehire := recode(fehire, 'c(0,8,9)=NA')]
gss.dat[, fejobaff := recode(fejobaff, 'c(0,8,9)=NA')]
gss.dat[, discaffm := recode(discaffm, '1=4;2=3;3=2;4=1;c(0,8,9)=NA')]
gss.dat[, discaffw := recode(discaffw, 'c(0,8,9)=NA')]

# party id
gss.dat[,partyid := as.numeric(partyid)]
gss.dat[,partyid := recode(partyid, 'c(0,8,9)=NA')]
gss.dat[,pid.3 := recode(partyid, "c(1,2,3)=1; 4=2;c(5,6,7)=3")]

# educ: 1='at least some college', 0='high school diploma or lower'
gss.dat[, degree := ifelse(degree %in% c(2,3,4), 1, 0)]
gss.dat[is.na(degree), degree := NA]

# fam.inc: 0='0-33 percentile', 1='68-100 percentile'
gss.dat[
   coninc == 0, coninc:= NA
][
   , c('inc.q1','inc.q2') := list(quantile(coninc, .34, na.rm=T),
                   quantile(coninc, .67, na.rm=T)),
   by=year
][
   , coninc := ifelse(coninc < inc.q1, 
                      1, ifelse(coninc >= inc.q1 & coninc < inc.q2,
                                2, 3))
][
   , `:=`(inc.q1 = NULL, inc.q2 = NULL)
]

# south
gss.dat[, region := ifelse(region %in% 5:7, 1,0)]

# female
gss.dat[, sex := ifelse(sex == 1, 1, 0)]

# black
gss.dat[, race := ifelse(race == 2, 1, 0)]

# age
gss.dat[, age := ifelse(age >= 98, NA, age)]

# drop rows with only missings
gss.dat <- gss.dat[rowSums(!is.na(gss.dat[, ..moral.vars.raw])) > 0]

### create summary of suicide and abortion questions

# get abortion questions
ab.quests <- grep('^ab', names(gss.dat), value=T)
# check
message(
   paste0('\nCombining variables: \n\n', 
          paste0(ab.quests,collapse='\n'),
          '\n\ninto one summary measure')
)
# recode them in to 0=no 1=yes
gss.dat[, (ab.quests):=lapply(.SD, recode, '2=0'), .SDcols = ab.quests]
# get observations' missing patterns
tmp  = gss.dat[, rowSums(!is.na(.SD)), .SDcols = ab.quests]
# sum number of "yes" and subtract from max(7)
gss.dat[,abort := length(ab.quests) - rowSums(.SD, na.rm=T), 
        .SDcols=ab.quests]
gss.dat[tmp == 0, abort := NA]

# same for suicide questions
su.quests <- grep('^suicide',names(gss.dat), value=T)

message(
   paste0('\nCombining variables: \n\n', 
          paste0(su.quests,collapse='\n'),
          '\n\ninto one summary measure')
)

gss.dat[, (su.quests):=lapply(.SD, recode, '2=0'), .SDcols=su.quests]
# get observations' missing patterns
tmp  = gss.dat[, rowSums(!is.na(.SD)), .SDcols=su.quests]
# sum number of "yes"
gss.dat[, suicide := length(su.quests) - rowSums(.SD, na.rm=T), 
        .SDcols = su.quests]
gss.dat[tmp == 0, suicide := NA]

# drop individual items for abortion and suicide
moral.vars <- moral.vars.raw[moral.vars.raw %in% 
                             c(ab.quests, su.quests) == F]
moral.vars <- sort(c(moral.vars, 'suicide', 'abort'))

### assign new variable names

# get codebook
c.book.gss <- read.xlsx(
   paste0(wd.base,'/codebook/variable_summary_final.xlsx'),
   sheetName='GSS_Summary_&_Recode',
   stringsAsFactors=F) 
c.book.gss <- 
data.table(c.book.gss)[
   !is.na(Variable.Name), .(Variable.Name,Original.Name)
]
      
# edit entries for summary variables
c.book.gss[
   Variable.Name == 'abortion', Original.Name := 'abort'
][
   Variable.Name == 'suicide', Original.Name := 'suicide'
]

# list of non-issue variables
dem.vars <- c('id','wtssnr','year','partyid','pid.3','degree','coninc',
              'region','age','race', 'sex')

# all variables
gss.vars <- c(moral.vars,dem.vars)

# check whether list is exhaustive
o.order <- c(
   tolower(c.book.gss$Original.Name),
   'wtssnr'
   )
# check
if (sum(sort(gss.vars) != sort(o.order))) {
      
   stop('Some variable missed out! (GSS col-reordering)')
   
}

# select only variables to use
gss.dat <- gss.dat[, ..gss.vars]

# sort variables
setcolorder(gss.dat, o.order)
# assign new variable names
setnames(
   gss.dat, 
   c(
      c.book.gss$Variable.Name,
      'weight'
     )
)

message('Saving Data ...')

# save file 
saveRDS(
   gss.dat, 
   paste0(
      data.path,
      'gss.recoded.rds'
      )
   )

### Save variable information

# variable names
var.sum.gss <- data.table(
   i.vars.label = c.book.gss$Variable.Name
)[
   i.vars.label %in% 
      c.book.gss[tolower(Original.Name) %in% moral.vars, Variable.Name]
]

# get max i.vars.no from NES
max.i.nes <- readRDS(
   paste0(
      data.path,
      'nes.var.sum.rds'
   )
)[,max(i.vars.no)]

# create GSS i.vars.no
var.sum.gss[,i.vars.no := (max.i.nes + 1):(max.i.nes + .N)]
var.sum.gss[,i.vars.class := 3]

message('Saving Variable Information ...')

saveRDS(
   var.sum.gss,
   paste0(
      data.path,
      'gss.var.sum.rds'
   )
)

message('Done!\n')

#### END OF CODE ####