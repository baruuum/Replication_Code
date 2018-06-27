

################################################
################################################
####            HOW ARE WE APART            ####
####                                        ####
####            DATA MANAGEMENT 1           ####
####       Preparing Data for GRM Model     ####
####                                        ####
#### Last Update: 08/09/2016                ####
####                                        ####
################################################
################################################

rm(list=ls())

library(car)

# working directory
wd <- "Working Directory Here!"
if (!isTRUE(getwd()==wd)) setwd(wd)

wd.raw <- paste0(wd, '/raw_data')
#### read cumulative data ####

setwd(wd.raw)
cum4812 <- read.table("anes_timeseries_cdf_rawdata.txt", header=T, sep = "|")
setwd(wd)

#### combining abortion var. ####
### opinion towards abortion is measured in two different versions in 1980. Here we use the "by law" version.

temp <- !is.na(cum4812$VCF0837) 
year80 <- ifelse(cum4812$VCF0004==1980,T,F) 
temp[year80] <- F
cum4812$VCF0838[temp] <- cum4812$VCF0837[temp]

# drop web sample (2012) 

web <- cum4812$VCF0017!=4
cum4812 <- cum4812[web,]


# selecting attitude vars

data.cum <- subset(cum4812, select = c(VCF0301,VCF0006,VCF0004,VCF9043,VCF0878,VCF0877a,VCF0876a,VCF0854,VCF0853,VCF0852,VCF0851,VCF0838,VCF0834,VCF9042,VCF9041,VCF9040,VCF9039,VCF9037,VCF9018,VCF9017,VCF9016,VCF9015,VCF9014,VCF9013,VCF0867a,VCF0830,VCF0816,VCF0814,VCF0813,VCF9133,VCF9132,VCF9131,VCF9050,VCF9049,VCF9047,VCF9046,VCF0894,VCF0893,VCF0891,VCF0890,VCF0889,VCF0887,VCF0886,VCF0839,VCF0809,VCF0806))


#### recoding variables ####

### Recode and rename variables ####

# issue variables 

data.cum$blacks.chan <- recode(data.cum$VCF0813, "c(0,9)=NA")
data.cum$civil.rights.too.fast <- recode(data.cum$VCF0814, "c(0,9)=NA")
data.cum$sch.integ <- recode(data.cum$VCF0816, "1=0; 2=1; c(0,9)=NA")
data.cum$blacks.aid <- recode(data.cum$VCF0830, "c(0,9)=NA")
data.cum$aff.action <- recode(data.cum$VCF0867a, "4=3; 5=4; c(7,8,9)=NA")
data.cum$eq.opp <- recode(data.cum$VCF9013, "c(8,9)=NA")
data.cum$too.much.eq.rights <- recode(data.cum$VCF9014, "1=5; 2=4; 4=2; 5=1; c(8,9)=NA")
data.cum$eq.chances <- recode(data.cum$VCF9015, "c(8,9)=NA")
data.cum$more.eq.chances <- recode(data.cum$VCF9016, "1=5; 2=4; 4=2; 5=1; c(8,9)=NA")
data.cum$less.eq <- recode(data.cum$VCF9017, "1=5; 2=4; 4=2; 5=1; c(8,9)=NA")
data.cum$eq.treat <- recode(data.cum$VCF9018, "c(8,9)=NA")
data.cum$fair.blacks <- recode(data.cum$VCF9037, "1=0; 5=1; c(0,9)=NA")
data.cum$hard.blacks <- recode(data.cum$VCF9039, "c(8,9)=NA")
data.cum$no.favor.blacks <- recode(data.cum$VCF9040, "1=5; 2=4; 4=2; 5=1; c(8,9)=NA")
data.cum$blacks.try.harder <- recode(data.cum$VCF9041, "1=5; 2=4; 4=2; 5=1; c(8,9)=NA")
data.cum$blacks.deserve.more <- recode(data.cum$VCF9042, "c(8,9)=NA")
data.cum$health.ins <- recode(data.cum$VCF0806, "c(0,9)=NA")
data.cum$jobs.guar7 <- recode(data.cum$VCF0809, "c(0,9)=NA")
data.cum$gov.services <- recode(data.cum$VCF0839, "1=7; 2=6; 3=5; 5=3; 6=2; 7=1; c(0,9)=NA")
data.cum$FS.poor <- recode(data.cum$VCF0886, "c(8,9)=NA")
data.cum$FS.childcare <- recode(data.cum$VCF0887, "c(8,9)=NA")
data.cum$FS.aids <- recode(data.cum$VCF0889, "c(8,9)=NA")
data.cum$FS.publicschools <- recode(data.cum$VCF0890, "c(8,9)=NA")
data.cum$FS.aidcollege <- recode(data.cum$VCF0891, "c(8,9)=NA")
data.cum$FS.homeless <- recode(data.cum$VCF0893, "c(8,9)=NA")
data.cum$FS.welfare <- recode(data.cum$VCF0894, "c(8,9)=NA")
data.cum$FS.food.stamps <- recode(data.cum$VCF9046, "7=3; c(8,9)=NA")
data.cum$FS.envir <- recode(data.cum$VCF9047, "7=3; c(8,9)=NA")
data.cum$FS.soc.sec <- recode(data.cum$VCF9049, "7=3; c(8,9)=NA")
data.cum$FS.assist.blacks <- recode(data.cum$VCF9050, "7=3; c(8,9)=NA")
data.cum$gov.lessmore <- recode(data.cum$VCF9131, "2=0; c(8,9)=NA")
data.cum$gov.market <- recode(data.cum$VCF9132, "1=0; 2=1; c(8,9)=NA")
data.cum$gov.big <- recode(data.cum$VCF9133, "2=0; c(8,9)=NA")
data.cum$women.role <- recode(data.cum$VCF0834, "c(0,9)=NA")
data.cum$abort <- recode(data.cum$VCF0838, "1=4; 2=3; 3=2; 4=1; c(0,5,9)=NA")
data.cum$new.lifestyles <- recode(data.cum$VCF0851, "1=5; 2=4; 4=2; 5=1; c(8,9)=NA")
data.cum$moral.behavior <- recode(data.cum$VCF0852, "c(8,9)=NA")
data.cum$trad.values <- recode(data.cum$VCF0853, "1=5; 2=4; 4=2; 5=1; c(8,9)=NA")
data.cum$different.values <- recode(data.cum$VCF0854, "c(8,9)=NA")
data.cum$homosex <- recode(data.cum$VCF0876a, "4=3; 5=4; c(7,9)=NA")
data.cum$gay.military <- recode(data.cum$VCF0877a, "4=3; 5=4; c(7,8,9)=NA")
data.cum$adopt <- recode(data.cum$VCF0878, "1=0; 5=1; c(8,9)=NA")
data.cum$sch.pray.1 <- recode(data.cum$VCF9043, "c(0,9)=NA")



# non-issue vars

data.cum$year <- data.cum$VCF0004
data.cum$id <- data.cum$VCF0006
data.cum$pid <- recode(data.cum$VCF0301, "0=NA")

# pid.3: 1= Dem, 2=Ind, 3=Rep
# note: 'leaners' are included into partisan categories
data.cum$pid.3 <- recode(data.cum$pid, "c(1,2,3)=1; 4=2; c(5,6,7)=3")

#### drop raw (unrecoded vars) ####

var.labels <- c('blacks.chan','civil.rights.too.fast','sch.integ','blacks.aid','aff.action','eq.opp','too.much.eq.rights','eq.chances','more.eq.chances','less.eq','eq.treat','fair.blacks','hard.blacks','no.favor.blacks','blacks.try.harder','blacks.deserve.more','health.ins','jobs.guar7','gov.services','FS.poor','FS.childcare','FS.aids','FS.publicschools','FS.aidcollege','FS.homeless','FS.welfare','FS.food.stamps','FS.envir','FS.soc.sec','FS.assist.blacks','gov.lessmore','gov.market','gov.big','women.role','abort','new.lifestyles','moral.behavior','trad.values','different.values','homosex','gay.military','adopt','sch.pray.1','pid','pid.3','year','id')


### Generate Data for IRT ###

# drop unnecessary variables
model.dat <- data.cum[, var.labels]


# change order of issue items
# first : gov.services
# last : abortion

model.dat <- model.dat[, c(19, 1:18,20:34, 36:43,35,44:ncol(model.dat))]

# drop pre 1986 years and 2002

model.dat <- model.dat[model.dat$year> 1985 & model.dat$year!=2002,]

# Generate classification var for issues

i.vars.label <- names(model.dat)[c(1:43)]
i.vars.class <- c(1,rep(2,16),rep(1,16),rep(3,10))
i.vars.no <- 1:43
i.vars.summ <- cbind(i.vars.no, i.vars.label, i.vars.class)

# basic parameters

years.measured <- sort(unique(model.dat$year))
no.years<-length(years.measured)
no.i.vars <- length(i.vars.label)
no.vars <- dim(model.dat)[2]
no.obs <- dim(model.dat)[1]

# Summary Matrix

temp <- rep(NA, no.i.vars)
count <- 0
for (var in 1:no.i.vars) {
    count <- count + 1
    m.no <- 0
    for (y in 1:no.years) {
        if (sum(!is.na(model.dat[[i.vars.summ[var,2]]][model.dat$year==years.measured[y]])) > 0)
            m.no <- m.no + 1
    }
    temp[count] <- m.no
}


i.vars.summ <- cbind(i.vars.summ, temp)
colnames(i.vars.summ)[4] <- "no.measured.years"

## New Variable Labels 

new.l <- matrix(c('gov.services','blacks.chan','civil.rights.too.fast','sch.integ','blacks.aid','aff.action','eq.opp','too.much.eq.rights','eq.chances','more.eq.chances','less.eq','eq.treat','fair.blacks','hard.blacks','no.favor.blacks','blacks.try.harder','blacks.deserve.more','health.ins','jobs.guar7','FS.poor','FS.childcare','FS.aids','FS.publicschools','FS.aidcollege','FS.homeless','FS.welfare','FS.food.stamps','FS.envir','FS.soc.sec','FS.assist.blacks','gov.lessmore','gov.market','gov.big','women.role','new.lifestyles','moral.behavior','trad.values','different.values','homosex','gay.military','adopt','sch.pray.1','abort','gov.spend.services','blacks.pos.change','civil.rights.too.fast','school.integration','aid.to.blacks','affirmative.action','ensure.equal.opp','equal.rights.too.far','equal.chances','unequal.chances','worry.abt.equality','equal.treatment','fair.jobs.4.blacks','hard.4.blacks.succeed','no.favor.blacks','blacks.try.harder','blacks.deserve.more','health.insurance','guar.jobs.&.income','FS.poor','FS.childcare','FS.aids','FS.public.schools','FS.aid.4.college','FS.homeless','FS.welfare','FS.food.stamps','FS.environment','FS.soc.security','FS.assistance.blacks','less.government','gov.VS.free.market','gov.too.involved','women.role','new.lifestyles','moral.behavior','traditional.values','tolerance.diff.values','gay.discrimination','gay.military','gay.adoption','school.prayer','abortion'),byrow=F, ncol=2)
colnames(new.l) <- c('i.vars.label','new.label')

# check order & merge
sum(i.vars.summ[,2] != new.l[,1])
i.vars.summ <- cbind(i.vars.summ, new.l[,2])

saveRDS(i.vars.summ,'summary.Rda')

i.vars.summ <- readRDS('summary.Rda')


# unique identifier
model.dat$unique <- model.dat$year*10000 + model.dat$id

# drop respondents with no valid responses
na.dum.data <- rowSums(!is.na(model.dat[,1:no.i.vars]))
full.dat <- model.dat[na.dum.data > 0,]

# number of obs.

dim(full.dat)[1]

# row numbers
full.dat$unique.no <- 1:dim(full.dat)[1]



# save data
saveRDS(full.dat, 'full.dat.Rda')
full.dat <- readRDS('full.dat.Rda')

# change dummies from (0,1) to (1,2)
temp.dat <- full.dat[, c(1:no.i.vars)]
n.cats <- apply(temp.dat, 2, function(x){sum(!is.na(unique(x)))})
temp.dat[, n.cats==2] <- temp.dat[,n.cats==2]+1

# generating data for cut points
cats <- apply(temp.dat, 2, max, na.rm=T)
cuts <- cats - 1
end.cuts <- cumsum(cuts)
start.cuts <- end.cuts - cuts +1
n.cuts <- sum(cuts)

cut.dat <- list(cats, cuts, end.cuts, start.cuts, n.cuts)
names(cut.dat) <- c('cats','cuts','end.cuts','start.cuts','n.cuts')
# save cut point data
saveRDS(cut.dat, 'cut.dat.Rda')

cut.dat <- readRDS('cut.dat.Rda')

### data for analysis: ###

dim.2.dat.full <- full.dat
temp.dat <- dim.2.dat.full[, c(1:nrow(i.vars.summ),ncol(dim.2.dat.full))]
n.cats <- apply(temp.dat, 2, function(x){sum(!is.na(unique(x)))})
temp.dat[, n.cats==2] <- temp.dat[,n.cats==2]+1

# allocate new unique.no (since some years are excluded, these are different from the original 'unique.no' variables)

dim.2.old.unique.no <- temp.dat$unique.no
temp.dat$unique.no <- 1:nrow(temp.dat)

# change data format from wide to long
names(temp.dat) <- c(paste('item',1:nrow(i.vars.summ), sep=''), 'unique.no')
long.dat.2 <- reshape(temp.dat, varying=names(temp.dat)[1:nrow(i.vars.summ)], v.names='response',idvar='unique.no', timevar='item',direction='long')

# drop NAs
sum(tapply(long.dat.2$response, long.dat.2$unique.no, function(x){sum(!is.na(x))})==0)
long.dat.2.re <- long.dat.2[!is.na(long.dat.2$response),]

# check reshaping

for (ii in 1:no.i.vars) {
    print(paste0('item no.',ii,' (', names(dim.2.dat.full)[ii],') :', ifelse(sum(table(long.dat.2.re$response[long.dat.2.re$item==ii])!=table(dim.2.dat.full[,ii]))==0,'OK','!!!!!!!!!!!!!!!!!!')))
}


# save data #
saveRDS(long.dat.2.re, 'grm.dat.Rda')

dim.2.dat <- readRDS('grm.dat.Rda')

### data for corr-year model ###

temp.dat <- dim.2.dat.full[, c(1:nrow(i.vars.summ),46,ncol(dim.2.dat.full))]
n.cats <- apply(temp.dat, 2, function(x){sum(!is.na(unique(x)))})
temp.dat[, n.cats==2] <- temp.dat[,n.cats==2]+1

# allocate new unique.no (since some years are excluded, these are different from the original 'unique.no' variables)

dim.2.old.unique.no.year <- temp.dat$unique.no
temp.dat$unique.no <- 1:nrow(temp.dat)

# change data format from wide to long
names(temp.dat) <- c(paste('item',1:nrow(i.vars.summ), sep=''), 'year','unique.no')
temp.dat$year <- recode(temp.dat$year, '1986=1; 1988=2; 1990=3; 1992=4; 1994=5; 1996=6; 1998=7; 2000=8; 2004=9; 2008=10; 2012=11')

# generate small dataset that maches id to year
tt.dat <- temp.dat[, c('unique.no','year')]
saveRDS(tt.dat, 'time.id.dat.Rda')

long.dat.3 <- reshape(temp.dat, varying=names(temp.dat)[1:nrow(i.vars.summ)], v.names='response',idvar=c('year','unique.no'), timevar='item',direction='long')

# check reshaping

for (ii in 1:no.i.vars) {
    print(paste0('item no.',ii,' (', names(dim.2.dat.full)[ii],') :', ifelse(sum(table(long.dat.2.re$response[long.dat.2.re$item==ii])!=table(dim.2.dat.full[,ii]))==0,'OK','!!!!!!!!!!!!!!!!!!')))
}

# drop NAs
sum(tapply(long.dat.2$response, long.dat.2$unique.no, function(x){sum(!is.na(x))})==0)
long.dat.3.re <- long.dat.3[!is.na(long.dat.3$response),]

# save data #
saveRDS(long.dat.3.re, 'grm.year.dat.Rda')


#### END OF CODE ####