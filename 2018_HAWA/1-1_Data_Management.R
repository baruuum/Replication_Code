################################################
################################################
####            HOW ARE WE APART            ####
####                                        ####
####            DATA MANAGEMENT 1           ####
####       Preparing Data for DIF Model     ####
####                                        ####
#### Last Update: 08/09/2016                ####
####                                        ####
################################################
################################################


rm(list=ls())

library(gdata)
library(foreign)

#set wd
wd <- "Working Directory Here"
if (getwd() != wd) setwd(wd)

wd.raw <- paste0(wd, '/raw_data')
setwd(wd.raw)

# get nes raw data
nes.raw <- read.table("anes_timeseries_cdf_rawdata.txt", header=T, sep="|")

setwd(wd)

# presidential years
years.measured <- c(1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012)
no.years <- length(years.measured)

# only years of presidential elections
nes.cum <- nes.raw[nes.raw$VCF0004 %in% years.measured,]

# exclude web-based studies
nes.cum <- nes.cum[nes.cum$VCF0017!=4,]

# variables to be scaled, in order: R-placement, Demparty, Repparty, Pres, Demcand, Repcand, pid
vars <- c('VCF0803', 'VCF0503', 'VCF0504', 'VCF9080', 'VCF9088', 'VCF9096', 'VCF0301')
no.vars <- length(vars)

# missing values
for (v in vars) {
    print(table(nes.cum[[v]]))
}
for (v in vars) {
    nes.cum[[v]][nes.cum[[v]]==0] <- NA
    nes.cum[[v]][nes.cum[[v]]> 7] <- NA
}

#rename vars
nes.cum <- rename.vars(nes.cum, c('VCF0006','VCF0004','VCF0803', 'VCF0503', 'VCF0504', 'VCF9080', 'VCF9088', 'VCF9096', 'VCF0301'),
                       c('id','year','self', 'dem.p', 'rep.p', 'pres', 'dem.cand', 'rep.cand','pid'))

cum.data <- nes.cum[,c('id','year','self', 'dem.p', 'rep.p', 'pres', 'dem.cand', 'rep.cand','pid')]

# summary df for years & candidates
sum.cands <- matrix(c(1972, 'Nixon', 'McGovern', 'Nixon', 1976,"Ford","Carter","Ford",1980,"Carter","Carter","Reagan",1984,"Reagan","Mondale","Reagan",1988,"Reagan","Dukakis","H.W.Bush",1992,"H.W.Bush","Clinton","H.W.Bush",1996,"Clinton","Clinton","Dole",2000,"Clinton","Gore","W.Bush",2004,"W.Bush","Kerry","W.Bush", 2008, 'W.Bush', 'Obama','McCain', 2012, 'Obama', 'Obama','Romney'),ncol=4, byrow=T)
colnames(sum.cands) <- c('year', 'pres', 'dem.cand', 'rep.cand')

# generating separate vars for parties in each year

for (y in years.measured) {
    
    temp.str.dem <- paste('dem.',y, sep='')
    temp.str.rep <- paste('rep.',y, sep='')
    
    cum.data[[temp.str.dem]] <- cum.data$dem.p
    cum.data[[temp.str.dem]][cum.data$year != y] <- NA
    
    cum.data[[temp.str.rep]] <- cum.data$rep.p
    cum.data[[temp.str.rep]][cum.data$year != y] <- NA
    
}

# generating separate vars for each figure

figs <- c('Nixon','McGovern','Ford', 'Carter', 'Reagan', 'Mondale', 'Dukakis', 'H.W.Bush', 'Clinton', 'Dole', 'Gore', 'W.Bush', 'Kerry', 'Obama', 'McCain','Romney')
for (f in figs) {
    
    cum.data[[f]] <- rep(NA,dim(cum.data)[1])
    
    ind <- which(sum.cands==f, arr.ind=T)
    
    for (r in 1:dim(ind)[1]){
        temp.year <- sum.cands[ind[r,][1],1]
        temp.col <- colnames(sum.cands)[ind[r,][2]]
        
        cum.data[[f]][cum.data$year==temp.year] <- cum.data[[temp.col]][cum.data$year==temp.year]
        
    }
    
    print(f)
    print(rbind(table(cum.data[[f]], cum.data$year),colSums(table(cum.data[[f]], cum.data$year))))
}

# generate unique id & sort data set
cum.data$unique <- cum.data$year*10000 + cum.data$id
cum.data <- cum.data[order(cum.data$unique), ]

############# Adding new variables of lib-cons placement ######

## 1972 ##

setwd(wd.raw)

# read 1972 data
nes.1972 <- read.dta('NES1972.dta', convert.factors=F)

# rename
nes.1972 <- rename.vars(nes.1972, c('V720002','V720655', 'V720652'), c('id', 'Wallace', 'self'))
# add year & unique id
nes.1972$year <- 1972
nes.1972$unique <- nes.1972$year*10000 + nes.1972$id
# missings
nes.1972$Wallace[nes.1972$Wallace == 0 | nes.1972$Wallace > 7] <- NA
nes.1972$self[nes.1972$self == 0 | nes.1972$self > 7] <- NA

# test
identical(table(nes.1972$unique, nes.1972$self), table(cum.data$unique[cum.data$year==1972], cum.data$self[cum.data$year==1972]))

# selecting variables to merge
nes.m.1972 <- nes.1972[, c('unique','Wallace')]

# merge data
cum.data <- merge(cum.data, nes.m.1972, by='unique', all.x=T)


## 1976 ##

# read 1976 data
nes.1976 <- read.dta('NES1976.dta', convert.factors=F)

# rename
nes.1976 <- rename.vars(nes.1976, c('V763002', 'V763286', 'V763292', 'V763291', 'V763293', 'V763294', 'V763295', 'V763296'),
                        c('id', 'self', 'Wallace', 'Humphrey', 'McGovern', 'Reagan', 'Mondale', 'Dole'))

# add year & unique id
nes.1976$year <- 1976
nes.1976$unique <- nes.1976$year*10000 + nes.1976$id
nes.1976 <- nes.1976[order(nes.1976$unique),]
# missings
for (f in c('self', 'Wallace', 'Humphrey', 'McGovern', 'Reagan', 'Mondale', 'Dole')) {
    nes.1976[[f]][nes.1976[[f]] == 0 | nes.1976[[f]] > 7] <- NA
    if (max(nes.1976[[f]], na.rm=T) != 7 | min(nes.1976[[f]], na.rm=T) != 1) stop('check range')
}

# test
identical(table(nes.1976$unique, nes.1976$self), table(cum.data$unique[cum.data$year==1976], cum.data$self[cum.data$year==1976]))

# selecting variables to merge
nes.m.1976 <- nes.1976[, c('unique','Wallace', 'Humphrey', 'McGovern', 'Reagan', 'Mondale', 'Dole')]

cands <- c('Wallace', 'Humphrey', 'McGovern', 'Reagan', 'Mondale', 'Dole')
for (v in cands) {
    if (v %in% names(cum.data)) {
        print(v)
        cum.data[cum.data$year==1976, v] <- nes.m.1976[[v]]
    } else cum.data <- merge(cum.data, nes.m.1976[, c('unique',v)], by='unique', all.x=T)        
}

# test
for (f in  cands) {
    print(all.equal(cbind(cum.data$unique[cum.data$year==1976], cum.data[cum.data$year==1976, f ]), 
                    cbind(nes.m.1976$unique, nes.m.1976[[f]] )))
}


## 1980 ##
# read 1976 data
nes.1980 <- read.dta('NES1980.dta', convert.factors=F)
# rename
nes.1980 <- rename.vars(nes.1980, c('V800004', 'V800267', 'V800270', 'V800271', 'V800272', 'V800273', 'V800274', 'V800275', 'V800276', 'V800277'),
                        c('id', 'self', 'T.Kennedy', 'Connally', 'Ford', 'Brown', 'Anderson', 'H.W.Bush', 'Mondale', 'Lucey'))

# add year & unique id
nes.1980$year <- 1980
nes.1980$unique <- nes.1980$year*10000 + nes.1980$id
nes.1980 <- nes.1980[order(nes.1980$unique),]
# missings
for (f in c('self', 'T.Kennedy', 'Connally', 'Ford', 'Brown', 'Anderson', 'H.W.Bush', 'Mondale', 'Lucey')) {
    nes.1980[[f]][nes.1980[[f]] == 0 | nes.1980[[f]] > 7] <- NA
    if (max(nes.1980[[f]], na.rm=T) != 7 | min(nes.1980[[f]], na.rm=T) != 1) stop('check range')
}

# test
identical(table(nes.1980$unique, nes.1980$self), table(cum.data$unique[cum.data$year==1980], cum.data$self[cum.data$year==1980]))

# selecting variables to merge
nes.m.1980 <- nes.1980[, c('unique','T.Kennedy', 'Connally', 'Ford', 'Brown', 'Anderson', 'H.W.Bush', 'Mondale', 'Lucey')]

cands <- c('T.Kennedy', 'Connally', 'Ford', 'Brown', 'Anderson', 'H.W.Bush', 'Mondale', 'Lucey')
# merge data (have to run this twice, don't know why)
for (v in cands) {
    if (v %in% names(cum.data)) {
        print(v)
        cum.data[cum.data$year==1980, v] <- nes.m.1980[[v]]
    } else cum.data <- merge(cum.data, nes.m.1980[, c('unique',v)], by='unique', all.x=T)        
}

# test
for (f in  cands) {
    print(all.equal(cbind(cum.data$unique[cum.data$year==1980], cum.data[cum.data$year==1980, f ]), 
                    cbind(nes.m.1980$unique, nes.m.1980[[f]] )))
}


## 1984 : No new candidates

## 1988

nes.1988 <- read.dta('NES1988.dta', convert.factors=F)
# rename
nes.1988 <- rename.vars(nes.1988, c('V880004','V880228', 'V880233'), c('id', 'self', 'Jackson'))

# add year & unique id
nes.1988$year <- 1988
nes.1988$unique <- nes.1988$year*10000 + nes.1988$id
nes.1988 <- nes.1988[order(nes.1988$unique),]
# missings
for (f in c('self', 'Jackson')) {
    nes.1988[[f]][nes.1988[[f]] == 0 | nes.1988[[f]] > 7] <- NA
    if (max(nes.1988[[f]], na.rm=T) != 7 | min(nes.1988[[f]], na.rm=T) != 1) stop('check range')
}

# test
identical(table(nes.1988$unique, nes.1988$self), table(cum.data$unique[cum.data$year==1988], cum.data$self[cum.data$year==1988]))

# selecting variables to merge
nes.m.1988 <- nes.1988[, c('unique','Jackson')]

cum.data <- merge(cum.data, nes.m.1988 , by='unique', all.x=T)        


# test
print(all.equal(cbind(cum.data$unique[cum.data$year==1988], cum.data[cum.data$year==1988, 'Jackson' ]), 
                cbind(nes.m.1988$unique, nes.m.1988[['Jackson']] )))


## 1992

nes.1992 <- read.dta('NES1992.dta', convert.factors=F)
# rename
nes.1992 <- rename.vars(nes.1992, c('V923004', 'V923509', 'V923516'),
                        c('id', 'self', 'Perot'))

# add year & unique id
nes.1992$year <- 1992
nes.1992$unique <- nes.1992$year*10000 + nes.1992$id
nes.1992 <- nes.1992[order(nes.1992$unique),]
# missings
for (f in c('self', 'Perot')) {
    nes.1992[[f]][nes.1992[[f]] == 0 | nes.1992[[f]] > 7] <- NA
    if (max(nes.1992[[f]], na.rm=T) != 7 | min(nes.1992[[f]], na.rm=T) != 1) stop('check range')
}

# test
identical(table(nes.1992$unique, nes.1992$self), table(cum.data$unique[cum.data$year==1992], cum.data$self[cum.data$year==1992]))

# selecting variables to merge
nes.m.1992 <- nes.1992[, c('unique','Perot')]

cum.data <- merge(cum.data, nes.m.1992 , by='unique', all.x=T)        


# test
print(identical(cbind(cum.data$unique[cum.data$year==1992], cum.data[cum.data$year==1992, 'Perot' ]), 
                cbind(nes.m.1992$unique, nes.m.1992[['Perot']] )))

## 1996 ##

nes.1996 <- read.dta('NES1996.dta', convert.factors=F)

# rename
nes.1996 <- rename.vars(nes.1996, c('V960001', 'V960009', 'V960365', 'V960373'),
                        c('id', 'id.1992','self', 'Perot'))

# add year & unique id
nes.1996$year <- 1996
nes.1996$unique <- nes.1996$year*10000 + nes.1996$id
nes.1996 <- nes.1996[order(nes.1996$unique),]
# missings
for (f in c('self', 'Perot')) {
    nes.1996[[f]][nes.1996[[f]] == 0 | nes.1996[[f]] > 7] <- NA
    if (max(nes.1996[[f]], na.rm=T) != 7 | min(nes.1996[[f]], na.rm=T) != 1) stop('check range')
}

# test
identical(table(nes.1996$unique, nes.1996$self), table(cum.data$unique[cum.data$year==1996], cum.data$self[cum.data$year==1996]))

# selecting variables to merge
nes.m.1996 <- nes.1996[, c('unique','Perot')]

#merge
cum.data[cum.data$year==1996, 'Perot'] <- nes.m.1996[['Perot']]

# test
print(all.equal(cbind(cum.data$unique[cum.data$year==1996], cum.data[cum.data$year==1996, 'Perot']), 
                cbind(nes.m.1996$unique, nes.m.1996[['Perot']] )))

## 2000

nes.2000 <- read.dta('NES2000.dta', convert.factors=F)
nes.2000$std <- ifelse(nes.2000$V000005d==1,T,F)
nes.2000 <- rename.vars(nes.2000, c('V000001', 'V000439', 'V000439a', 'V000475', 'V000475a'),
                        c('id', 'self.ftf','self.t', 'Buchanan.ftf', 'Buchanan.t'))

# add year & unique id
nes.2000$year <- 2000
nes.2000$unique <- nes.2000$year*10000 + nes.2000$id
nes.2000 <- nes.2000[order(nes.2000$unique),]

for (f in c('self.ftf', 'self.t', 'Buchanan.ftf', 'Buchanan.t')) {
    nes.2000[[f]][nes.2000[[f]] > 7] <- 0
}

nes.2000$self <- nes.2000$self.ftf + nes.2000$self.t
nes.2000$Buchanan <- nes.2000$Buchanan.ftf + nes.2000$Buchanan.t

# missings
for (f in c('self', 'Buchanan')) {
    nes.2000[[f]][nes.2000[[f]] == 0 | nes.2000[[f]] > 7] <- NA
    if (max(nes.2000[[f]], na.rm=T) != 7 | min(nes.2000[[f]], na.rm=T) != 1) stop('check range')
}

# test
identical(table(nes.2000$unique, nes.2000$self), table(cum.data$unique[cum.data$year==2000], cum.data$self[cum.data$year==2000]))

# selecting variables to merge
nes.m.2000 <- nes.2000[, c('unique','Buchanan')]

#merge
cum.data <- merge(cum.data, nes.m.2000, by = 'unique', all.x=T)


# test
print(all.equal(cbind(cum.data$unique[cum.data$year==2000], cum.data[cum.data$year==2000, 'Buchanan']), 
                cbind(nes.m.2000$unique, nes.m.2000[['Buchanan']] )))


## 2004 ##

nes.2004 <- read.dta('NES2004.dta', convert.factors=F)
nes.2004 <- rename.vars(nes.2004, c('V040001', 'V043085', 'V043089'),
                        c('id', 'self', 'Nader'))

# add year & unique id
nes.2004$year <- 2004
nes.2004$unique <- nes.2004$year*10000 + nes.2004$id
nes.2004 <- nes.2004[order(nes.2004$unique),]

# missings
for (f in c('self', 'Nader')) {
    nes.2004[[f]][nes.2004[[f]] == 0 | nes.2004[[f]] > 7] <- NA
    if (max(nes.2004[[f]], na.rm=T) != 7 | min(nes.2004[[f]], na.rm=T) != 1) stop('check range')
}

# test
identical(table(nes.2004$unique, nes.2004$self), table(cum.data$unique[cum.data$year==2004], cum.data$self[cum.data$year==2004]))

# selecting variables to merge
nes.m.2004 <- nes.2004[, c('unique','Nader')]

#merge
cum.data <- merge(cum.data, nes.m.2004, by = 'unique', all.x=T)


# test
print(all.equal(cbind(cum.data$unique[cum.data$year==2004], cum.data[cum.data$year==2004, 'Nader']), 
                cbind(nes.m.2004$unique, nes.m.2004[['Nader']] )))


setwd(wd)
saveRDS(cum.data, 'pres.year.Rda')

### END OF CODE ###