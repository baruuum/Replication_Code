
################################################
################################################
####            HOW ARE WE APART            ####
####                                        ####
####                DIF MODEL               ####
####                                        ####
#### Last Update: 08/09/2016                ####
####                                        ####
################################################
################################################

library(foreign)
library(ggplot2)
library(gridExtra)
library(basicspace)
library(xtable)
library(reshape)
library(car)
library(overlap)
library(scales)

rm(list=ls())


#set wd
wd <- "Working Directory Here!"
if (getwd() != wd) setwd(wd)
wd.raw <- paste0(wd,'/raw_data')
paper.wd <- "Path to directory where figures should be placed"

# read data of voters

setwd(wd.raw)
nes <- read.table('anes_timeseries_cdf_rawdata.txt', header=T, sep='|')
v.dat <- nes[, c('VCF0702', 'VCF0004', 'VCF0006')]
names(v.dat) <- c('voted','year','id')
setwd(wd)
saveRDS(v.dat, 'v.dat.Rda')

# codes: 0=missing, 1=did not vote, 2=voted

v.dat <- readRDS('v.dat.Rda')

#read data
cs.data <- readRDS('pres.year.Rda')

# re-order and calculate basic parameters
cs.data <- cs.data[order(cs.data$unique),]
years.measured <- unique(cs.data$year)
n.years <- length(years.measured)

# common space scores for political figures (presidents with president scores)
cs.score <- matrix(c(0.548,-0.569,0.501,-0.521,-0.404,0.688,-0.447,0.328,-0.46,0.187,0.578,-0.452,-0.336,0.729,-0.368,0.38,-0.373), byrow=T, nrow=1)
colnames(cs.score) <- c('Nixon','McGovern','Ford','Carter','Humphrey','Reagan','Mondale','Dole','T.Kennedy','Anderson','H.W.Bush','Clinton','Gore','W.Bush','Obama','McCain','Kerry')

# common space scores based on congress if possible
# not former congress members: carter, reagan, clinton, bush
cs.cong <- matrix(c(0.167,-0.569,0.291,-0.521,-0.404,0.688,-0.447,0.328,-0.46,0.187,0.208,-0.452,-0.336,0.729,-0.363,0.38,-0.373), byrow=T, nrow=1)
colnames(cs.cong) <- c('Nixon','McGovern','Ford','Carter','Humphrey','Reagan','Mondale','Dole','T.Kennedy','Anderson','H.W.Bush','Clinton','Gore','W.Bush','Obama','McCain','Kerry')

# cf scores (1979-2012)
# no ford, no lucey
# pre-nomination scores used for candidates in presidential elections

cf.score <- matrix(c(-0.380492198,0.986580102,-0.79840705,0.795778965,-0.884942825,0.89302788,-0.65051308,-0.6629713,-0.823537082,-0.761293877,-0.89852274,0.447471293,0.599765887,-0.894493264,0.921898289,1.258247945,-0.956070052,-1.338166943,-1.350869376,0.68018326,0.878902218),byrow=T,nrow=1)

colnames(cf.score) <- c('Carter','Reagan','Mondale','H.W.Bush','T.Kennedy','Connally','Brown','Anderson','Dukakis','Jackson','Clinton','Perot','Dole','Gore','W.Bush','Buchanan','Kerry','Nader','Obama','McCain','Romney')


for (v in colnames(cs.score)){
    print(v)
    print(table(cs.data[[v]], cs.data$year, useNA='ifany'))
}


# read common space data
setwd(wd.raw)
nominate <- read.dta('commonspace2012.DTA')
# align year with congress
mean.cs <- matrix(c(1972, 92, 1976, 94, 1980, 96, 1984, 98, 1988, 100, 1992, 102, 1996, 104, 2000, 106, 2004, 108,2008, 110, 2012,112), ncol=2, byrow=T)
temp.mat <- array(NA, dim=c(11,4))
mean.cs <- cbind(mean.cs, temp.mat)

# calculate party means and medians (common space)
count <- 0
for (y in mean.cs[,2]) {
    count <- count + 1
    mean.cs[count,3] <- mean(nominate$dwnom1[nominate$cong==y & nominate$party==100])
    mean.cs[count,4] <- mean(nominate$dwnom1[nominate$cong==y & nominate$party==200])
    mean.cs[count,5] <- median(nominate$dwnom1[nominate$cong==y & nominate$party==100])
    mean.cs[count,6] <- median(nominate$dwnom1[nominate$cong==y & nominate$party==200])
}
colnames(mean.cs) <- c('year', 'congress', 'mean.dem', 'mean.rep', 'med.dem', 'med.rep')


#using medians of parties
temp.mat <- matrix(c(mean.cs[,5], mean.cs[,6]), nrow=1, byrow=T)
name.vec <- rep(NA, 22)
for (y in 1:11) {
    name.vec[y] <- paste('dem.', mean.cs[y,1], sep='')
    name.vec[y+11] <- paste('rep.', mean.cs[y,1], sep='')
}
colnames(temp.mat) <- name.vec    

cs.score <- cbind(cs.score, temp.mat)
cs.cong <- cbind(cs.cong, temp.mat)
cs.score.a <- t(cs.score)
cs.cong.a <- t(cs.cong)

### calculating party medians of CFscores ##

cf <- read.csv('cfscores.csv')
cf.dat <- cf[cf$cycle %in% years.measured,]

mean.cf <- cbind(mean.cs[, 1:2], rep(NA,dim(mean.cs)[1]),rep(NA,dim(mean.cs)[1]))

count <- 0
for (y in mean.cf[,1]) {
    count <- count + 1
    mean.cf[count,3] <- mean(cf.dat$cfscore[cf.dat$cycle==y & cf.dat$Party==100])
    mean.cf[count,4] <- mean(cf.dat$cfscore[cf.dat$cycle==y & cf.dat$Party==200])
}
colnames(mean.cf) <- c('year', 'congress', 'med.dem', 'med.rep')
mean.cf[1:2,3:4] <- NA

temp.mat <- matrix(c(mean.cf[,3], mean.cf[,4]), nrow=1, byrow=T)
name.vec <- rep(NA, 22)
for (y in 1:11) {
    name.vec[y] <- paste('dem.', mean.cf[y,1], sep='')
    name.vec[y+11] <- paste('rep.', mean.cf[y,1], sep='')
}
colnames(temp.mat) <- name.vec    

cf.score <- cbind(cf.score,temp.mat)
cf.score.a <- t(cf.score)


# stimuli in data
res.stimuli <- cs.data[, c('unique', 'year','self','pid','Nixon','McGovern','Ford','Carter','Humphrey','Reagan','Mondale','Dole','T.Kennedy','Anderson','H.W.Bush','Clinton','Gore','W.Bush','Obama','McCain','Kerry',colnames(cs.score)[18:39])]
n.vars <- dim(res.stimuli)[2]

# check
all.equal(names(res.stimuli[, -c(1,2,3,4)]), colnames(cs.score))
rbind(names(res.stimuli[,-c(1,2,3,4)]), colnames(cs.score))

# use only respondents who placed more than 3 stimuli
temp.dum <- rowSums(!is.na(res.stimuli[, -c(1,2,3,4)]))
self.dum <- !is.na(res.stimuli$self)
res.stimuli.full <- res.stimuli[temp.dum > 2 & self.dum,]

temp.stimuli <- res.stimuli[self.dum & temp.dum>0,]
#centering variables
res.stimuli.full[,3:n.vars] <- res.stimuli.full[, 3:n.vars] - 4

obs <- dim(res.stimuli.full)[1]

# check number of observations for each year
tapply(res.stimuli.full$unique, res.stimuli.full$year, function(x){sum(!is.na(x))})

# save unique identifies of respondents used in the analysis
setwd(wd)
saveRDS(res.stimuli.full$unique, 'symbol.res.Rda')

### Start Projection ###

library(doParallel)
registerDoParallel(cores=3)

#reg cs.scores on respondents' placements
temp.res <- foreach (i=1:obs, .combine='rbind') %dopar% {
    map <- lm(cs.score.a ~ t(res.stimuli.full[i,-c(1,2,3,4)]))
    map2 <- lm(cs.cong.a ~ t(res.stimuli.full[i,-c(1,2,3,4)]))
    res.vec <- c(res.stimuli.full[i,1],coef(map),coef(map2))
    res.vec
}

colnames(temp.res) <- c('unique', 'intercept.pres', 'slope.pres', 'intercept.cong', 'slope.cong')

#merge estimated regression coefficients
c.data <- merge(res.stimuli.full, temp.res, by='unique')

## respondents ideal points
# note: respondents who placed all stimuli on the same position have slope of NA 
c.data$idpt <- c.data$slope.pres*c.data$self + c.data$intercept.pres
s.dum.pres <- is.na(c.data$slope.pres) & !is.na(c.data$intercept.pres)
c.data$idpt[s.dum.pres] <- c.data$intercept.pres[s.dum.pres]
s.dum.cong <- is.na(c.data$slope.cong) & !is.na(c.data$intercept.cong)
c.data$idpt.cong <- c.data$slope.cong*c.data$self + c.data$intercept.cong
c.data$idpt.cong[s.dum.cong] <- c.data$intercept.cong[s.dum.cong]

########## Same process for CF scores ################

res.stimuli.cf <- cs.data[,c('unique','year','self','pid',colnames(cf.score))]
all.equal(names(res.stimuli.cf[,-c(1:4)]), colnames(cf.score))
temp.cf <- rowSums(!is.na(res.stimuli.cf[,-c(1:4)]))
self.cf <- !is.na(res.stimuli.cf$self)
res.stimuli.cf.full <- res.stimuli.cf[temp.cf > 2 & self.cf & res.stimuli.cf$year > 1976,]
tapply(res.stimuli.cf.full$unique, res.stimuli.cf.full$year, function(x){sum(!is.na(x))})
####################


obs.2 <- dim(res.stimuli.cf.full)[1]
n.vars.2 <- dim(res.stimuli.cf.full)[2]
res.stimuli.cf.full[,3:n.vars.2] <- res.stimuli.cf.full[, 3:n.vars.2] - 4
#reg cs.scores on respondents' placements
temp.res <- foreach (i=1:obs.2, .combine='rbind') %dopar% {
    map <- lm(cf.score.a ~ t(res.stimuli.cf.full[i,-c(1,2,3,4)]))
    res.vec <- c(res.stimuli.cf.full[i,1],coef(map))
    res.vec
}

colnames(temp.res) <- c('unique', 'intercept', 'slope')

stopImplicitCluster()

#merge estimated regression coefficients
cf.data <- merge(res.stimuli.cf.full, temp.res, by='unique')

# respondents ideal points
cf.data$idpt <- cf.data$slope*cf.data$self + cf.data$intercept
cf.dum <- is.na(cf.data$slope) & !is.na(cf.data$intercept)
cf.data$idpt[cf.dum] <- cf.data$intercept[cf.dum]
tapply(cf.data$idpt, cf.data$year, function(x){sum(!is.na(x))})


################ a-m scaling  ##############

# summary list of NOMINATE stimuli
sum.stimuli.cs <- list()

count <- 0
for (y in years.measured) {
    
    count <- count + 1
    temp <- names(c.data)[which(colSums(!is.na(c.data[c.data$year==y,]))>1)]
    stimuli <- temp[!(temp %in% c('unique','year','id','self','pid', 'intercept.pres', 'slope.pres','intercept.cong','slope.cong', 'idpt','idpt.cong'))]
    obs.full <- sum(rowSums(is.na(c.data[stimuli][c.data$year==y,]))==0)
    obs.3 <- sum(rowSums(!is.na(c.data[stimuli][c.data$year==y,]))>2)
    sum.stimuli.cs[[count]] <- list(year=y, stimuli=stimuli, obs.full=obs.full, obs.3=obs.3)
}

c.data$idpt.am <- NA
c.data$idpt.check.am <- NA

# A-M ideal points using NOMINATE stimuli
stim.list<-list()

for (y in 1:n.years) {
    temp.data <- c.data[c.data$year==years.measured[y], c('self',sum.stimuli.cs[[y]]$stimuli)]
    temp.data[is.na(temp.data)] <- 999
    res <- aldmck(data=temp.data, polarity=length(sum.stimuli.cs[[y]]$stimuli), respondent=1, missing=999, verbose=T)
    c.data[c.data$year==years.measured[y], c('idpt.am', 'idpt.check.am')] <- res$respondent[,c('idealpt', 'selfplace')]
    stim.list[[y]] <- res$stimuli
}

all.equal(c.data$self, c.data$idpt.check.am)


### Same for CF scores

sum.stimuli.cf <- list()

count <- 0
for (y in years.measured) {
    
    count <- count + 1
    temp <- names(cf.data)[which(colSums(!is.na(cf.data[cf.data$year==y,]))>1)]
    stimuli <- temp[!(temp %in% c('unique','year','id','self','pid', 'intercept', 'slope', 'idpt'))]
    obs.full <- sum(rowSums(is.na(cf.data[stimuli][cf.data$year==y,]))==0)
    obs.3 <- sum(rowSums(!is.na(cf.data[stimuli][cf.data$year==y,]))>2)
    sum.stimuli.cf[[count]] <- list(year=y, stimuli=stimuli, obs.full=obs.full, obs.3=obs.3)
}

cf.data.backup <- cf.data

cf.data$idpt.am <- NA
cf.data$idpt.check.am <- NA

stim.list.cf<-list()

for (y in 3:n.years) {
    temp.data <- cf.data[cf.data$year==years.measured[y], c('self',sum.stimuli.cf[[y]]$stimuli)]
    print(names(temp.data))
    temp.data[is.na(temp.data)] <- 999
    res <- aldmck(data=temp.data, polarity=length(sum.stimuli.cf[[y]]$stimuli), respondent=1, missing=999, verbose=T)
    cf.data[cf.data$year==years.measured[y], c('idpt.am', 'idpt.check.am')] <- res$respondent[,c('idealpt', 'selfplace')]
    stim.list.cf[[y]] <- res$stimuli
}

all.equal(cf.data$self, cf.data$idpt.check.am)


#### Correlation matrix (NOMINATE idpts vs self-placements, cf)

cor.mat <- years.measured
temp.vec <- vector('numeric', length=length(years.measured))
iter <- 0
for (y in years.measured) {
    iter <- iter + 1
    print(cor(c.data$idpt.cong[c.data$year==y], c.data$self[c.data$year==y], use='pairwise'))
    temp.vec[iter] <- cor(c.data$idpt.cong[c.data$year==y], c.data$self[c.data$year==y], use='pairwise')
}
print(cor(c.data$idpt, c.data$self, use='pairwise'))
cor.mat <- cbind(cor.mat, temp.vec)

temp.vec <- vector('numeric', length=length(years.measured))
iter <- 0
for (y in years.measured) {
    iter <- iter + 1
    print(cor(c.data$idpt.cong[c.data$year==y], c.data$idpt.am[c.data$year==y], use='pairwise'))
    temp.vec[iter] <- cor(c.data$idpt.cong[c.data$year==y], c.data$idpt.am[c.data$year==y], use='pairwise')
}
cor.mat <- cbind(cor.mat, temp.vec)
rownames(cor.mat) <- years.measured
colnames(cor.mat) <- c('year','self','am')

temp <- cf.data[, c('unique','idpt')]
names(temp) <- c('unique','idpt.cf')
full.dat <- merge(c.data, temp, by='unique',all.x=T,all.y=T)

temp.vec <- vector('numeric', length=length(years.measured))
iter <- 0
for (y in years.measured) {
    iter <- iter + 1
    print(cor(full.dat$idpt.cong[full.dat$year==y], full.dat$idpt.cf[full.dat$year==y], use='pairwise'))
    temp.vec[iter] <- cor(full.dat$idpt.cong[full.dat$year==y], full.dat$idpt.cf[full.dat$year==y], use='pairwise')
}

cor.mat <- cbind(cor.mat, temp.vec)
colnames(cor.mat)[4] <- 'cf'
temp <- cor.mat[3:nrow(cor.mat),2:4]
xtable(t(temp), digits=3)



### check correlations between stimuli estimates (NOMINATE vs. A-M)

temp <- unlist(stim.list)
temp <- data.frame(stimuli = names(temp), amk = temp)
temp2 <- tapply(temp$amk, temp$stimuli, mean)
temp3 <- data.frame(stimuli=names(temp2), am.cs=temp2)
stim.comp <- data.frame(stimuli=colnames(cs.cong), cs.cong=t(cs.cong))
stim.comp <- merge(stim.comp, temp3, by='stimuli')
with(stim.comp, cor(cs.cong, am.cs))

# reg intercept on self placement

summary(lm(intercept.cong ~ self, data=c.data))
summary(lm(intercept.cong ~ pid, data=c.data))
summary(lm(intercept.cong ~ pid + factor(year), data=c.data))
summary(lm(intercept.cong ~ self + factor(year), data=c.data))



# Polarization?


temp <- cf.data[, c('unique','year','idpt')]
names(temp) <- c('unique','year','cf.idpt')
full.dat <- merge(c.data, temp, by=c('unique','year'), all=T)
temp <- full.dat[full.dat$year>1979, c('year','idpt.cong','cf.idpt')]
temp$cf.idpt <- scale(temp$cf.idpt)
temp$idpt.cong <- scale(temp$idpt.cong)
names(temp) <- c('year','NOMINATE','CFscores')
df <- melt(temp, id.vars='year')

g1 <- ggplot(na.omit(df), aes(x=value, linetype=variable)) + geom_density(alpha=.5, color='black', size=.5) + theme_bw() + facet_wrap(~year) + theme(legend.position="none", axis.title=element_text(size=20),legend.text=element_text(size=20), strip.background = element_rect(fill='grey90'), axis.text=element_text(size=15), legend.key.size=unit(1.5, 'cm'), legend.title=element_text(size=20), strip.text = element_text(size=20)) + scale_x_continuous(name='\n Scaled Self-Placements', limits=c(-4,4)) + ylab(NULL)

df.2 <- na.omit(full.dat[full.dat$year > 1979, c('self','year')])
df.2$one <- 1
temp <- tapply(rep(1, length(df.2$year)), list(df.2$year, df.2$self), sum)
temp <- temp/rowSums(temp)
temp <- as.data.frame(as.table(temp))
names(temp) <- c('year', 'self', 'pr')

g2 <- ggplot(temp, aes(x=self, y=pr)) + geom_bar(stat='identity',col='white', fill='black', alpha=.7, width=.9) + facet_wrap(~year)+ theme_bw() + labs(x='\n Raw Self-Placement') +theme(legend.position="none", axis.title=element_text(size=20),legend.text=element_text(size=20), strip.background = element_rect(fill='grey90'), axis.text=element_text(size=15), legend.key.size=unit(1.5, 'cm'), legend.title=element_text(size=20), strip.text = element_text(size=20)) + ylab(NULL)
    
setwd(paper.wd)
pdf('cscf.pdf',height=7, width=14, family='CM Roman')
grid.arrange(arrangeGrob(g1, g2,ncol=2,widths=c(1.5,1)))
dev.off()

# all respondents 

g1 <- ggplot(na.omit(df), aes(x=value, linetype=variable)) + geom_density(alpha=.5, color='black', size=.5) + theme_bw() + facet_wrap(~year) + theme(legend.position="none", axis.title=element_text(size=20),legend.text=element_text(size=20), strip.background = element_rect(fill='grey90'), axis.text=element_text(size=15), legend.key.size=unit(1.5, 'cm'), legend.title=element_text(size=20), strip.text = element_text(size=20)) + scale_x_continuous(name='\n Scaled Self-Placements') + ylab(NULL)

df.2 <- na.omit(full.dat[full.dat$year > 1979, c('self','year')])
df.2$one <- 1
temp <- tapply(rep(1, length(df.2$year)), list(df.2$year, df.2$self), sum)
temp <- temp/rowSums(temp)
temp <- as.data.frame(as.table(temp))
names(temp) <- c('year', 'self', 'pr')

g2 <- ggplot(temp, aes(x=self, y=pr)) + geom_bar(stat='identity',col='white', fill='black', alpha=.7, width=.9) + facet_wrap(~year)+ theme_bw() + labs(x='\n Raw Self-Placement') +theme(legend.position="none", axis.title=element_text(size=20),legend.text=element_text(size=20), strip.background = element_rect(fill='grey90'), axis.text=element_text(size=15), legend.key.size=unit(1.5, 'cm'), legend.title=element_text(size=20), strip.text = element_text(size=20)) + ylab(NULL)

pdf('cscf_full.pdf',height=7, width=14, family='CM Roman')
grid.arrange(arrangeGrob(g1, g2,ncol=2,widths=c(1.5,1)))
dev.off()


# sophisticated ?

temp <- cf.data[, c('unique','year','idpt')]
names(temp) <- c('unique','year','cf.idpt')
full.dat <- merge(c.data, temp, by=c('unique','year'), all=T)
temp <- full.dat[full.dat$year>1979 & full.dat$slope.cong>0, c('year','idpt.cong','cf.idpt')]
temp$cf.idpt <- scale(temp$cf.idpt)
temp$idpt.cong <- scale(temp$idpt.cong)
names(temp) <- c('year','NOMINATE','CFscores')
df <- melt(temp, id.vars='year')

g1<- ggplot(na.omit(df), aes(x=value, linetype=variable)) + geom_density(alpha=.5, color='black', size=.5) + theme_bw() + facet_wrap(~year) + theme(legend.position="none", axis.title=element_text(size=20),legend.text=element_text(size=20), strip.background = element_rect(fill='grey90'), axis.text=element_text(size=15), legend.key.size=unit(1.5, 'cm'), legend.title=element_text(size=20), strip.text = element_text(size=20)) + scale_x_continuous(name='\n Scaled Self-Placements', limits=c(-4,4)) + ylab(NULL)

df.2 <- na.omit(full.dat[full.dat$year > 1979 & full.dat$slope.cong>0, c('self','year')])
df.2$one <- 1
temp <- tapply(rep(1, length(df.2$year)), list(df.2$year, df.2$self), sum)
temp <- temp/rowSums(temp)
temp <- as.data.frame(as.table(temp))
names(temp) <- c('year', 'self', 'pr')

g2 <- ggplot(na.omit(temp), aes(x=self, y=pr)) + geom_bar(stat='identity',col='white', fill='black', alpha=.7, width=.9) + facet_wrap(~year)+ theme_bw() + labs(x='\n Raw Self-Placement') +theme(legend.position="none", axis.title=element_text(size=20),legend.text=element_text(size=20), strip.background = element_rect(fill='grey90'), axis.text=element_text(size=15), legend.key.size=unit(1.5, 'cm'), legend.title=element_text(size=20), strip.text = element_text(size=20)) + ylab(NULL)

pdf('cscf_soph.pdf',height=7, width=14, family='CM Roman')
grid.arrange(arrangeGrob(g1, g2,ncol=2,widths=c(1.5,1)))
dev.off()


### voters?

v.dat$unique <- with(v.dat, year*10000 + id)
v.dat <- v.dat[, names(v.dat) %in% c('unique','voted')]


temp <- cf.data[, c('unique','year','idpt')]
names(temp) <- c('unique','year','cf.idpt')
full.dat <- merge(c.data, temp, by=c('unique','year'), all=T)
full.dat <- merge(full.dat, v.dat, by='unique', all.x=T)

temp <- full.dat[full.dat$year>1979 & full.dat$voted==2, c('year','idpt.cong','cf.idpt')]
temp$cf.idpt <- scale(temp$cf.idpt)
temp$idpt.cong <- scale(temp$idpt.cong)
names(temp) <- c('year','NOMINATE','CFscores')
df <- melt(temp, id.vars='year')

g1 <- ggplot(na.omit(df), aes(x=value, linetype=variable)) + geom_density(alpha=.5, color='black', size=.5) + theme_bw() + facet_wrap(~year) + theme(legend.position="none", axis.title=element_text(size=20),legend.text=element_text(size=20), strip.background = element_rect(fill='grey90'), axis.text=element_text(size=15), legend.key.size=unit(1.5, 'cm'), legend.title=element_text(size=20), strip.text = element_text(size=20)) + scale_x_continuous(name='\n Scaled Self-Placements', limits=c(-4,4)) + ylab(NULL)

df.2 <- na.omit(full.dat[full.dat$year > 1979 & full.dat$voted==2, c('self','year')])
df.2$one <- 1
temp <- tapply(rep(1, length(df.2$year)), list(df.2$year, df.2$self), sum)
temp <- temp/rowSums(temp)
temp <- as.data.frame(as.table(temp))
names(temp) <- c('year', 'self', 'pr')

g2 <- ggplot(na.omit(temp), aes(x=self, y=pr)) + geom_bar(stat='identity',col='white', fill='black', alpha=.7, width=.9) + facet_wrap(~year)+ theme_bw() + labs(x='\n Raw Self-Placement') +theme(legend.position="none", axis.title=element_text(size=20),legend.text=element_text(size=20), strip.background = element_rect(fill='grey90'), axis.text=element_text(size=15), legend.key.size=unit(1.5, 'cm'), legend.title=element_text(size=20), strip.text = element_text(size=20)) + ylab(NULL)

pdf('cscf_voted.pdf',height=7, width=14, family='CM Roman')
grid.arrange(arrangeGrob(g1, g2,ncol=2,widths=c(1.5,1)))
dev.off()


# Polarization? SDs

temp <- data.frame(year=years.measured)
temp$sd <- tapply(c.data$idpt.cong, factor(c.data$year), sd, na.rm=T)
temp$sd <- scale(temp$sd)
temp$type <- 'NOMINATE'
temp <- temp[temp$year > 1979,]
temp$type2 <- 'Scaled Self-Placements'

years.measured.cf <- years.measured[-c(1:2)]
n.years.cf <- length(years.measured.cf)
temp.c <- data.frame(year=years.measured.cf)
temp.c$sd <- tapply(cf.data$idpt, factor(cf.data$year), sd, na.rm=T)
temp.c$sd <- scale(temp.c$sd)
temp.c$type <- 'CF'
temp.c$type2 <- 'Scaled Self-Placements'

temp2 <- data.frame(year=years.measured, sd=tapply(c.data$self, factor(c.data$year), sd, na.rm=T))
temp2$sd <- scale(temp2$sd)
temp2$type <- 'Raw'
temp2$type2 <- 'Raw Self-Placements'
temp2 <- temp2[temp2$year > 1979,]

df <- rbind(temp, temp.c, temp2)
rownames(df) <- NULL
df$type <- factor(df$type, levels=c('NOMINATE','CF','Raw'))
df$type2 <- factor(df$type2, level=c('Scaled Self-Placements','Raw Self-Placements'))

pdf('sdsymbol.pdf', width=14, height=8, family='CM Roman')
ggplot(df, aes(x=year, y=sd, group=type)) + geom_point(col='black', alpha=.3,size=4, aes(shape=type)) + geom_line(linetype='dotted', size=.7, col='black', alpha=.2) + geom_smooth(method='loess', se=F, size=1, aes(linetype=type), col='black') + theme_bw() + scale_linetype_manual(values=c(NOMINATE='dashed', CF='dotdash', Raw='solid')) + scale_x_continuous(name='\n Year', breaks=seq(1980,2012,4)) + scale_y_continuous(name='Scaled Standard Deviation \n') + theme(legend.position="bottom", axis.title=element_text(size=20),legend.text=element_text(size=20), strip.background = element_rect(fill='grey90'), axis.text=element_text(size=15), legend.key.size=unit(1.5, 'cm'), legend.title=element_blank(), strip.text = element_text(size=20)) + facet_wrap(~type2)
dev.off()


############# Sorting 

c.data$pid2 <- recode(c.data$pid, "lo:-1='Democrat'; 1:3='Republican'; else=NA")
cf.data$pid2 <- recode(cf.data$pid, "lo:-1='Democrat'; 1:3='Republican'; else=NA")

temp.dat <- data.frame(year=years.measured[-c(1:2)])
temp.vec <- matrix(NA, nrow=length(years.measured[-c(1,2)]), ncol=7)

temp.data <- c.data[!is.na(c.data$idpt.cong) & !is.na(c.data$pid2),]
iter <- 0
for (y in years.measured[-c(1,2)]) {
    iter <- iter + 1
    temp.vec[iter,1] <- y
    temp <- data.frame(idpt=rescale(temp.data$idpt.cong[temp.data$year==y],to=c(0,2*pi)),self=rescale(temp.data$self[temp.data$year==y],to=c(0,2*pi)),pid2=temp.data$pid2[temp.data$year==y])
    temp.vec[iter,2:4] <- overlapEst(temp$idpt[temp$pid2=='Democrat'], temp$idpt[temp$pid2=='Republican'])
    temp.vec[iter,5:7] <- overlapEst(temp$self[temp$pid2=='Democrat'], temp$self[temp$pid2=='Republican'])
}
colnames(temp.vec) <- c('year','d1.sc','d4.sc','d5.sc','d1.un','d4.un','d5.un')
overlap.dat <- data.frame(temp.vec[,c('year','d5.sc','d5.un')])

# bootstrapped s.e.

temp <- data.frame(idpt=rescale(temp.data$idpt.cong[temp.data$year==1980],to=c(0,2*pi)),pid2=temp.data$pid2[temp.data$year==1980])

ovp <- overlapEst(temp$idpt[temp$pid2=='Democrat'], temp$idpt[temp$pid2=='Republican'])[3]

registerDoParallel(cores=3)
boot.res <- foreach (ii=1:200, .combine='c', .packages='overlap') %dopar% {
    temp2 <- temp[sample(1:nrow(temp), nrow(temp), replace=T),]
    ob <- overlapEst(temp2$idpt[temp2$pid2=='Democrat'], temp2$idpt[temp2$pid2=='Republican'])[3]
    return(ob)
}
hist(boot.res)
abline(v=ovp, col='red')
print(quantile(boot.res, c(.05, .95)))

temp <- data.frame(idpt=rescale(temp.data$idpt.cong[temp.data$year==2012],to=c(0,2*pi)),pid2=temp.data$pid2[temp.data$year==2012])
ovp <- overlapEst(temp$idpt[temp$pid2=='Democrat'], temp$idpt[temp$pid2=='Republican'])[3]
boot.res <- foreach (ii=1:200, .combine='c', .packages='overlap') %dopar% {
    temp2 <- temp[sample(1:nrow(temp), nrow(temp), replace=T),]
    ob <- overlapEst(temp2$idpt[temp2$pid2=='Democrat'], temp2$idpt[temp2$pid2=='Republican'])[3]
    return(ob)
}
hist(boot.res)
abline(v=ovp, col='red')
print(quantile(boot.res, c(.05, .95)))


###############CF

temp.dat <- data.frame(year=years.measured[-c(1,2)])
temp.vec <- matrix(NA, nrow=length(years.measured[-c(1,2)]), ncol=4)

temp.data <- cf.data[!is.na(cf.data$idpt) & !is.na(cf.data$pid2),]
iter <- 0
for (y in years.measured[-c(1,2)]) {
    iter <- iter + 1
    temp.vec[iter,1] <- y
    temp <- data.frame(idpt=rescale(temp.data$idpt[temp.data$year==y],to=c(0,2*pi)),self=rescale(temp.data$self[temp.data$year==y],to=c(0,2*pi)),pid2=temp.data$pid2[temp.data$year==y])
    temp.vec[iter,2:4] <- overlapEst(temp$idpt[temp$pid2=='Democrat'], temp$idpt[temp$pid2=='Republican'])
}
colnames(temp.vec) <- c('year','d1.cf','d4.cf','d5.cf')
temp.vec <- data.frame(temp.vec)


## merge 
overlap.dat <- merge(overlap.dat, temp.vec[, c(1,4)], by='year', all.x=T)
summary(lm(d5.sc ~ I(year/10), data=overlap.dat))

names(overlap.dat) <- c('year', 'NOMINATE', 'Raw', 'CF')
o.df <- melt(overlap.dat, id.vars='year')

pdf('overlap.pdf',width=10,height=8, family='CM Roman')
o.df$variable <- factor(o.df$variable, levels=c('NOMINATE','CF','Raw'))
ggplot(o.df, aes(x=year,y=value, group=variable, shape=variable, col=variable)) + theme_bw() + scale_x_continuous(name='\n Year', breaks=seq(1980,2012,4)) + geom_point(size=4) + geom_line(linetype='dotted', size=.7)+ ylab('Overlap \n') + theme(legend.position="bottom", legend.text=element_text(size=20),legend.title=element_blank(), axis.title=element_text(size=20), axis.text=element_text(size=15)) + scale_color_grey(start=0, end=.7)
dev.off()



######### Sophisticated #####

df <- c.data[, c('pid2', 'idpt.cong', 'year')]
df <- na.omit(df)

years.measured <- sort(unique(c.data$year))[-c(1,2)]
temp.dat <- data.frame(year=years.measured)
temp.vec <- matrix(NA, nrow=length(years.measured), ncol=7)

temp.data <- c.data[!is.na(c.data$idpt.cong) & !is.na(c.data$pid2) & c.data$slope.cong >0,]
iter <- 0
for (y in years.measured) {
    iter <- iter + 1
    temp.vec[iter,1] <- y
    temp <- data.frame(idpt=rescale(temp.data$idpt.cong[temp.data$year==y],to=c(0,2*pi)),self=rescale(temp.data$self[temp.data$year==y],to=c(0,2*pi)), pid2=temp.data$pid2[temp.data$year==y])
    temp <- temp[!is.na(temp$pid2),]
    temp.vec[iter,2:4] <- overlapEst(temp$idpt[temp$pid2=='Democrat'], temp$idpt[temp$pid2=='Republican'])
    temp.vec[iter,5:7] <- overlapEst(temp$self[temp$pid2=='Democrat'], temp$self[temp$pid2=='Republican'])
}
colnames(temp.vec) <- c('year','d1.sc','d4.sc','d5.sc','d1.un','d4.un','d5.un')
overlap.dat.2 <- data.frame(temp.vec[,c('year','d5.sc','d5.un')])

###############CF

temp.dat <- data.frame(year=years.measured)
temp.vec <- matrix(NA, nrow=length(years.measured), ncol=4)

temp.data <- cf.data[!is.na(cf.data$idpt) & !is.na(cf.data$pid2) & cf.data$slope>0,]
iter <- 0
for (y in years.measured) {
    iter <- iter + 1
    temp.vec[iter,1] <- y
    temp <- data.frame(idpt=rescale(temp.data$idpt[temp.data$year==y],to=c(0,2*pi)),self=rescale(temp.data$self[temp.data$year==y],to=c(0,2*pi)),pid2=temp.data$pid2[temp.data$year==y])
    temp <- temp[!is.na(temp$pid2),]
    temp.vec[iter,2:4] <- overlapEst(temp$idpt[temp$pid2=='Democrat'], temp$idpt[temp$pid2=='Republican'])
}
colnames(temp.vec) <- c('year','d1.cf','d4.cf','d5.cf')
temp.vec <- data.frame(temp.vec[,c('year','d5.cf')])
overlap.dat.2 <- merge(overlap.dat.2, temp.vec, by='year', all.x=T)
names(overlap.dat.2) <- c('year','NOMINATE','Raw','CF')

o.df.2 <- melt(overlap.dat.2, id.vars='year')
o.df.2$variable <- factor(o.df.2$variable, levels=c('NOMINATE','CF','Raw'))

pdf('overlap_soph.pdf',width=10,height=7, family='CM Roman')
ggplot(o.df.2, aes(x=year,y=value, group=variable, shape=variable, col=variable)) + theme_bw() + scale_x_continuous(name='\n Year', breaks=seq(1980,2012,4)) + geom_point(size=4) + geom_line(linetype='dotted', size=.7)+ ylab('Overlap \n') + theme(legend.position="bottom", legend.text=element_text(size=20),legend.title=element_blank(), axis.title=element_text(size=20), axis.text=element_text(size=15)) + scale_color_grey(start=0, end=.7)
dev.off()



### VOTERS ####

v.data <- merge(c.data, v.dat, by='unique')

years.measured <- sort(unique(v.data$year))[-c(1,2)]
temp.dat <- data.frame(year=years.measured)
temp.vec <- matrix(NA, nrow=length(years.measured), ncol=7)

temp.data <- v.data[!is.na(v.data$idpt.cong) & !is.na(v.data$pid2) & v.data$voted==2,]
iter <- 0
for (y in years.measured) {
    iter <- iter + 1
    temp.vec[iter,1] <- y
    temp <- data.frame(idpt=rescale(temp.data$idpt.cong[temp.data$year==y],to=c(0,2*pi)),self=rescale(temp.data$self[temp.data$year==y],to=c(0,2*pi)), pid2=temp.data$pid2[temp.data$year==y])
    temp <- temp[!is.na(temp$pid2),]
    temp.vec[iter,2:4] <- overlapEst(temp$idpt[temp$pid2=='Democrat'], temp$idpt[temp$pid2=='Republican'])
    temp.vec[iter,5:7] <- overlapEst(temp$self[temp$pid2=='Democrat'], temp$self[temp$pid2=='Republican'])
}
colnames(temp.vec) <- c('year','d1.sc','d4.sc','d5.sc','d1.un','d4.un','d5.un')
overlap.dat.v <- data.frame(temp.vec[,c('year','d5.sc','d5.un')])


temp.dat <- data.frame(year=years.measured)
temp.vec <- matrix(NA, nrow=length(years.measured), ncol=4)

cf.v.data <- merge(cf.data, v.dat, by='unique')
temp.data <- cf.v.data[!is.na(cf.v.data$idpt) & !is.na(cf.v.data$pid2) & cf.v.data$voted==2,]
iter <- 0
for (y in years.measured) {
    iter <- iter + 1
    temp.vec[iter,1] <- y
    temp <- data.frame(idpt=rescale(temp.data$idpt[temp.data$year==y],to=c(0,2*pi)),self=rescale(temp.data$self[temp.data$year==y],to=c(0,2*pi)),pid2=temp.data$pid2[temp.data$year==y])
    temp <- temp[!is.na(temp$pid2),]
    temp.vec[iter,2:4] <- overlapEst(temp$idpt[temp$pid2=='Democrat'], temp$idpt[temp$pid2=='Republican'])
}
colnames(temp.vec) <- c('year','d1.cf','d4.cf','d5.cf')
temp.vec <- data.frame(temp.vec[,c('year','d5.cf')])
overlap.dat.v <- merge(overlap.dat.v, temp.vec, by='year', all.x=T)
names(overlap.dat.v) <- c('year','NOMINATE','Raw','CF')

o.df.v <- melt(overlap.dat.v, id.vars='year')
o.df.v$variable <- factor(o.df.v$variable, levels=c('NOMINATE','CF','Raw'))


pdf('overlap_voted.pdf',width=10,height=7, family='CM Roman')
ggplot(o.df.v, aes(x=year,y=value, group=variable, shape=variable, col=variable)) + theme_bw() + scale_x_continuous(name='\n Year', breaks=seq(1980,2012,4)) + geom_point(size=4) + geom_line(linetype='dotted', size=.7)+ ylab('Overlap \n') + theme(legend.position="bottom", legend.text=element_text(size=20),legend.title=element_blank(), axis.title=element_text(size=20), axis.text=element_text(size=15)) + scale_color_grey(start=0, end=.7)
dev.off()


### END OF CODE ###