
################################################
################################################
####                                        ####
####            HOW ARE WE APART            ####
####                                        ####
####    GRM Models: Analysis of Results     ####
####                                        ####
####                                        ####
#### Last Update: 08/09/2016                ####
####                                        ####
################################################
################################################

# remove all objects
rm(list=ls())


# load packages
library(coda)
library(rstan)
library(ggplot2)
library(ltm)
library(reshape)
library(car)
library(grid)
library(gridExtra)
library(xtable)

# set working directory
wd <- "Working Directory Here!"
if (!isTRUE(getwd()==wd)) setwd(wd)
paper.wd <- "Path to directory where figures should be placed"


i.vars.summ <- readRDS('summary.Rda')
full.dat <- readRDS('full.dat.Rda')
cut.dat <- readRDS('cut.dat.Rda')
grm.dat <- readRDS('grm.dat.Rda')
grm.year.dat <- readRDS('grm.year.dat.Rda')


#change working directory to MCMC results
wd.3.dim <- paste0(wd,'/MCMC/dim3_corr_year_sd_mean')
setwd(wd.3.dim)

# extract MCMC simulations
gamma.mcmc <- readRDS('gamma.Rda')
kappa.mcmc <- readRDS('kappa.Rda')
theta.mcmc <- readRDS('theta.Rda')
mu.mcmc <- readRDS('mu.Rda')
tau.mcmc <- readRDS('tau.Rda')

### generate dataset of loadings ###
gamma.dat <- data.frame(apply(gamma.mcmc[[1]],3,colMeans))
names(gamma.dat) <- c('gamma1','gamma2','gamma3')
gamma.dat$label <- i.vars.summ[,5]
gamma.dat$i.vars.no <- as.numeric(i.vars.summ[,1])

# function for generatting highest posterior density intervals
gen.HPD <- function(x) {
    require(coda)
    d <- length(dim(x[[1]]))
    if (d>2) {
        m.list <- list()
        for (dd in 1:dim(x[[1]])[3]) {
            chain.name <- paste0('y',dd)
            assign(chain.name, as.mcmc(x[[1]][,,dd]))
            m.list[[dd]] <- get(chain.name)
        }
        m.list <- as.mcmc.list(m.list)
    } else m.list <- as.mcmc(x[[1]])
    
    return(HPDinterval(m.list))
}

g.hpd <- data.frame(do.call('cbind',gen.HPD(gamma.mcmc)))
names(g.hpd) <- paste0(rep(c('lo','hi'),3), rep(1:3, each=2))

# merge to dataset
gamma.dat <- cbind(gamma.dat, g.hpd)
rownames(gamma.dat) <- i.vars.summ[,5]


g1 <- gamma.dat[gamma.dat$i.vars.no %in% as.numeric(i.vars.summ[i.vars.summ[,3]==1,1]),c('gamma1','lo1','hi1')]
g2 <- gamma.dat[gamma.dat$i.vars.no %in% as.numeric(i.vars.summ[i.vars.summ[,3]==2,1]),c('gamma2','lo2','hi2')]
g3 <- gamma.dat[gamma.dat$i.vars.no %in% as.numeric(i.vars.summ[i.vars.summ[,3]==3,1]),c('gamma3','lo3','hi3')]
names(g1) <- names(g2) <- names(g3) <- c('gamma','hi','lo')

gamma.print <- rbind(g1,g2,g3)
gamma.print$label <- rownames(gamma.print)
temp <- i.vars.summ[, c(5, 3)]
colnames(temp) <- c('label','domain')
gamma.print <- merge(gamma.print, temp, by='label')
gamma.print <- gamma.print[order(gamma.print$domain, -gamma.print$gamma),]
gamma.print[,2:4] <- apply(gamma.print[,2:4],2, function(w)formatC(w, digits=3, format='f'))

## kappa ##

kappa.dat <- data.frame(colMeans(kappa.mcmc[[1]]))
names(kappa.dat) <- 'kappa'
kappa.dat$label <- rep(i.vars.summ[,5],cut.dat$cuts)
kappa.dat$i.vars.no <- as.numeric(rep(i.vars.summ[,1],cut.dat$cuts))

k.list <- vector('list',nrow(i.vars.summ))
cc <- 0
for (v in i.vars.summ[,1]) {
    cc <- cc + 1
    v <- as.numeric(v)
    k.list[[cc]] <- kappa.dat[kappa.dat$i.vars.no==v,1]
    ll <- length(k.list[[cc]])
    k.list[[cc]] <- c(formatC(k.list[[cc]], digits=3, format='f'), rep("---", 6-ll))
}
names(k.list) <- i.vars.summ[,5]    
kappa.print <- do.call('rbind', k.list)
colnames(kappa.print) <- paste0('Cut',1:6)
kappa.print <- cbind(kappa.print,rownames(kappa.print))
colnames(kappa.print)[7] <- 'label'

disc.cut.print <- merge(gamma.print, kappa.print, by='label')
disc.cut.print <- disc.cut.print[,c(5,1,2,3,4,6:ncol(disc.cut.print))]
disc.cut.print <- disc.cut.print[order(disc.cut.print$domain, -as.numeric(disc.cut.print$gamma)),]
disc.cut.print$domain <- sapply(disc.cut.print$domain, function(w) switch(as.numeric(w), 'Economic','Civil Rights', 'Moral'))

x.disc.cut <- xtable(disc.cut.print)
print(x.disc.cut, tabular.environment='longtable', include.rownames=F)



### SD ####

setwd(paper.wd)

draw <- sample(1:nrow(theta.mcmc$theta), 50, replace=F)
temp.year <- full.dat[full.dat$year!=2002 & full.dat$year >= 1986,'year']
rand.1 <- cbind(temp.year,t(theta.mcmc$theta[draw, ,1]))
r1 <- sapply(2:ncol(rand.1), function(x){tapply(rand.1[,x], rand.1[,1], sd)})
rand.2 <- cbind(temp.year,t(theta.mcmc$theta[draw, ,2]))
r2 <- sapply(2:ncol(rand.2), function(x){tapply(rand.2[,x], rand.2[,1], sd)})
rand.3 <- cbind(temp.year,t(theta.mcmc$theta[draw, ,3]))
r3 <- sapply(2:ncol(rand.3), function(x){tapply(rand.3[,x], rand.3[,1], sd)})

sd.dat <- data.frame(year=as.numeric(rep(rownames(r1),3)), rbind(r1, r2,r3), domain=rep(1:3,each=nrow(r1)))
sd.dat$domain <- factor(sd.dat$domain, levels=1:3, labels=c('Economic','Civil Rights', 'Moral'))

df <- melt(sd.dat, id.vars=c('year','domain'))

pdf('sd.pdf', family='CM Roman', width=14, height=8)
ggplot(df) + geom_jitter(aes(x=year, y=value, group=variable), size=2, alpha=.3, col='black',shape=16) + theme_bw() + facet_wrap(~domain) + labs(y='Standard Deviation \n', x='\n Year') + scale_x_continuous(breaks=sort(unique(df$year))) + geom_line(aes(x=year, y=value, group=variable), stat='smooth', method='loess', span=1.2, size=1, se=F, linetype=1, col='grey', alpha=.3) + theme(axis.text.x = element_text(angle = 45, hjust=1, size=15), axis.title=element_text(size=20),strip.background = element_rect(fill='grey90'), strip.text = element_text(size=20))
dev.off()


### Distributions ####

theta.dat <- full.dat[full.dat$year!=2002 & full.dat$year >= 1986,]
temp <- apply(theta.mcmc[[1]], 3, colMeans)
colnames(temp) <- c('Economic','Civil Rights','Moral')
theta.dat <- cbind(theta.dat, temp)

p.theta <- theta.dat[theta.dat$year %in% c(1988,2000,2012),c('year','Economic','Civil Rights','Moral')]
df <- melt(p.theta, id.vars='year')

pdf('idpt8812.pdf', family='CM Roman', width=14, height=8)
ggplot(df, aes(x=value, linetype=factor(year))) + stat_density(geom='line', position='identity') + scale_linetype_manual(name='Year', values=c('dotted','longdash','solid'))+theme_bw() +xlim(-5,5)  + xlab(bquote(atop(phantom(),theta))) + ylab('Density \n') +theme(legend.position='bottom',axis.title=element_text(size=20),legend.text=element_text(size=20), strip.background = element_rect(fill='grey90'), axis.text=element_text(size=15), legend.key.size=unit(1.5, 'cm'), legend.title=element_text(size=20), strip.text = element_text(size=20)) + facet_wrap(~variable, scale='free')
dev.off()

## All years ##

pdf('idpt_all_years.pdf', family='CM Roman', width=14, height=8)
a.theta <- theta.dat[,c('year','Economic','Civil Rights','Moral')]
df <- melt(a.theta, id.vars='year')
ggplot(df, aes(x=value)) + geom_density() + facet_grid(year~variable) + theme_bw()  +theme(axis.title=element_text(size=20),legend.text=element_text(size=20), strip.background = element_rect(fill='grey90'), axis.text=element_text(size=15), legend.key.size=unit(1.5, 'cm'), legend.title=element_text(size=20), strip.text = element_text(size=20))+ ylab('Density \n')+ xlab(bquote(atop(phantom(),theta)))
dev.off()


### Voters ###
setwd(wd)
setwd(paste0(wd,'/raw_data'))
nes <- read.table('anes_timeseries_cdf_rawdata.txt', header=T, sep='|')
v.dat <- nes[, c('VCF0702', 'VCF0004', 'VCF0006')]
names(v.dat) <- c('voted','year','id')
setwd(wd)
saveRDS(v.dat, 'v.dat.Rda')

# codes: 0=missing, 1=did not vote, 2=voted

setwd(wd)
v.dat <- readRDS('v.dat.Rda')
theta.v.dat <- merge(theta.dat, v.dat, by=c('id','year'))
v.theta <- theta.v.dat[theta.v.dat$voted==2,c('year','Economic','Civil Rights','Moral')]
df <- melt(v.theta, id.vars='year')
setwd(paper.wd)
pdf('idpt_voters.pdf', family='CM Roman', width=14, height=8)
ggplot(na.omit(df), aes(x=value)) + geom_density() + facet_grid(year~variable) + theme_bw()  +theme(axis.title=element_text(size=20),legend.text=element_text(size=20), strip.background = element_rect(fill='grey90'), axis.text=element_text(size=15), legend.key.size=unit(1.5, 'cm'), legend.title=element_text(size=20), strip.text = element_text(size=20))+ ylab('Density \n')+ xlab(bquote(atop(phantom(),theta)))
dev.off()


#### Sorting ####

library(overlap)
library(car)
library(scales)

library(doParallel)
registerDoParallel(cores=3)

n.draws <- 50
rand.draw.1 <- sample(1:nrow(theta.mcmc$theta[,,1]), n.draws, replace=F)
pid.year <- full.dat[full.dat$year > 1984 & full.dat$year != 2002, c('year','pid')]
pid.year$pid2<- recode(pid.year$pid, "c(1,2,3)='Democrat'; 4=NA; c(5,6,7)='Republican'")
years.measured <- sort(unique(pid.year$year))


set.seed(54123)
overl <- function(d=tt, col=x) {overlapEst(d[d$pid2=='Democrat',col], d[d$pid2=='Republican',col])}
gen.sort.dat <- function(v) {
    require(doParallel)
    sort.1 <- cbind(pid.year,t(theta.mcmc$theta[rand.draw.1,,v]))
    sort.1 <- sort.1[!is.na(sort.1$pid2),]
    sort.1[, 4:ncol(sort.1)] <- apply(sort.1[,4:ncol(sort.1)], 2, rescale, to=c(0,2*pi))

    o.mat.1 <- foreach (i=1:length(years.measured), .combine='cbind', .packages='overlap', .export=c('years.measured', 'overl')) %dopar% {
        y <- years.measured[i]
        tt <- sort.1[sort.1$year==y,]
        temp <- sapply(4:ncol(tt), function(w) overl(d=tt, col=w))
        o.coef <- matrix(temp[3,])
        colnames(o.coef) <- y
        o.coef
    }
    return(o.mat.1)
}

sort.res <- lapply(1:3, gen.sort.dat)
saveRDS(sort.res,'sort.res.Rda')
sort.res <- readRDS('sort.res.Rda')

df <- lapply(sort.res, melt)
df <- do.call('rbind',df)
df$domain <- rep(1:3, each=n.draws*length(years.measured))
df$domain <- factor(df$domain, levels=1:3, labels=c('Economic', 'Civil Rights', 'Moral'))
names(df) <- c('draw','year','overlap', 'domain')

setwd(paper.wd)

pdf('ov.pdf', family='CM Roman', width=14, height=7)
ggplot(df) + geom_jitter(aes(x=year, y=overlap, group=draw),col='black', size=2, alpha=.3, shape=16) + theme_bw() + labs(y='Overlap \n', x='\n Year') + scale_x_continuous(breaks=sort(unique(df$year))) + scale_y_continuous(breaks=seq(.4,1,.1), limits=c(.4,1))+ geom_line(stat='smooth',aes(x=year, y=overlap, group=draw), size=1, alpha=.3, method='loess', span=1, linetype=1, col='grey') + facet_wrap(~domain) + theme(axis.text.x = element_text(angle = 45, hjust=1, size=15), axis.text.y = element_text(size=15),axis.title=element_text(size=20),strip.background = element_rect(fill='grey90'), strip.text = element_text(size=20))
dev.off()


### using subset of respondents included in analysis of symbolic ideology
setwd(wd)

library(doParallel)
registerDoParallel(cores=3)

dim.2.dat.full <- full.dat[full.dat$year!=2002 & full.dat$year >= 1986,]
sym.res <- readRDS('symbol.res.Rda')
temp <- cbind(1:nrow(dim.2.dat.full), dim.2.dat.full)
temp <- temp[temp$unique %in% sym.res,]
joint.samp <- temp[,1]

n.draws <- 100
rand.draw.1 <- sample(1:nrow(theta.mcmc$theta[,,1]), n.draws, replace=F)

pid.year <- full.dat[full.dat$year > 1984 & full.dat$year != 2002, c('year','pid')]
pid.year$pid2<- recode(pid.year$pid, "c(1,2,3)='Democrat'; 4=NA; c(5,6,7)='Republican'")
pid.year <- pid.year[joint.samp,]
years.measured.joint <- sort(unique(pid.year$year))


set.seed(54123)
overl <- function(d=tt, col=x) {overlapEst(d[d$pid2=='Democrat',col], d[d$pid2=='Republican',col])}
gen.sort.dat <- function(v) {
    require(doParallel)
    sort.1 <- cbind(pid.year,t(theta.mcmc$theta[rand.draw.1,joint.samp,v]))
    sort.1 <- sort.1[!is.na(sort.1$pid2),]
    sort.1[, 4:ncol(sort.1)] <- apply(sort.1[,4:ncol(sort.1)], 2, rescale, to=c(0,2*pi))

    o.mat.1 <- foreach (i=1:length(years.measured.joint), .combine='cbind', .packages='overlap', .export=c('years.measured.joint', 'overl')) %dopar% {
        y <- years.measured.joint[i]
        tt <- sort.1[sort.1$year==y,]
        temp <- sapply(4:ncol(tt), function(w) overl(d=tt, col=w))
        o.coef <- matrix(temp[3,])
        colnames(o.coef) <- y
        o.coef
    }
    return(o.mat.1)
}

sort.res.joint <- lapply(1:3, gen.sort.dat)

df <- lapply(sort.res.joint, melt)
df <- do.call('rbind',df)
df$domain <- rep(1:3, each=n.draws*length(years.measured.joint))
df$domain <- factor(df$domain, levels=1:3, labels=c('Economic', 'Civil Rights', 'Moral'))
names(df) <- c('draw','year','overlap', 'domain')

setwd(paper.wd)
pdf('ov_sym.pdf', family='CM Roman', width=14, height=7)
ggplot(df) + geom_jitter(aes(x=year, y=overlap, group=draw),col='black', size=2, alpha=.3, shape=16) + theme_bw() + labs(y='Overlap \n', x='\n Year') + scale_x_continuous(breaks=sort(unique(df$year))) + scale_y_continuous(breaks=seq(.3,1,.1), limits=c(.3,1))+ geom_line(stat='smooth',aes(x=year, y=overlap, group=draw), size=1, alpha=.3, method='loess', span=1, linetype=1, col='grey') + facet_wrap(~domain) + theme(axis.text.x = element_text(angle = 45, hjust=1, size=15), axis.text.y = element_text(size=15),axis.title=element_text(size=20),strip.background = element_rect(fill='grey90'), strip.text = element_text(size=20))
dev.off()

sum.joint <- lapply(sort.res.joint, function(x) {
    a <- colMeans(x)
    b <- apply(x,2,quantile, probs=c(.05,.95))
    res <- rbind(a,b)
    res <- res[c(2,1,3),]
    rownames(res)[2] <- 'mean'
    return(res)
})
print(sum.joint)


## Dimensional Alignment ##

setwd(wd)
draw <- sample(1:dim(theta.mcmc[[1]])[1], 50, replace=F)
year <- theta.dat$year
years.measured <- sort(unique(theta.dat$year))

e <- cbind(year,t(theta.mcmc[[1]][draw,,1]))
cr <- cbind(year, t(theta.mcmc[[1]][draw,,2]))
m <- cbind(year, t(theta.mcmc[[1]][draw,,3]))

year.cor <- function(w, dim1,dim2) {
    tt <- dim1[dim1[,1]==w,2:ncol(dim1)]
    tt2 <- dim2[dim2[,1]==w,2:ncol(dim2)]
    ttt <- sapply(1:ncol(tt), function(cc) cor(tt[,cc], tt2[,cc]))
    return(ttt)
}

e.cr <- sapply(years.measured, function(w) year.cor(w, e,cr))
colMeans(e.cr)
e.m <- sapply(years.measured, function(w) year.cor(w, e,m))
cr.m <- sapply(years.measured, function(w) year.cor(w, cr,m))

colnames(e.m) <- colnames(cr.m) <- colnames(e.cr) <- years.measured
df.e.cr <- melt(e.cr)
df.e.m <- melt(e.m)
df.cr.m <- melt(cr.m)


names(df.e.cr) <- names(df.e.m) <- names(df.cr.m) <- c('draw','year','rho')

df.e.cr$domain <- 'Economic, Civil Rights'
df.e.m$domain <- 'Economic, Moral'
df.cr.m$domain <- 'Civil Rights, Moral'

df <- rbind(df.e.cr, df.e.m, df.cr.m)
df$domain <- factor(df$domain, levels=c('Economic, Civil Rights', 'Economic, Moral', 'Civil Rights, Moral'))

setwd(paper.wd)
pdf('da.pdf', width=14, height=8, family='CM Roman')
ggplot(df, aes(x=year, y=rho, group=draw)) + geom_jitter(col='black',size=2, alpha=.3, shape=16) + theme_bw() + facet_wrap(~domain) + geom_line(stat='smooth', method='loess', span=1, col='grey', alpha=.3) + theme_bw() + labs(y='Correlation \n', x='\n Year') + scale_x_continuous(breaks=sort(unique(df$year))) + theme(axis.text.x = element_text(angle = 45, hjust=1, size=15), axis.title=element_text(size=20),strip.background = element_rect(fill='grey90'), strip.text = element_text(size=20))
dev.off()


### Strong Partisans ###

draw <- sample(1:dim(theta.mcmc[[1]])[1], 50, replace=F)
year <- theta.dat$year
s.pid <- ifelse(theta.dat$pid==1 | theta.dat$pid==7,1,0)

e <- cbind(year,t(theta.mcmc[[1]][draw,,1]))
cr <- cbind(year,t(theta.mcmc[[1]][draw,,2]))
m <- cbind(year,t(theta.mcmc[[1]][draw,,3]))

e <- e[s.pid==1,]
cr <- cr[s.pid==1,]
m <- m[s.pid==1,]

year.cor.spid <- function(w, dim1,dim2) {
    tt <- dim1[dim1[,1]==w,2:ncol(dim1)]
    tt2 <- dim2[dim2[,1]==w,2:ncol(dim2)]
    ttt <- sapply(1:ncol(tt), function(cc) cor(tt[,cc], tt2[,cc], use='pairwise'))
    return(ttt)
}

e.cr <- sapply(years.measured, function(w) year.cor.spid(w, e,cr))
colMeans(e.cr, na.rm=T)
e.m <- sapply(years.measured, function(w) year.cor.spid(w, e,m))
cr.m <- sapply(years.measured, function(w) year.cor.spid(w, cr,m))


colnames(e.m) <- colnames(cr.m) <- colnames(e.cr) <- years.measured
df.e.cr <- melt(e.cr)
df.e.m <- melt(e.m)
df.cr.m <- melt(cr.m)


names(df.e.cr) <- names(df.e.m) <- names(df.cr.m) <- c('draw','year','rho')

df.e.cr$domain <- 'Economic, Civil Rights'
df.e.m$domain <- 'Economic, Moral'
df.cr.m$domain <- 'Civil Rights, Moral'

df <- rbind(df.e.cr, df.e.m, df.cr.m)
df$domain <- factor(df$domain, levels=c('Economic, Civil Rights', 'Economic, Moral', 'Civil Rights, Moral'))

setwd(paper.wd)
pdf('da_s_partisans.pdf', width=14, height=8, family='CM Roman')
ggplot(df, aes(x=year, y=rho, group=draw)) + geom_jitter(col='black',size=2, alpha=.3, shape=16) + theme_bw() + facet_wrap(~domain) + geom_line(stat='smooth', method='loess', span=1, col='grey', alpha=.3) + theme_bw() + labs(y='Correlation \n', x='\n Year') + scale_x_continuous(breaks=sort(unique(df$year))) + theme(axis.text.x = element_text(angle = 45, hjust=1, size=15), axis.title=element_text(size=20),strip.background = element_rect(fill='grey90'), strip.text = element_text(size=20))
dev.off()
embed_fonts('da_s_partisans.pdf')



##### END OF CODE #####
