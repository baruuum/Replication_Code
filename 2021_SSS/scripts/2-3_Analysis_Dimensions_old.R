
## 
## Analysis of Dimensions
##


## Set up ------------------------------------------------------------------------

# load packages
library(MASS)
library(dplyr)
library(data.table)
library(dtplyr)
library(viridis)
library(scales)
library(rstan)
library(xtable)

# setting paths
if (!exists('wd.base')) {
   wd.base <- 'C:/Users/struxture/Dropbox/###_work_current/#_weak_ties/analysis/'
   code.path <- paste0(wd.base,'##_code/')
   mcode.path <- paste0(code.path,'Model_Codes/')
   rawdata.path <- paste0(wd.base,'##_rawdata/')
   data.path <- paste0(wd.base,'data/')
   res.path <- paste0(wd.base,'results/')
   output.path <- paste0(wd.base,'output/')
}

# load functions
source(paste0(code.path,'0_functions.R'))

# what model?
tie.type <- 'acq.core.s'
model.no <- 'NB4'

temp.path <- 'C:/Users/struxture/Dropbox/###_work_current/#_weak_ties/drafts/'

# set seed
set.seed(123)

## Loading Data --------------------------------------------------------------

## Load Data

# load fitted object
# fit <- readRDS(paste0(res.path,
#                       tie.type,'/',
#                       model.no,'/',
#                       paste0(tie.type,'.',
#                              model.no,
#                              '.rds'
#                       )
#    )
# )

fit <- readRDS('D:/wd/r/weakties/results/acq/CONV.NB4/acq.core.s.NB4.rds')

# load predictors & allocate new ids
ids <- fit$data$ind.sum
dat <- merge(ids, 
             readRDS(paste0(data.path,'predictors.rds')), 
             by.x='old.id', 
             by.y='id') %>%
   setorder(new.id)



# generate labels of groupsppc.rm 

group.labs <- copy(fit$data$item.sum)
group.labs[,short.labs:=gsub('acq','',labs)]
group.labs[,short.labs:=paste0(toupper(substr(short.labs,1,1)),
                substr(short.labs,2,nchar(short.labs)))]

long.labs <- matrix(c('Unemployed', 'Own second home', 'In state or federal prison', 'Asian or Asian-American', 'Black or African-American', 'Hispanic', 'White', 'Gay men or women', 'Cohabiting women', 'Attend religious service regularly', 'Attend religious service rarely/never', 'Strongly liberal', 'Strongly conservative', 'acqunemp', 'acqhome', 'acqprisn', 'acqasian', 'acqblack', 'acqhisp', 'acqwhite', 'acqgay', 'acqcohab', 'acqgoatt', 'acqnoatt', 'acqlib', 'acqcon'), ncol=2)
colnames(long.labs) <- c('long.labs','labs')

group.labs <- merge(group.labs, long.labs, by='labs')
setorder(group.labs, item.no)


## PPC -----------------------------------------------------------------------

ppc <- gen.ppc.df(fit, mu='lambda', logged=T, quants=c(.025,.975),
                  n.samps=4000)
ppc.rm <- readRDS(paste0(res.path,'acq.core.s/L.RM.ppc.df.95.rds'))
ppc.df <-rbind(ppc$post.df[,model:='Negative Binomial'],
               ppc.rm$post.df[,model:='Random Mixing'])
ppc.df[,`:=`(prop=NULL,sds=NULL,item=NULL,labs=NULL)]
ppc.dat <- copy(ppc$dat.df)
ppc.df <- merge(ppc.df, ppc.dat[,list(probs,fact.value,new.labs)],
                by=c('fact.value','new.labs'))

# ppc.dat[,`:=`(item=NULL,
#               V1=NA,
#               V2=NA,
#               N=NULL,
#               model='Observed')] %>%
#    setnames('response','value') %>% 
#    setcolorder(c('value','probs','V1','V2','fact.value','new.labs','model'))
flims <- ppc.df[,list(
   min(c(probs,V1)),
   max(c(probs,V2))
   ), 
   by=list(fact.value)] %>%
   melt(id.vars='fact.value')
flims[,`:=`(variable=NULL,value2=value)]
setnames(flims, c('fact.value','probs','V1'))
ggplot(ppc.df, aes(x=probs, col=model)) +
   geom_errorbar(aes(ymin=V1, ymax=V2)) +
   geom_abline(intercept=0, slope=1)+
   coord_fixed(ratio=1)+
   coord_flip()+
   theme_bw() + 
   facet_wrap(~fact.value, scale='free') +
   geom_blank(data=flims, inherit.aes=F,
              aes(x=probs,y=V1)) +
   scale_color_manual('', 
                      values=c('Negative Binomial'='black',
                      'Random Mixing'='grey')) +
   labs(x='Data', y='95% Posterior Intervals')

ppc.df[fact.value=='0',fact.value:='None']
ppc.df[fact.value=='more than 10', fact.value:='More than 10']
ppc.df[,fact.value:=factor(fact.value,
                           levels=c('None','1','2-5',
                                    '6-10','More than 10'))]


pdf(paste0(temp.path,'ppc.pdf'),
    width=9,height=6)
ggplot(ppc.df, aes(x=fact.value)) +
   geom_col(data=ppc.df[model=='Negative Binomial'],
            aes(y=probs),
            col='white',
            fill='grey',
            alpha=.5) +
   facet_wrap(~new.labs) +
   geom_errorbar(aes(ymin=V1, ymax=V2, col=model, group=model),
                 width=.3,
                 position=position_dodge(.9),
                 size=.5)+
   scale_color_manual(name='',
                      values=c('black','darkgrey'))+
   labs(x='Number of Ties', y='Proportion')+
   theme_bw() +
   theme(legend.position=c(.4,.04),
         legend.key = element_rect(colour = 'transparent', 
                                   fill = 'white'),
         axis.text.x=element_text(angle=45,
                                  hjust=1),
         panel.grid.minor=element_blank(),
         panel.grid.major=element_blank(),
         # strip.background=element_rect(color='white',fill='white')
         strip.background=element_blank()
         )
dev.off()

# ppc.df.2 <- copy(ppc.df)
# ggplot(ppc.df[fact.value=='more than 10'|
#                  fact.value=='0'], aes(x=new.labs)) +
#    geom_col(data=ppc.df[(fact.value=='more than 10'|fact.value=='0') & 
#                            model=='Negative Binomial'],
#             aes(y=probs),
#             col='white',
#             fill='grey',
#             alpha=.5) +
#    geom_errorbar(aes(ymin=V1, ymax=V2, col=model, group=model),
#                  width=.3,
#                  position=position_dodge(.9),
#                  size=1)+
#    facet_wrap(~fact.value) + 
#    scale_color_manual(name='Models',
#                       values=c('black','darkgrey'))+
#    labs(x='Number of Ties', y='Proportion')+
#    theme_bw() 
# 
# ggplot(ppc.df, aes(x=new.labs)) +
#    geom_col(data=ppc.df[model=='Negative Binomial'],
#             aes(y=probs),
#             col='white',
#             fill='grey',
#             alpha=.5) +
#    facet_wrap(~fact.value) +
#    geom_errorbar(aes(ymin=V1, ymax=V2, col=model, group=model),
#                  width=.3,
#                  position=position_dodge(.9),
#                  size=1)+
#    scale_color_manual(name='Models',
#                       values=c('black','darkgrey'))+
#    labs(x='Number of Ties', y='Proportion')+
#    theme_bw() 


## Load Positions and Rotate -------------------------------------------------

# individual positions
pos.i <- gen.pos.df(fit=fit,
                    what='pos_i',
                    stat=median,
                    item=F)

# stimuli positions
pos.j <- gen.pos.df(fit=fit, 
                   what='pos_j',
                   item=T,
                   stat=median,
                   new.labs='abbr')

### Rotate (max ind variance)

# get number of dimensions
n.dim <- fit$fit@par_dims$pos_i[2]
dim.names <- paste0('dim.',1:n.dim)
r.dim.names <- paste0('r.',dim.names)

# rotate ind positions
rot <- rotate.pos(pos.i[,..dim.names], return.matrix=T)
r.pos.i <- cbind(pos.i, rot$new.pos)

# rotate stimuli positions
r.pos.j <- sweep(as.matrix(pos.j[,..dim.names]),2,rot$means) %*% rot$R %>%
    data.table %>%
    setnames(r.dim.names)
r.pos.j <- cbind(pos.j,r.pos.j)

### Data.tables for plotting

# df for individuals
df.i <- melt(r.pos.i, id.vars='ind') %>%
   filter(variable %in% r.dim.names)
# df for stimuli
df.j <- melt(r.pos.j[,-'ind'], id.vars='labs') %>%
   filter(variable %in% r.dim.names)
# jitter y-position of stimuli
library(ggrepel)

ggplot(df.i, aes(x=value)) +
   geom_histogram(bins=100, alpha=.2, col='grey') + 
   theme_bw()+
   geom_text_repel(data=df.j, 
             aes(x=value,y=0, label=labs),
             ylim=c(0,10),
             segment.alpha=.6,
             min.segment.length=.1)+
   facet_wrap(~variable, scales='free')

### Rotate (max g variance)

# rotate ind positions
J.rot <- rotate.pos(pos.j[,..dim.names], return.matrix=T)
colnames(J.rot$new.pos) <- paste0('J.',colnames(J.rot$new.pos))
r.pos.j <- cbind(r.pos.j, J.rot$new.pos)

# rotate stimuli positions
J.r.pos.i <- sweep(as.matrix(pos.i[,..dim.names]),
                   2,
                   J.rot$means) %*% J.rot$R %>%
   data.table %>%
   setnames(paste0('J.',r.dim.names))
r.pos.i <- cbind(r.pos.i,J.r.pos.i)

### Data.tables for plotting

# df for individuals
J.df.i <- melt(r.pos.i, id.vars='ind') %>%
   filter(variable %in% paste0('J.',r.dim.names))
# df for stimuli
J.df.j <- melt(r.pos.j[,-'ind'], id.vars='labs') %>%
   filter(variable %in% paste0('J.',r.dim.names))
# jitter y-position of stimuli
library(ggrepel)

ggplot(J.df.i, aes(x=value)) +
   geom_histogram(bins=100, alpha=.2, col='grey') + 
   theme_bw()+
   geom_text_repel(data=J.df.j, 
                   aes(x=value,y=0, label=labs),
                   ylim=c(0,10),
                   segment.alpha=.6,
                   min.segment.length=.1)+
   facet_wrap(~variable, scales='free')

## Delta ---------------------------------------------------------------------
# 
# post.delta <- as.data.table(fit$fit, pars='delta')
# samps <- sample(1L:nrow(post.delta),500,replace=F)
# df.delta <- melt(post.delta[samps])%>%data.table
# df.delta[,item.no:=gsub('delta\\[(.*)\\]','\\1',variable)%>%as.numeric]
# df.delta <- merge(df.delta, group.labs, by='item.no', all.x=T)
# df.delta[,long.labs:=factor(
#    long.labs, 
#    levels=df.delta[,mean(value), by='long.labs'][
#                   order(V1),long.labs]
#    )]
# 
# ggplot(df.delta, aes(x=long.labs, y=value)) + 
#    geom_jitter(size=1, 
#                col='grey', 
#                width=.2,
#                alpha=.5)+
#    geom_boxplot(outlier.size=0,
#                 outlier.shape=NA,
#                 # coef=0,
#                 alpha=.1,
#                 width=.5) +
#    # geom_hline(yintercept=1, col='red') +
#    theme_bw() +
#    labs(x='', y=expression(delta[j])) +
#    coord_flip()

## Distances -----------------------------------------------------------------
# 

pos.j <- as.matrix(fit$fit, 'pos_j') %>% colMeans %>%
   matrix(nrow=fit$data$n.items, ncol=n.dim) %>%
   data.table
pos.i <- as.matrix(fit$fit, 'pos_i') %>% colMeans %>% 
   matrix(nrow=fit$data$n.inds, ncol=n.dim) %>%
   data.table
pos <- rbind(pos.j, pos.i)
# get SVD
S <- svd(as.matrix(pos))
pfit <- as.matrix(pos) %*% S$v

df.items <- pfit[1:fit$data$n.items,1:2] %>% data.table
df.items[, labs:=group.labs[,short.labs]]
df.items[, long.labs:=group.labs[,long.labs]]
df.inds <- pfit[(fit$data$n.items+1):nrow(pfit),1:2] %>% 
   data.table

greg <- as.matrix(fit$fit, 'gamma')
alpha <- as.matrix(fit$fit, 'alpha')
greg.alpha <- sweep(greg,1,alpha,'+')
greg.alpha <- exp(greg.alpha)

df.inds[,greg:=colMeans(greg.alpha)]
pdf(paste0(temp.path,'pscatter.pdf'),
    width=7,height=7.5)
ggplot(df.inds, aes(x=V1, y=V2)) + 
   geom_point(aes(size=greg),col='grey', shape=1) + 
   geom_text(data=df.items, 
             aes(label=labs),
             size=5) +
   scale_size_continuous(name=expression(paste('Individual Gregariousness (',gamma[i],')')))+
   theme_bw() + 
   labs(x='', y='') +
   theme(legend.position='bottom') +
   coord_fixed()
dev.off()
# library(ggExtra)


ggplot(a, aes(x=V1, y=V2)) + 
   geom_point(aes(size=greg, col=race.fact),shape=1) + 
   geom_text(data=df.items, 
             aes(label=labs),
             size=5) +
   scale_size_continuous(name=expression(paste('Individual Gregariousness (',gamma[i],')')))+
   theme_bw() + 
   labs(x='', y='') +
   theme(legend.position='bottom') +
   coord_fixed()


# ggMarginal(g, 
#            type='histogram', 
#            color='white',
#            fill='grey',
#            alpha=.5,
#            bins=75)

# group dist
# d1 <- combn(nrow(pos.j),2, function(w) {
#    sqrt(sum((pos.j[w[1]]-pos.j[w[2]])^2))
# })
# 
# d2 <- combn(nrow(pos.j),2, function(w) {
#    sqrt(sum((df.items[w[1],1:2]-df.items[w[2],1:2])^2))
# })
# cor(d1,d2)
# 
# # ind dists
# d11 <- combn(nrow(pos.i),2, function(w) {
#    sqrt(sum((pos.i[w[1]]-pos.i[w[2]])^2))
# })
# 
# d21 <- combn(nrow(pos.i),2, function(w) {
#    sqrt(sum((df.inds[w[1],-3]-df.inds[w[2],-3])^2))
# })
# cor(d11,d21)



# 
# 
# exp.count <- function(fit.dat,
#                          n.samps=500) {
# 
#    n.dim <- fit$fit@par_dims$pos_i[2]
#    i.draws <- as.matrix(fit.dat$fit, 'pos_i')
#    j.draws <- as.matrix(fit.dat$fit, 'pos_j')
#    p.draws <- exp(as.matrix(fit.dat$fit, 'pops_logged'))
#    d.draws <- as.matrix(fit.dat$fit, 'delta')
#    g.draws <- as.matrix(fit.dat$fit, 'gamma')
#    a.draws <- as.matrix(fit.dat$fit, 'alpha')
# 
#    samps <- sample(seq_len(nrow(j.draws)),n.samps, replace=F)
# 
#    rm.count <- lapply(samps, function(w) {
#                   outer(exp(a.draws[w,] + g.draws[w,]),p.draws[w,])
#    })
#    
#    d.list <- lapply(samps, function(w) {
#          vec_pdist(
#             matrix(i.draws[w,],nrow=fit.dat$data$n.inds,ncol=n.dim),
#             matrix(j.draws[w,], nrow=fit.dat$data$n.items, ncol=n.dim)
#          )^(-d.draws[w,])
#    })
#    exp.count <- Map('*',rm.count,d.list)
#    return(list(rm.count, exp.count))
# }
# 
# count.list <- exp.count(fit, n.samps=300)
# rm.list <- count.list[[1]]
# df1 <- lapply(rm.list, colMeans) %>%
#    do.call('rbind',.) %>%
#    data.table %>%
#    setnames(group.labs[,short.labs]) %>%
#    melt
# ggplot(df1, aes(x=variable, y=value)) + geom_jitter()
# exp.list <- count.list[[2]]
# df2 <- lapply(exp.list, colMeans) %>%
#    do.call('rbind',.) %>%
#    data.table %>%
#    setnames(group.labs[,short.labs]) %>%
#    melt
# ggplot(df2, aes(x=variable, y=value)) + geom_jitter()
# 
# df <- rbind(df1[,group:='rm'],df2[,group:='exp'])
# ggplot(df, aes(x=variable, y=value,col=group)) + 
#    stat_boxplot(alpha=.5, outlier.shape=NA) +
#    theme_bw() + 
#    scale_y_log10(breaks=c(.1,1,10,100)) +
#    coord_flip()
# 
# 
# dij.list <- function(fit.dat, n.samps=100) {
#    
#    i.draws <- as.matrix(fit.dat$fit, 'pos_i')
#    j.draws <- as.matrix(fit.dat$fit, 'pos_j')
#    
#    samps <- sample(1:nrow(j.draws), n.samps, replace=F)
#    
#    lapply(samps, function(w) {
#       vec_pdist(
#          matrix(i.draws[w,],
#                 nrow=fit.dat$data$n.inds,
#                 ncol=fit.dat$fit@par_dims$pos_i[2]),
#          matrix(j.draws[w,],
#                 nrow=fit.dat$data$n.items,
#                 ncol=fit.dat$fit@par_dims$pos_i[2])
#       )
#    })
# }
# 
# dij <- dij.list(fit, n.samps=300)
# 
# med.ij.dists <- lapply(dij, function(w) apply(w,2,median)) %>%
#    do.call(rbind,.)%>%
#    data.table
# df <- melt(med.ij.dists)
# ggplot(df, aes(x=variable, y=value)) + geom_boxplot()


## Homophily -----------------------------------------------------------------

gen.pos.mats <- function(fit.dat, 
                         n.samps=300) {
   
   n.dim <- fit$fit@par_dims$pos_i[2]
   i.draws <- as.matrix(fit.dat$fit, 'pos_i')
   j.draws <- as.matrix(fit.dat$fit, 'pos_j')
   
   samps <- sample(1L:nrow(i.draws), n.samps, replace=F)
   
   i.samps <- i.draws[samps,]
   j.samps <- j.draws[samps,]
   
   j.list <- lapply(1L:n.samps,function(w) {
      matrix(j.draws[w,], nrow=fit.dat$data$n.items, ncol=n.dim)
   })
   i.list <- lapply(1L:n.samps, function(w) {
      matrix(i.draws[w,], nrow=fit.dat$data$n.inds, ncol=n.dim)
   })
   
   res <- Map(list, i.list, j.list)
   names(res) <- paste0('draw.',samps)
   return(list(pos.draws=res, samps=samps))
   
}

# get posterior draws of positions
pos.draws <- gen.pos.mats(fit,n.samps=300)
# sampled draws
samps <- pos.draws$samps
# positions
pos.draws <- pos.draws$pos.draws

# get delta draws
# delta.draws <- as.matrix(fit$fit, pars='delta')[samps,]
# delta.draws <- lapply(seq_len(nrow(delta.draws)),function(w) delta.draws[w,])
# merge to list of positions
# comb.draws <- Map(list,pos.draws, delta.draws)

# # calculate weighted distance
# dist.draws <- lapply(comb.draws, function(w) {
#    
#    # raw distances
#    dd <- vec_pdist(w[[1]][[1]], w[[1]][[2]])
#    # weighted distances
#    w.dd <- sweep(dd,2,w[[2]],"*")
#    
#    return(list(ij.dist=dd, w.ij.dist=w.dd))
#    })

# calculate distances

dist.draws <- lapply(pos.draws, function(w) {
   
   vec_pdist(w[[1]],w[[2]])
   
})

h.vars=c('work',
         'male',
         'race.fact',
         's.inc',
         'att.fact',
         's.educ',
         's.age',
         'married',
         # 's.size',
         'loc',
         'sym.3')

## Distanct to Blacks
# library(MASS)
# # which quantiles?
# quants <- c(.025,.5,.975)
# # which column is the var
# col.no <- fit$data$item.sum[grepl('black', labs),item.no]
# reg.black <- lapply(dist.draws, function(w) {
#    
#    df <- cbind(w[,col.no],dat)
#    df[,race.fact:=factor(race.fact,
#                          levels=c('Black','Hispanic','Other','White')
#                          )
#       ]
#    r1 <- lm(V1 ~ race.fact, data=df)
#    b.simple <- mvrnorm(1,coef(r1), vcov(r1))[2:4]
#    nn <- names(b.simple)
#    # nn <- names(r1)
#    f <- paste0('V1 ~ ',paste0(h.vars, collapse=' + '))
#    r2 <- lm(as.formula(f), data=df)
#    b.multiple <- mvrnorm(1,coef(r2),vcov(r2))
#    b.multiple <- b.multiple[nn]
#    # r2 <- r2[nn]
#    return(list(b.simple=b.simple, b.multiple=b.multiple))
# })
# 
# 
# black.simple <- lapply(reg.black,`[[`,'b.simple') %>%
#    do.call('rbind',.) %>% 
#    apply(2,function(w) quantile(w, probs=quants)) %>% 
#    t %>% 
#    cbind(rownames(.),.) %>%
#    data.table 
# 
# black.multiple <- lapply(reg.black,`[[`,'b.multiple') %>%
#    do.call('rbind',.) %>% 
#    apply(2,function(w) quantile(w, probs=quants)) %>% 
#    t %>% 
#    cbind(rownames(.),.) %>%
#    data.table 
# 
#    
# ## Whites
# 
# col.no <- fit$data$item.sum[grepl('white', labs),item.no]
# reg.white <- lapply(dist.draws, function(w) {
#    
#    df <- cbind(w[,col.no],dat)
#    r1 <- lm(V1 ~ race.fact, data=df)
#    b.simple <- mvrnorm(1,coef(r1), vcov(r1))[2:4]
#    nn <- names(b.simple)
#    f <- paste0('V1 ~ ',paste0(h.vars, collapse=' + '))
#    r2 <- lm(as.formula(f), data=df)
#    b.multiple <- mvrnorm(1,coef(r2),vcov(r2))
#    b.multiple <- b.multiple[nn]
#    return(list(b.simple=b.simple, b.multiple=b.multiple))
# })
# 
# white.simple <- lapply(reg.white,`[[`,'b.simple') %>%
#    do.call('rbind',.) %>% 
#    apply(2,function(w) quantile(w, probs=quants)) %>% 
#    t %>% 
#    cbind(rownames(.),.) %>%
#    data.table 
# white.multiple <- lapply(reg.white,`[[`,'b.multiple') %>%
#    do.call('rbind',.) %>% 
#    apply(2,function(w) quantile(w, probs=quants)) %>% 
#    t %>% 
#    cbind(rownames(.),.) %>%
#    data.table 
# 
# ### Liberals
# 
# col.no <- fit$data$item.sum[grepl('lib', labs),item.no]
# reg.lib <- lapply(dist.draws, function(w) {
#    
#    df <- cbind(w[,col.no],dat)
#    df[,sym.3:=factor(sym.3, levels=c('Liberal','Moderate','Conservative'))]
#    r1 <- lm(V1 ~ sym.3, data=df)
#    b.simple <- mvrnorm(1,coef(r1), vcov(r1))[2:3]
#    nn <- names(b.simple)
#    f <- paste0('V1 ~ ',paste0(h.vars, collapse=' + '))
#    r2 <- lm(as.formula(f), data=df)
#    b.multiple <- mvrnorm(1,coef(r2),vcov(r2))
#    b.multiple <- b.multiple[nn]
#    return(list(b.simple=b.simple, b.multiple=b.multiple))
# })
# 
# lib.simple <- lapply(reg.lib,`[[`,'b.simple') %>%
#    do.call('rbind',.) %>% 
#    apply(2,function(w) quantile(w, probs=quants)) %>% 
#    t %>% 
#    cbind(rownames(.),.) %>%
#    data.table 
# lib.multiple <- lapply(reg.lib,`[[`,'b.multiple') %>%
#    do.call('rbind',.) %>% 
#    apply(2,function(w) quantile(w, probs=quants)) %>% 
#    t %>% 
#    cbind(rownames(.),.) %>%
#    data.table 
# 
# ### Conservatives
# col.no <- fit$data$item.sum[grepl('con', labs),item.no]
# reg.con <- lapply(dist.draws, function(w) {
#    
#    df <- cbind(w[,col.no],dat)
#    df[,sym.3:=factor(sym.3, levels=c('Conservative','Moderate','Liberal'))]
#    r1 <- lm(V1 ~ sym.3, data=df)
#    b.simple <- mvrnorm(1,coef(r1), vcov(r1))[2:3]
#    nn <- names(b.simple)
#    f <- paste0('V1 ~ ',paste0(h.vars, collapse=' + '))
#    r2 <- lm(as.formula(f), data=df)
#    b.multiple <- mvrnorm(1,coef(r2),vcov(r2))
#    b.multiple <- b.multiple[nn]
#    return(list(b.simple=b.simple, b.multiple=b.multiple))
# })
# 
# con.simple <- lapply(reg.con,`[[`,'b.simple') %>%
#    do.call('rbind',.) %>% 
#    apply(2,function(w) quantile(w, probs=quants)) %>% 
#    t %>% 
#    cbind(rownames(.),.) %>%
#    data.table 
# con.multiple <- lapply(reg.con,`[[`,'b.multiple') %>%
#    do.call('rbind',.) %>% 
#    apply(2,function(w) quantile(w, probs=quants)) %>% 
#    t %>% 
#    cbind(rownames(.),.) %>%
#    data.table 
# 
# ### Rel attendance (regularly)
# col.no <- fit$data$item.sum[grepl('goatt', labs),item.no]
# reg.att <- lapply(dist.draws, function(w) {
#    
#    df <- cbind(w[,col.no],dat)
#    r1 <- lm(V1 ~ att.fact, data=df)
#    b.simple <- mvrnorm(1,coef(r1), vcov(r1))[2:3]
#    nn <- names(b.simple)
#    f <- paste0('V1 ~ ',paste0(h.vars, collapse=' + '))
#    r2 <- lm(as.formula(f), data=df)
#    b.multiple <- mvrnorm(1,coef(r2),vcov(r2))
#    b.multiple <- b.multiple[nn]
#    return(list(b.simple=b.simple, b.multiple=b.multiple))
# })
# 
# att.simple <- lapply(reg.att,`[[`,'b.simple') %>%
#    do.call('rbind',.) %>% 
#    apply(2,function(w) quantile(w, probs=quants)) %>% 
#    t %>% 
#    cbind(rownames(.),.) %>%
#    data.table 
# att.multiple <- lapply(reg.att,`[[`,'b.multiple') %>%
#    do.call('rbind',.) %>% 
#    apply(2,function(w) quantile(w, probs=quants)) %>% 
#    t %>% 
#    cbind(rownames(.),.) %>%
#    data.table 
# 
# 
# ### Relig Attend (rarely/never)
# col.no <- fit$data$item.sum[grepl('noatt', labs),item.no]
# reg.noatt <- lapply(dist.draws, function(w) {
#    # require(MASS)
#    df <- cbind(w[,col.no],dat)
#    df[, att.fact:=factor(att.fact,
#                          levels=c('Less than once a month',
#                                   'At least once a month',
#                                   'At least once a week')
#                   )
#       ]
#    r1 <- lm(V1 ~ att.fact, data=df)
#    b.simple <- mvrnorm(1,coef(r1), vcov(r1))[2:3]
#    nn <- names(b.simple)
#    f <- paste0('V1 ~ ',paste0(h.vars, collapse=' + '))
#    r2 <- lm(as.formula(f), data=df)
#    b.multiple <- mvrnorm(1,coef(r2),vcov(r2))
#    b.multiple <- b.multiple[nn]
#    return(list(b.simple=b.simple, b.multiple=b.multiple))
# })
# 
# noatt.simple <- lapply(reg.noatt,`[[`,'b.simple') %>%
#    do.call('rbind',.) %>% 
#    apply(2,function(w) quantile(w, probs=quants)) %>% 
#    t %>% 
#    cbind(rownames(.),.) %>%
#    data.table 
# noatt.multiple <- lapply(reg.noatt,`[[`,'b.multiple') %>%
#    do.call('rbind',.) %>% 
#    apply(2,function(w) quantile(w, probs=quants)) %>% 
#    t %>% 
#    cbind(rownames(.),.) %>%
#    data.table 
# 


### Generate latex 
# 
# h.res.tab <- rbind(black.simple,
#                    black.multiple,
#                    white.simple,
#                    white.multiple,
#                    lib.simple,
#                    lib.multiple,
#                    con.simple,
#                    con.multiple,
#                    att.simple,
#                    att.multiple,
#                    noatt.simple,
#                    noatt.multiple)
# h.res.tab <- cbind(
#    rep(
#       c('Black (Baseline = Black)',
#         'White (Baseline = White)',
#         'Strongly liberal (Baseline = Liberal)',
#         'Strongly conservative (Baseline = Conservative)',
#         'Attend regularly (Baseline = At least once a week)',
#         'Attend rarely/never (Baseline = Less than once a month)'),
#       times=c(6,6,4,4,4,4)),
#    c(rep(
#       rep(c('Raw',
#             'Adjusted'),
#           times=c(3,3)),
#       2),
#      rep(
#         rep(c('Raw',
#               'Adjusted'),
#             times=c(2,2)),
#         2),
#      rep(
#         rep(c('Raw',
#               'Adjusted'),
#             times=c(2,2)),
#         2)
#    ),
#    h.res.tab)
# setnames(h.res.tab,
#          c('Outcome', 
#            'Mean.diff',
#            'Variables',
#            'q.025',
#            'q.50',
#            'q.975'))


library(MASS)
# which quantiles?
quants <- c(.025,.5,.975)
# which column is the var
col.no <- fit$data$item.sum[grepl('black', labs),item.no]
reg.black <- lapply(dist.draws, function(w) {
   
   df <- cbind(w[,col.no],dat)
   df[,black.dum:=race.fact=='Black']
   r1 <- lm(V1 ~ black.dum, data=df)
   b.simple <- mvrnorm(1,coef(r1), vcov(r1))[2]
   nn <- names(b.simple)
   # nn <- names(r1)
   f <- paste0('V1 ~ black.dum + ',
               paste0(h.vars[h.vars!='race.fact'], collapse=' + '))
   r2 <- lm(as.formula(f), data=df)
   b.multiple <- mvrnorm(1,coef(r2),vcov(r2))
   b.multiple <- b.multiple[nn]
   # r2 <- r2[nn]
   return(list(b.simple=b.simple, b.multiple=b.multiple))
})


black.simple <- lapply(reg.black,`[[`,'b.simple') %>%
   do.call('rbind',.) %>% 
   apply(2,function(w) quantile(w, probs=quants)) %>% 
   t %>% 
   cbind(rownames(.),.) %>%
   data.table 

black.multiple <- lapply(reg.black,`[[`,'b.multiple') %>%
   do.call('rbind',.) %>% 
   apply(2,function(w) quantile(w, probs=quants)) %>% 
   t %>% 
   cbind(rownames(.),.) %>%
   data.table 


## Whites

col.no <- fit$data$item.sum[grepl('white', labs),item.no]
reg.white <- lapply(dist.draws, function(w) {
   
   df <- cbind(w[,col.no],dat)
   df[, white.dum:=race.fact=='White']
   r1 <- lm(V1 ~ white.dum, data=df)
   b.simple <- mvrnorm(1,coef(r1), vcov(r1))[2]
   nn <- names(b.simple)
   f <- paste0('V1 ~ white.dum + ',
               paste0(h.vars[h.vars!='race.fact'], collapse=' + '))
   r2 <- lm(as.formula(f), data=df)
   b.multiple <- mvrnorm(1,coef(r2),vcov(r2))
   b.multiple <- b.multiple[nn]
   return(list(b.simple=b.simple, b.multiple=b.multiple))
})

white.simple <- lapply(reg.white,`[[`,'b.simple') %>%
   do.call('rbind',.) %>% 
   apply(2,function(w) quantile(w, probs=quants)) %>% 
   t %>% 
   cbind(rownames(.),.) %>%
   data.table 
white.multiple <- lapply(reg.white,`[[`,'b.multiple') %>%
   do.call('rbind',.) %>% 
   apply(2,function(w) quantile(w, probs=quants)) %>% 
   t %>% 
   cbind(rownames(.),.) %>%
   data.table 


col.no <- fit$data$item.sum[grepl('hisp', labs),item.no]
reg.hisp <- lapply(dist.draws, function(w) {
   
   df <- cbind(w[,col.no],dat)
   df[,hisp.dum:=race.fact=='Hispanic']
   r1 <- lm(V1 ~ hisp.dum, data=df)
   b.simple <- mvrnorm(1,coef(r1), vcov(r1))[2]
   nn <- names(b.simple)
   # nn <- names(r1)
   f <- paste0('V1 ~ hisp.dum + ',
               paste0(h.vars[h.vars!='race.fact'], collapse=' + '))
   r2 <- lm(as.formula(f), data=df)
   b.multiple <- mvrnorm(1,coef(r2),vcov(r2))
   b.multiple <- b.multiple[nn]
   # r2 <- r2[nn]
   return(list(b.simple=b.simple, b.multiple=b.multiple))
})


hisp.simple <- lapply(reg.hisp,`[[`,'b.simple') %>%
   do.call('rbind',.) %>% 
   apply(2,function(w) quantile(w, probs=quants)) %>% 
   t %>% 
   cbind(rownames(.),.) %>%
   data.table 

hisp.multiple <- lapply(reg.hisp,`[[`,'b.multiple') %>%
   do.call('rbind',.) %>% 
   apply(2,function(w) quantile(w, probs=quants)) %>% 
   t %>% 
   cbind(rownames(.),.) %>%
   data.table 


### Liberals

col.no <- fit$data$item.sum[grepl('lib', labs),item.no]
reg.lib <- lapply(dist.draws, function(w) {
   
   df <- cbind(w[,col.no],dat)
   df[,lib.dum:=sym.ideo<=2]
   r1 <- lm(V1 ~ lib.dum, data=df)
   b.simple <- mvrnorm(1,coef(r1), vcov(r1))[2]
   nn <- names(b.simple)
   f <- paste0('V1 ~ lib.dum + ',
               paste0(h.vars[h.vars!='sym.3'], collapse=' + '))
   r2 <- lm(as.formula(f), data=df)
   b.multiple <- mvrnorm(1,coef(r2),vcov(r2))
   b.multiple <- b.multiple[nn]
   return(list(b.simple=b.simple, b.multiple=b.multiple))
})

lib.simple <- lapply(reg.lib,`[[`,'b.simple') %>%
   do.call('rbind',.) %>% 
   apply(2,function(w) quantile(w, probs=quants)) %>% 
   t %>% 
   cbind(rownames(.),.) %>%
   data.table 
lib.multiple <- lapply(reg.lib,`[[`,'b.multiple') %>%
   do.call('rbind',.) %>% 
   apply(2,function(w) quantile(w, probs=quants)) %>% 
   t %>% 
   cbind(rownames(.),.) %>%
   data.table 

### Conservatives
col.no <- fit$data$item.sum[grepl('con', labs),item.no]
reg.con <- lapply(dist.draws, function(w) {
   
   df <- cbind(w[,col.no],dat)
   df[,con.dum:=sym.ideo>=6]
   r1 <- lm(V1 ~ con.dum, data=df)
   b.simple <- mvrnorm(1,coef(r1), vcov(r1))[2]
   nn <- names(b.simple)
   f <- paste0('V1 ~ con.dum + ',
               paste0(h.vars[h.vars!='sym.3'], collapse=' + '))
   r2 <- lm(as.formula(f), data=df)
   b.multiple <- mvrnorm(1,coef(r2),vcov(r2))
   b.multiple <- b.multiple[nn]
   return(list(b.simple=b.simple, b.multiple=b.multiple))
})

con.simple <- lapply(reg.con,`[[`,'b.simple') %>%
   do.call('rbind',.) %>% 
   apply(2,function(w) quantile(w, probs=quants)) %>% 
   t %>% 
   cbind(rownames(.),.) %>%
   data.table 
con.multiple <- lapply(reg.con,`[[`,'b.multiple') %>%
   do.call('rbind',.) %>% 
   apply(2,function(w) quantile(w, probs=quants)) %>% 
   t %>% 
   cbind(rownames(.),.) %>%
   data.table 

### Rel attendance (regularly)
col.no <- fit$data$item.sum[grepl('goatt', labs),item.no]
reg.att <- lapply(dist.draws, function(w) {
   
   df <- cbind(w[,col.no],dat)
   r1 <- lm(V1 ~ att.week, data=df)
   b.simple <- mvrnorm(1,coef(r1), vcov(r1))[2]
   nn <- names(b.simple)
   f <- paste0('V1 ~ att.week + ',
               paste0(h.vars[h.vars!='att.fact'], collapse=' + '))
   r2 <- lm(as.formula(f), data=df)
   b.multiple <- mvrnorm(1,coef(r2),vcov(r2))
   b.multiple <- b.multiple[nn]
   return(list(b.simple=b.simple, b.multiple=b.multiple))
})

att.simple <- lapply(reg.att,`[[`,'b.simple') %>%
   do.call('rbind',.) %>% 
   apply(2,function(w) quantile(w, probs=quants)) %>% 
   t %>% 
   cbind(rownames(.),.) %>%
   data.table 
att.multiple <- lapply(reg.att,`[[`,'b.multiple') %>%
   do.call('rbind',.) %>% 
   apply(2,function(w) quantile(w, probs=quants)) %>% 
   t %>% 
   cbind(rownames(.),.) %>%
   data.table 


### Relig Attend (rarely/never)
col.no <- fit$data$item.sum[grepl('noatt', labs),item.no]
reg.noatt <- lapply(dist.draws, function(w) {
   # require(MASS)
   df <- cbind(w[,col.no],dat)
   df[, ly.dum:=attend %in% c('lt once a year', 'never')]
   r1 <- lm(V1 ~ ly.dum, data=df)
   b.simple <- mvrnorm(1,coef(r1), vcov(r1))[2]
   nn <- names(b.simple)
   f <- paste0('V1 ~ ly.dum + ',
               paste0(h.vars[h.vars!='att.fact'], collapse=' + '))
   r2 <- lm(as.formula(f), data=df)
   b.multiple <- mvrnorm(1,coef(r2),vcov(r2))
   b.multiple <- b.multiple[nn]
   return(list(b.simple=b.simple, b.multiple=b.multiple))
})

noatt.simple <- lapply(reg.noatt,`[[`,'b.simple') %>%
   do.call('rbind',.) %>% 
   apply(2,function(w) quantile(w, probs=quants)) %>% 
   t %>% 
   cbind(rownames(.),.) %>%
   data.table 
noatt.multiple <- lapply(reg.noatt,`[[`,'b.multiple') %>%
   do.call('rbind',.) %>% 
   apply(2,function(w) quantile(w, probs=quants)) %>% 
   t %>% 
   cbind(rownames(.),.) %>%
   data.table 


### Generate latex 

h.res.tab <- rbind(black.simple,
      black.multiple,
      hisp.simple,
      hisp.multiple,
      white.simple,
      white.multiple,
      lib.simple,
      lib.multiple,
      con.simple,
      con.multiple,
      att.simple,
      att.multiple,
      noatt.simple,
      noatt.multiple)
h.res.tab <- cbind(
   rep(
      c('Black',
        'Hispanic',
        'White',
        'Strongly lib.',
        'Strongly cons.',
        'Attend regularly',
        'Attend rarely/never'),
      each=2),
   c(rep(c('Unadjusted',
         'Adjusted'),
      times=7)),
   h.res.tab)
setnames(h.res.tab,
         c('Outcome', 
           'Mean.diff',
           'Variables',
           'q.025',
           'q.50',
           'q.975'))
# 
# h.res.tab <- melt(h.res.tab,
#                   id.vars=c('Outcome',
#                             'Mean.diff',
#                             'Variables')
#                   ) %>%
#    setnames('variable','quantile')
qs <- c('q.025','q.50','q.975')
h.res.tab[,(qs):=lapply(.SD, as.numeric), .SDcols=qs]
h.res.tab[,Variables:=sub('att.fact|race.fact|sym.3','',Variables)]
h.res.tab[,Outcome:=factor(Outcome,
        levels=c('Black',
                 'Hispanic',
                 'White',
                 'Strongly lib.',
                 'Strongly cons.',
                 'Attend regularly',
                 'Attend rarely/never'))
        ]


temp.path <- 'C:/Users/struxture/Dropbox/###_work_current/#_weak_ties/drafts/'

pdf(paste0(temp.path,'homophily.pdf'),
    width=8, height=4)
ggplot(h.res.tab,
       aes(col=Mean.diff)) +
   geom_hline(yintercept=0, 
              col='lightgray',
              linetype=2)+
   geom_linerange(position=position_dodge(width=.5),
                  aes(x=Outcome,
                      ymin=q.025,
                      ymax=q.975))+
   geom_point(position=position_dodge(width=.5),
              aes(x=Outcome, y=q.50),
              shape=21,
              fill='white') + 
   scale_color_manual(name='',
                      values=c('darkgrey','black'))+
   theme_bw()+
   labs(x='',y='Choice Homophily') +
   theme(legend.position='bottom',
         panel.grid=element_blank())
dev.off()


# pdf(paste0(temp.path,'homophily.pdf'),
#     width=6, height=6)
#    ggplot(h.res.tab,
#        aes(col=Mean.diff)) +
#    geom_hline(yintercept=0, 
#               col='lightgray',
#               linetype=2)+
#    geom_linerange(position=position_dodge(width=.5),
#                  aes(x=Variables,
#                      ymin=q.025,
#                      ymax=q.975))+
#    geom_point(position=position_dodge(width=.5),
#               aes(x=Variables, y=q.50),
#               shape=21,
#               fill='white') + 
#    scale_color_manual(name='Mean Difference',
#                       values=c('darkgrey','black'))+
#    theme_bw()+
#    coord_flip()+
#    labs(x='',y='Difference in Distance to Baseline') +
#    facet_wrap(~Outcome, 
#               ncol=1,
#               scales='free_y') +
#    theme(legend.position='bottom',
#          panel.grid=element_blank(),
#          strip.background=element_blank())
# dev.off()

# 
# h.res.tab <- dcast(h.res.tab,
#               Outcome + Variables ~ Mean.diff, 
#               value.var=c('q.50','q.025','q.975'))
# setcolorder(h.res.tab, c(1,2,4,6,8,3,5,7))
# nn <- names(h.res.tab)
# nn <- nn[nn %in% c('Outcome','Variables')==F]
# h.res.tab <- h.res.tab[,(nn):=lapply(.SD,
#                                function(w) round(as.numeric(w),2)
#                                ),
#                                .SDcols=nn]
# h.res.tab[,cred.raw:=paste0('(',q.025_Raw,',',q.975_Raw,')')]
# h.res.tab[,cred.adj:=paste0('(',q.025_Adjusted,',',q.975_Adjusted,')')]
# h.res.tab[,`:=`(q.025_Raw=NULL,
#                 q.975_Raw=NULL,
#                 q.025_Adjusted=NULL,
#                 q.975_Adjusted=NULL)]
# setcolorder(h.res.tab, c(1,2,3,5,4,6))
# h.res.tab[,Variables:=sub('race\\.fact|sym\\.3','',Variables)]
# h.res.tab[,Outcome:=factor(Outcome, 
#                            levels=c('Black',
#                                     'White',
#                                     'Strongly liberal',
#                                     'Strongly conservative',
#                                     'Att. relig. regularly',
#                                     'Att. relig. rarely/never'
#                                     )
#                      )
#           ]
# setorder(h.res.tab, Outcome)
# 
# print(xtable(h.res.tab,digits=2), 
#       include.rownames=F)

## Spatial Consolidation -----------------------------------------------------


gen.j.dists.mat <- function(fit.dat,
                        n.samps=300) {
   
   pos.j <- as.matrix(fit.dat$fit, pars='pos_j')
   n.dim <- fit$fit@par_dims$pos_i[2]
   samps <- sample(seq_len(nrow(pos.j)), n.samps, replace=F)
   jj <- pos.j[samps,]
   
   j.list <- lapply(seq_len(n.samps), function (w) 
      matrix(jj[w,],
             nrow=fit.dat$data$n.items,
             ncol=n.dim)
   )
   # d.mat <- as.matrix(fit.dat$fit, pars='delta')[samps,]
   # d.list <- lapply(seq_len(n.samps), function (w)
   #    d.mat[w,]
   # )
   # 
   # comb.list <- Map(list, j.list,d.list)
   
   # dist.list <- lapply(comb.list, function (w) {
   #    
   #    x <- w[[1]]
   #    d <- w[[2]]
   #    
   #    j.dists.raw <- vec_pdist(x,x)
   #    j.dists.weighted <- sweep(j.dists.raw,2,d,'*')
   # 
   #    list(j.dists.raw,j.dists.weighted)
   # })
   # 
   # raw.dists <- lapply(dist.list,`[[`,1)
   # w.dists <- lapply(dist.list, `[[`,2)
   # 
   # return(list(raw.dists=raw.dists, w.dists=w.dists))
   lapply(j.list, function(w) vec_pdist(w,w))

}

j.dists <- gen.j.dists.mat(fit)
# w.dists <- j.dists$w.dists
med.dists <- lapply(j.dists, as.vector) %>% 
   do.call('rbind',.) %>%
   apply(2,median) %>%
   matrix(ncol=13,nrow=13)
mean.dists <- Reduce('+',j.dists)/length(j.dists)                        
rownames(med.dists) <- colnames(med.dists) <- fit$data$item.sum[,labs]



princomp(pos)
prcomp(pos.j)
pfit <- princomp(pos.j)
plot(pfit$scores[,1:2], type='n')
text(pfit$scores[,1:2], labels=1:13)

z <- svd(pos.j)
names(z)
zfit <- as.matrix(pos.j) %*% z$v
zfit <- z$u[,1:2] %*% diag(z$d)[1:2,1:2] %*% t(z$v[,1:2]) %>%
    data.frame

library(igraph)
g <- max(mean.dists) - mean.dists + 1
g <- graph_from_adjacency_matrix(g, mode='undirected',weighted=T)
g <- simplify(g)
plot(g, layout=layout_with_kk, edge.width=g$weight)

hc <- hclust(as.dist(med.dists))
new.dists <- med.dists[hc$order, hc$order]


df <- melt(med.dists) %>%data.table
df[Var1==Var2, value:=NA]
# df[,q5:=cut(value, 
#             breaks=quantile(value, seq(0,1,.25),include.lowest=T))]
ggplot(df, aes(x=Var1,y=Var2, fill=value)) + 
   geom_tile()+
   # scale_fill_viridis(name='Spatial\nConsolidation',
   #                    option='A', discrete=T, end=.75,
   #                    direction=-1) +
   # scale_fill_gradient(low='grey90',high='black', na.value='white')+
   scale_fill_viridis(option='A', alpha=.8)+
   # geom_tile(data=temp, aes(x=Var1,y=Var2),
   #           fill='white', col='white')+
   geom_abline(intercept=0,slope=1)+
   coord_fixed() +
   theme_bw()+
   scale_x_discrete(expand=c(0,0))+
   scale_y_discrete(expand=c(0,0))+
   labs(x=NULL,y=NULL) +
   theme(axis.text.x=element_text(angle=90),
         axis.ticks=element_blank(),
         axis.line=element_blank(),
         panel.grid.major=element_blank())



row.angle <- function(X,
                      weighted=F,
                      delta=NULL) {
   
   # note: input should be matrix of |pos.j-pos.i|
   #       which contains in each row the difference between
   #       group position and ind position
   #       it should have, therefore, n.group rows and n.dim cols
   
   # for all nrow choose 2, do
   combn(nrow(X),2, function(w) {
      # get ith row
      x <- X[w[1],]
      # get jth row
      y <- X[w[2],]
      if (weighted) {
         # get ith delta
         dx <- delta[w[1]]
         # get jth delta
         dy <- delta[w[2]]
      }
      
      # calculate norms
      x.norm <- sqrt(sum(x^2))
      y.norm <- sqrt(sum(y^2))
      if (weighted) {
         # get maximum of norms
         m.dist <- max(dx*x.norm,dy*y.norm)
      }
      # generate raw angle
      r.angle <- acos(sum(x*y)/(x.norm*y.norm))
      if (weighted) {
      # generate weighted angle
      w.angle <- r.angle/m.dist
      # return indices and angle
      c(w[1],w[2],r.angle,w.angle)
      } else {
         c(w[1],w[2],r.angle)
      }
   }) %>% t
   
}

get.consolidation <- function(fit.dat, 
                              weighted=F,
                              n.samps=100) {
   
   # get draws
   post.draws.i <- as.matrix(fit.dat$fit, pars='pos_i')
   post.draws.j <- as.matrix(fit.dat$fit, pars='pos_j')
   if (weighted)
      post.draws.delta <- as.matrix(fit.dat$fit, pars='delta')
   
   # subsample n.samps
   samps <- sample(1:nrow(post.draws.i), n.samps, replace=F)
   
   # get ind position
   pos.i <- post.draws.i[samps,]
   # get group position
   pos.j <- post.draws.j[samps,]
   # get delta
   if (weighted)
      delta <- post.draws.delta[samps,]
   
   # number of individuals
   n.inds <- fit.dat$data$n.inds
   # number of groups
   n.items <- fit.dat$data$n.items
   # number of dimensions
   n.dims <- fit.dat$fit@par_dims$pos_i[2]
   
   # get matrix form lists
   i.list <- lapply(1L:n.samps,function(w) {
      
      matrix(pos.i[w,], nrow=n.inds, ncol=n.dims)
      
   })
   j.list <- lapply(1L:n.samps, function(w) {
      matrix(pos.j[w,], nrow=n.items, ncol=n.dims)
   })
   
   # consolidation from the persp of each individual
   angle.dat <- lapply(1L:n.samps, function (w) {
      
      # get individual for wth draw (matrix form)
      ii <- i.list[[w]]
      # get groups for wth draw
      jj <- j.list[[w]]
      # get delta for wth draw
      if (weighted) 
         dd <- delta[w,]
      
      # subtract i's position from group positions, i.e., |ii - jj|
      # result is an n.inds-length list with each element equal to
      # to a matrix which has pos.j-pos.i as a row
      ss <- lapply(1L:n.inds,function(z) sweep(jj,2,ii[z,]))
      
      # calculate angles
      if (weighted) {
         angles <- lapply(ss, row.angle, delta=dd) %>%
            do.call('rbind',.)
         n.combs <- choose(n.items,2)
         angles <- cbind(angles, 
                         rep(1:n.inds, each=n.combs),
                         rep(w,nrow(angles)))  %>%
            data.table %>%
            setnames(c('item.1','item.2','r.angle','w.angle','ind','draw'))
         return(angles)
      } else {
         angles <- lapply(ss, row.angle) %>%
            do.call('rbind',.)
         n.combs <- choose(n.items,2)
         angles <- cbind(angles, 
                         rep(1:n.inds, each=n.combs),
                         rep(w,nrow(angles)))  %>%
            data.table %>%
            setnames(c('item.1','item.2','r.angle','ind','draw'))
      }
      
   }) %>% rbindlist
   
   return(angle.dat)
   
}



c.dat <- get.consolidation(fit, n.samps=100)
r2d <- function(a) a*180/base::pi
c.dat[, n.angle:= r2d(r.angle)]

# average over population
x <- c.dat[,mean(n.angle),by=list(item.1,item.2,draw)]
x <- x[, median(V1), by=list(item.1,item.2)]
x[,lab.1:=fit$data$item.sum[item.1,labs]]
x[,lab.2:=fit$data$item.sum[item.2,labs]]

temp <- x[,list(item.2,item.1,V1,lab.2,lab.1)] %>%
   setnames(c('item.1','item.2','V1','lab.1','lab.2'))
z <- rbind(x,temp)
# z[,deg := rad2degree(V1)]
# z[,deg.2:=ifelse(deg>90,T,F)]
# z[,q5:=cut(deg, breaks=quantile(deg, probs=seq(0,1,1/5), include.lowest=T))]

z[,q5:=cut(V1, breaks=quantile(V1, probs=seq(0,1,1/4)), include.lowest=T)]

long.labs.2 <- as.data.table(long.labs)
setorder(long.labs.2[,temp:=c(2,1,3:nrow(long.labs.2))],temp)[,temp:=NULL]
long.labs.2[,num.labs:=1:nrow(long.labs.2)]
long.labs.2[,long.labs:=paste0(num.labs,'. ',long.labs)]
long.labs.2[,num.labs:=factor(num.labs, levels=num.labs)]
long.labs.2[,long.labs:=factor(long.labs, levels=rev(long.labs))]

z <- merge(z, long.labs.2[,.(long.labs,labs)], by.x='lab.1', by.y='labs')
setnames(z, 'long.labs', 'long.labs.1')
z <- merge(z, long.labs.2[,.(labs, num.labs)], by.x='lab.2', by.y='labs')

# segment <- data.table(x=seq(.5,12.5,1),
#                       xend=seq(1.5,13.5,1),
#                       y=seq(12.5,.5,-1),
#                       yend=seq(13.5,1.5,-1))

pdf(paste0(temp.path,'consolidation.pdf'),
    width=7,height=6)
ggplot(z, aes(x=num.labs,y=long.labs.1)) + 
   geom_tile(aes(fill=V1), col='white', alpha=.9) +
   # geom_text(aes(label=round(V1,2)))+
   # scale_fill_viridis(name='Spatial\nConsolidation',
   #                    option='A', discrete=F, end=.75,
   #                    direction=1) +
   # scale_fill_brewer()+
   scale_fill_gradient(low='darkblue',high='grey90')+
   # scale_fill_gradient(name='',
   #                     low='grey95', high='black')+
   # geom_tile(data=temp, aes(x=Var1,y=Var2),
   #           fill='white', col='white')+
   geom_abline(intercept=14,slope=-1)+
   coord_fixed() +
   # geom_segment(data=segment,
   #              aes(x=x,
   #                  xend=xend,
   #                  y=y,
   #                  yend=yend))+
   # geom_segment(aes(x=.5,xend=1.5,y=12.5,yend=13.5))+
   # geom_segment(aes(x=12.5,xend=13.5,y=.5,yend=1.5))+
   theme_bw()+
   scale_x_discrete(expand=c(0,0))+
   scale_y_discrete(expand=c(0,0))+
   labs(x=NULL,y=NULL) +
   theme(axis.ticks=element_blank(),
         # axis.text.x=element_text(angle=90, hjust=1),
         axis.line=element_blank(),
         panel.grid.major=element_blank())
dev.off()



# blacks and whites
a.dat <- merge(dat[,-'new.id'], fit$data$ind.sum, by.x='old.id',by.y='old.id',all=T)
x <- merge(c.dat,a.dat[,.(new.id,race)], by.x='ind',by.y='new.id', all.x=T)
x <- x[race %in% c('black', 'white')]
x <- x[,mean(n.angle),by=list(item.1,item.2,race,draw)]
x <- x[, median(V1), by=list(item.1,item.2, race)]
x[,lab.1:=fit$data$item.sum[item.1,labs]]
x[,lab.2:=fit$data$item.sum[item.2,labs]]

temp <- x[,list(item.2,item.1,V1,lab.2,lab.1,race)] %>%
   setnames(c('item.1','item.2','V1','lab.1','lab.2','race'))
z <- rbind(x,temp)
# z[,deg := rad2degree(V1)]
# z[,deg.2:=ifelse(deg>90,T,F)]
# z[,q5:=cut(deg, breaks=quantile(deg, probs=seq(0,1,1/5), include.lowest=T))]

# z[,q5:=cut(V1, breaks=quantile(V1, probs=seq(0,1,1/4)), include.lowest=T)]

long.labs.2 <- as.data.table(long.labs)
setorder(long.labs.2[,temp:=c(2,1,3:nrow(long.labs.2))],temp)[,temp:=NULL]
long.labs.2[,num.labs:=1:nrow(long.labs.2)]
long.labs.2[,long.labs:=paste0(num.labs,'. ',long.labs)]
long.labs.2[,num.labs:=factor(num.labs, levels=num.labs)]
long.labs.2[,long.labs:=factor(long.labs, levels=rev(long.labs))]

z <- merge(z, long.labs.2[,.(long.labs,labs)], by.x='lab.1', by.y='labs')
setnames(z, 'long.labs', 'long.labs.1')
z <- merge(z, long.labs.2[,.(labs, num.labs)], by.x='lab.2', by.y='labs')
# z[,V2:=rescale(V1,0,1)]
temp <- z[race=='white']
temp <- merge(temp, z[race=='black'],by=c('lab.2','lab.1','item.1','item.2',
                           'long.labs.1','num.labs'))
temp[,`:=`(race.x=NULL,
           race.y=NULL,
           V1=V1.x-V1.y)]
temp[,`:=`(V1.x=NULL,
           V1.y=NULL)]
temp[,race:='Difference']
comb.z <- rbind(z,temp)

# segment <- data.table(x=seq(.5,12.5,1),
#                       xend=seq(1.5,13.5,1),
#                       y=seq(12.5,.5,-1),
#                       yend=seq(13.5,1.5,-1))

# pdf(paste0(temp.path,'consolidation.pdf'),
#     width=7,height=6)

ggplot(comb.z[race!='Difference'], aes(x=num.labs,y=long.labs.1)) + 
   geom_tile(aes(fill=V1), col='white', alpha=.9) +
   # geom_text(aes(label=round(V1,2)))+
   # scale_fill_viridis(name='Spatial\nConsolidation',
   #                    option='A', discrete=F, end=.75,
   #                    direction=1) +
   # scale_fill_brewer()+
   scale_fill_gradient2('',low='darkblue',
                        mid='white',
                        high='darkred',
                        midpoint=90)+
   # scale_fill_gradient(name='',
   #                     low='grey95', high='black')+
   # geom_tile(data=temp, aes(x=Var1,y=Var2),
   #           fill='white', col='white')+
   geom_abline(intercept=14,slope=-1)+
   coord_fixed() +
   # geom_segment(data=segment,
   #              aes(x=x,
   #                  xend=xend,
   #                  y=y,
   #                  yend=yend))+
   # geom_segment(aes(x=.5,xend=1.5,y=12.5,yend=13.5))+
   # geom_segment(aes(x=12.5,xend=13.5,y=.5,yend=1.5))+
   theme_bw()+
   scale_x_discrete(expand=c(0,0))+
   scale_y_discrete(expand=c(0,0))+
   labs(x=NULL,y=NULL) +
   theme(axis.ticks=element_blank(),
         # axis.text.x=element_text(angle=90, hjust=1),
         axis.line=element_blank(),
         panel.grid.major=element_blank()) +
   facet_wrap(~race)
   # geom_text(aes(label=round(V1,2)))
# dev.off()


ggplot(comb.z[race=='Difference'], aes(x=num.labs,y=long.labs.1)) + 
   geom_tile(aes(fill=V1), col='white', alpha=.9) +
   # geom_text(aes(label=round(V1,2)))+
   # scale_fill_viridis(name='',
   #                    option='B', discrete=F, end=.75,
   #                    direction=1) +
   # scale_fill_brewer()+
   scale_fill_gradient2('White-Black',
                        low = 'darkblue',
                        mid = 'white', 
                        high = 'darkred',
                        midpoint=0) +
   # scale_fill_gradient(name='',
   #                     low='grey95', high='black')+
   # geom_tile(data=temp, aes(x=Var1,y=Var2),
   #           fill='white', col='white')+
   geom_abline(intercept=12,slope=-1)+
   coord_fixed() +
   # geom_segment(data=segment,
   #              aes(x=x,
   #                  xend=xend,
   #                  y=y,
   #                  yend=yend))+
   # geom_segment(aes(x=.5,xend=1.5,y=12.5,yend=13.5))+
   # geom_segment(aes(x=12.5,xend=13.5,y=.5,yend=1.5))+
   theme_bw()+
   scale_x_discrete(expand=c(0,0))+
   scale_y_discrete(expand=c(0,0))+
   labs(x=NULL,y=NULL) +
   theme(axis.ticks=element_blank(),
         # axis.text.x=element_text(angle=90, hjust=1),
         axis.line=element_blank(),
         panel.grid.major=element_blank())



# libs and cons

a.dat <- merge(dat[,-'new.id'], fit$data$ind.sum, by='old.id',all=T)
x <- merge(c.dat,a.dat[,.(new.id,sym.3)], by.x='ind',by.y='new.id', all.x=T)
x <- x[,mean(n.angle),by=list(item.1,item.2,sym.3,draw)]
x <- x[, median(V1), by=list(item.1,item.2, sym.3)]
x[,lab.1:=fit$data$item.sum[item.1,labs]]
x[,lab.2:=fit$data$item.sum[item.2,labs]]

temp <- x[,list(item.2,item.1,V1,lab.2,lab.1,sym.3)] %>%
   setnames(c('item.1','item.2','V1','lab.1','lab.2','sym.3'))
z <- rbind(x,temp)
# z[,deg := rad2degree(V1)]
# z[,deg.2:=ifelse(deg>90,T,F)]
# z[,q5:=cut(deg, breaks=quantile(deg, probs=seq(0,1,1/5), include.lowest=T))]

# z[,q5:=cut(V1, breaks=quantile(V1, probs=seq(0,1,1/4)), include.lowest=T)]

long.labs.2 <- as.data.table(long.labs)
setorder(long.labs.2[,temp:=c(2,1,3:nrow(long.labs.2))],temp)[,temp:=NULL]
long.labs.2[,num.labs:=1:nrow(long.labs.2)]
long.labs.2[,long.labs:=paste0(num.labs,'. ',long.labs)]
long.labs.2[,num.labs:=factor(num.labs, levels=num.labs)]
long.labs.2[,long.labs:=factor(long.labs, levels=rev(long.labs))]

z <- merge(z, long.labs.2[,.(long.labs,labs)], by.x='lab.1', by.y='labs')
setnames(z, 'long.labs', 'long.labs.1')
z <- merge(z, long.labs.2[,.(labs, num.labs)], by.x='lab.2', by.y='labs')

ggplot(z[sym.3 %in% c('Liberal','Conservative')], aes(x=num.labs,y=long.labs.1)) + 
   geom_tile(aes(fill=V1), col='white', alpha=.9) +
   # geom_text(aes(label=round(V1,2)))+
   # scale_fill_viridis(name='Spatial\nConsolidation',
   #                    option='A', discrete=F, end=.75,
   #                    direction=1) +
   # scale_fill_brewer()+
   scale_fill_gradient('',low='darkblue',
                        high='white')+
   # scale_fill_gradient(name='',
   #                     low='grey95', high='black')+
   # geom_tile(data=temp, aes(x=Var1,y=Var2),
   #           fill='white', col='white')+
   geom_abline(intercept=14,slope=-1)+
   coord_fixed() +
   # geom_segment(data=segment,
   #              aes(x=x,
   #                  xend=xend,
   #                  y=y,
   #                  yend=yend))+
   # geom_segment(aes(x=.5,xend=1.5,y=12.5,yend=13.5))+
   # geom_segment(aes(x=12.5,xend=13.5,y=.5,yend=1.5))+
   theme_bw()+
   scale_x_discrete(expand=c(0,0))+
   scale_y_discrete(expand=c(0,0))+
   labs(x=NULL,y=NULL) +
   theme(axis.ticks=element_blank(),
         # axis.text.x=element_text(angle=90, hjust=1),
         axis.line=element_blank(),
         panel.grid.major=element_blank()) +
   facet_wrap(~sym.3)
# geom_text(aes(label=round(V1,2)))
# dev.off()


## regs


reg.dat <- merge(dat, r.pos.i, by.x='new.id',by.y='ind', all=T)

reg.res <- dim.reg(fit.dat=fit,
                dims=1:n.dim,
                i.vars=c('pid.3',
                         'work',
                         'male',
                         'race.fact',
                         's.inc',
                         'att.week',
                         's.educ',
                         's.age',
                         'married',
                         # 's.size',
                         'loc',
                         'sym.3',
                         's.occ',
                         'bush.00',
                         'bush.04'),
                p.dat=reg.dat,
                id.var='new.id',
                simple=T,
                rotation=rot,
                n.draws=300,
                R2=F,
                s.values=F)

r2.res <- dim.reg(fit.dat=fit,
                  dims=1:n.dim,
                  i.vars=c('pid.3',
                           'work',
                           'male',
                           'race.fact',
                           's.inc',
                           'att.week',
                           's.educ',
                           's.age',
                           'married',
                           's.size',
                           # 'loc',
                           'sym.3',
                           's.occ',
                           'bush.00',
                           'bush.04'),
                  p.dat=reg.dat,
                  id.var='new.id',
                  simple=F,
                  rotation=rot,
                  n.draws=300,
                  R2=T,
                  s.values=T)

R2.q <- sapply(r2.res$reg.list, 
               function(w) quantile(w['R2',],
                                    c(.025,.5,.975)))

S.q <- apply(r2.res$s.values, 
             2, 
             function(w) quantile(w^2,
                                  c(.025,.5,.975)))

norm.S.q <- apply(r2.res$s.values, 
                  1, 
                  function(w) {
                     w^2/sum(w^2)
                  }) %>%
            apply(1, function(w) quantile(w,c(.025,.5,.975)))

temp <- rbind(norm.S.q, R2.q)
colnames(temp) <- paste0('Dimension ',1:6)

print(xtable(temp, digits=3), include.rownames=T)

# get quantiles
qs <- lapply(reg.res$reg.list,function(w) apply(w,
                                    1,
                                    quantile,
                                    probs=c(.025,.05,.5,.95,.975)))
# generate dataset
df <- lapply(1:length(qs), function(w) {
   x<- t(qs[[w]]) 
   x <- x[,c(3,1,2,4,5)]
   x <- data.table(rownames(x),x,rep(w,nrow(x)))
   return(x)
   }) %>% 
   rbindlist %>%
   setnames(c('var','median','min1','min2','max2','max1','dim'))
df[,dim:=paste0('Dimension ',dim)]
                # '\n Median R-squared: ',
                # round(R2.q[2,dim],2),
                # ' (',round(R2.q[1,dim],2),',',
                # round(R2.q[3,dim],2),')')]


ggplot(df[var %in% c('(Intercept)','R2')==F], aes(x=var)) +
   geom_hline(yintercept=0, col='red', alpha=.6)+
   # geom_segment(aes(x=1,
   #                  xend=3,
   #                  y=0,
   #                  yend=0),
   #              col='red',
   #              linetype=2,
   #              alpha=.6)+
   geom_point(aes(y=median),size=2) +
   geom_linerange(aes(ymin=min1,ymax=max1),
                  size=.7, 
                  col=viridis(1,begin=.5)) +
   geom_linerange(aes(ymin=min2,ymax=max2),
                  size=1.2,
                  col=viridis(1,begin=.2)) +
   labs(x='', y='Coefficients')+
   scale_x_discrete(drop=F)+
   coord_flip() + 
   theme_bw() + 
   facet_wrap(~dim, nrow=1)

# new labels

df[, var.2:=recode(var,
                   'work'='Work fulltime',
                   'sym.3Liberal'='Liberal',
                   'sym.3Conservative'='Conservative',
                   's.occ'='Occupational Prestige',
                   's.inc'='Family income',
                   's.educ'='Years of education',
                   's.age'='Age',
                   'race.factOther'='Other (race)',
                   'race.factHispanic'='Hispanic',
                   'race.factBlack'='Black',
                   'pid.3Democrat'='Democrat',
                   'pid.3Republican'='Republican',
                   'married'='Married',
                   'male'='Male',
                   'locsmsa (1-100)'='Largest 100 SMSA',
                   'locother urban'='Other urban area',
                   'bush.04'='Voted Bush (2004)',
                   'bush.00'='Voted Bush (2000)',
                   'att.week'='Attend rel. more than weakly')]

df[,var.3:=factor(var.2, levels=c('Family income',
                                  'Years of education',
                                  'Occupational Prestige',
                                  'Work fulltime',
                                  'Black',
                                  'Hispanic',
                                  'Other (race)',
                                  'Democrat',
                                  'Republican',
                                  'Liberal',
                                  'Conservative',
                                  'Voted Bush (2004)',
                                  'Voted Bush (2000)',
                                  'Attend rel. more than weakly',
                                  'Largest 100 SMSA',
                                  'Other urban area',
                                  'Male',
                                  'Married',
                                  'Age'
                                  ))]
df[,var.4:=factor(var.3, levels=rev(levels(var.3)))]

temp.path <- 'C:/Users/struxture/Dropbox/###_work_current/#_weak_ties/drafts/'

pdf(paste0(temp.path,'dimreg.pdf'), width=9, height=4)          
ggplot(df[var %in% c('(Intercept)','R2')==F], aes(x=var.4)) +
   geom_hline(yintercept=0, col='red', alpha=.6)+
   # geom_segment(aes(x=1,
   #                  xend=3,
   #                  y=0,
   #                  yend=0),
   #              col='red',
   #              linetype=2,
   #              alpha=.6)+
   geom_point(aes(y=median),size=2) +
   geom_linerange(aes(ymin=min1,ymax=max1),
                  size=.7, 
                  col=viridis(1,begin=.5)) +
   geom_linerange(aes(ymin=min2,ymax=max2),
                  size=1.2,
                  col=viridis(1,begin=.2)) +
   labs(x='', y='Coefficients')+
   scale_x_discrete(drop=F)+
   coord_flip() + 
   theme_bw() + 
   facet_wrap(~dim, nrow=1) 
dev.off()
new.l <- c('Race','Socioeconomic Status','Political Orientation','Other Demographics')
temp <- c(rep(NA,7), 
          rep(NA,7), 
          rep(NA,7), 
          rep(NA,7)) %>%
   matrix(nrow=4, ncol=7, byrow=T)  %>%
   data.table
temp <- temp[,lapply(.SD, as.numeric)]
temp <- cbind(temp,new.l)
colnames(temp) <- names(df)

df.2 <- rbind(df, temp)    


ggplot(df.2[var %in% c('(Intercept)','R2')==F], aes(x=var)) +
   geom_hline(yintercept=0, col='red', alpha=.6)+
   # geom_segment(aes(x=1,
   #                  xend=3,
   #                  y=0,
   #                  yend=0),
   #              col='red',
   #              linetype=2,
   #              alpha=.6)+
   geom_point(aes(y=median),size=2) +
   geom_linerange(aes(ymin=min1,ymax=max1),
                  size=.7, 
                  col=viridis(1,begin=.5)) +
   geom_linerange(aes(ymin=min2,ymax=max2),
                  size=1.2,
                  col=viridis(1,begin=.2)) +
   labs(x='', y='Coefficients')+
   scale_x_discrete(drop=F)+
   coord_flip() + 
   theme_bw() + 
   facet_wrap(~dim, nrow=1)

## group positions


#################

temp <- pfit[14:nrow(pfit),]%>%data.table %>%
   setnames(paste0('r.dim.',1:4))
temp[,ind:=1:nrow(temp)]

z = merge(dat, temp, by.x='new.id',by.y='ind', all=T)
z[,r.dim.3.c4:=cut(r.dim.3, 
                  breaks=quantile(r.dim.3,seq(0,1,.25)), 
                  include.lowest=T,
                  labels=c('Dimension 3:\n Lowest Quartile',
                           'Dimension 3:\n 2nd Quartile',
                           'Dimension 3:\n 3rd Quartile',
                           'Dimension 4:\n Highest Quartile'))]
z[,r.dim.4.c4:=cut(r.dim.4, 
                  breaks=quantile(r.dim.4,seq(0,1,.25)), 
                  include.lowest=T,
                  labels=c('Dimension 4:\n Lowest Quartile',
                           'Dimension 4:\n 2nd Quartile',
                           'Dimension 4:\n 3rd Quartile',
                           'Dimension 4:\n Highest Quartile'))]
z[,r.dim.1.c3:=cut(r.dim.1, 
                   breaks=quantile(r.dim.1,c(0,1/3,2/3,1)), 
                   include.lowest=T,
                   labels=c('Dimension 3:\n Low',
                            'Dimension 3:\n Middle',
                            'Dimension 3:\n High'))]
z[,r.dim.2.c3:=cut(r.dim.2, 
                   breaks=quantile(r.dim.2,c(0,1/3,2/3,1)), 
                   include.lowest=T,
                   labels=c('Dimension 4:\n Low',
                            'Dimension 4:\n Middle',
                            'Dimension 4:\n High'))]

z[,r.dim.3.c3:=cut(r.dim.3, 
                   breaks=quantile(r.dim.3,c(0,1/3,2/3,1)), 
                   include.lowest=T,
                   labels=c('Dimension 3:\n Low',
                            'Dimension 3:\n Middle',
                            'Dimension 3:\n High'))]
z[,r.dim.4.c3:=cut(r.dim.4, 
                   breaks=quantile(r.dim.4,c(0,1/3,2/3,1)), 
                   include.lowest=T,
                   labels=c('Dimension 4:\n Low',
                            'Dimension 4:\n Middle',
                            'Dimension 4:\n High'))]

pdf(paste0(temp.path,'race.pdf'), width=10, height=8)
ggplot(z[!is.na(race.fact) & race.fact!='Other'], 
       aes(x=r.dim.1,y=r.dim.2, col=race.fact, shape=race.fact)) +
   geom_point(size=3, alpha=.8) + 
   theme_bw()+
   scale_color_manual(name='Race',
                      values=c('grey','black','black'))+
   scale_shape_manual(name='Race',
                      values=c(19,3,19))+
   labs(x='Dimension 1', y='Dimension 2')+
   facet_grid(r.dim.3.c3 ~ r.dim.4.c3) +
   theme(legend.position='bottom')
dev.off()


ggplot(z[!is.na(race.fact) & race.fact!='Other'], 
       aes(x=r.dim.1,y=r.dim.2, col=race.fact, shape=race.fact)) +
   geom_point(size=3, alpha=.8) + 
   theme_bw()+
   scale_color_manual(name='Race',
                      values=c('grey','black','black'))+
   scale_shape_manual(name='Race',
                      values=c(19,3,19))+
   labs(x='Dimension 1', y='Dimension 2')+
   facet_grid(r.dim.3.c3 ~ r.dim.4.c3) +
   theme(legend.position='bottom')



z[,inc.2:=recode(inc.fact,
                 'lowest'='Lowest 30%',
                 'highest'='Highest 30%')]
pdf(paste0(temp.path,'inc.pdf'), width=10, height=6)
ggplot(z[!is.na(inc.2) & inc.fact!='middle'], aes(x=r.dim.1,y=r.dim.2, col=inc.2)) +
   geom_point(size=3, alpha=.8) + 
   theme_bw()+
   scale_color_manual(name='Household Income',
                      values=c('grey','black'))+
   labs(x='Dimension 1', y='Dimension 2')+
   facet_grid(r.dim.3.c3 ~ r.dim.4.c3) +
   theme(legend.position='bottom')
dev.off()

ggplot(z[!is.na(s.inc)], aes(x=r.dim.1,y=r.dim.3, col=s.inc)) +
   geom_point(size=3, alpha=.8) + 
   theme_bw()+
   scale_color_viridis(name='Household Income')+
   # scale_colour_gradient(name='Household Income',
   #                      low='darkblue',
   #                      high='grey')+
   labs(x='Dimension 1', y='Dimension 2')+
   facet_grid(r.dim.2.c3 ~ r.dim.4.c3) +
   theme(legend.position='bottom')

ggplot(z[!is.na(educ)], aes(x=r.dim.1,y=r.dim.3, col=educ)) +
   geom_point(size=3) + 
   theme_bw()+
   scale_colour_gradient(name='Years of Education')+
   labs(x='Dimension 1', y='Dimension 2')+
   facet_grid(r.dim.2.c3 ~ r.dim.4.c3) +
   theme(legend.position='bottom')

pdf(paste0(temp.path,'ideo.pdf'), width=10, height=8)
ggplot(z[!is.na(sym.3) & sym.3 != 'Moderate'], aes(x=r.dim.1,y=r.dim.2, col=factor(sym.3))) +
   geom_point(size=3, alpha=.8) +
   theme_bw() +
   labs(x='Dimension 1', y='Dimension 2')+
   scale_color_manual(name='Ideological Self-Identification',
                      values=c('grey','black'))+
   facet_grid(r.dim.3.c3 ~ r.dim.4.c3)+
   theme(legend.position='bottom')
dev.off()

z[,sym.fact:=factor(sym.ideo, labels=c('Extremely Liberal',
                                       'Liberal',
                                       'Slightly Liberal',
                                       'Moderate',
                                       'Slightly Conservative',
                                       'Conservative',
                                       'Extremely Conservative'))]
ggplot(z[!is.na(sym.fact) & sym.fact!='Moderat'], 
       aes(x=r.dim.1,y=r.dim.2, col=factor(sym.fact))) +
   geom_point(size=2, alpha=1) +
   theme_bw() +
   labs(x='Dimension 1', y='Dimension 2')+
   scale_color_brewer(name='Symbolic Ideology',
                        palette='RdBu')+
   # scale_color_manual(name='Symbolic Ideology',
   #                    values=c('red4','red1','orange',
   #                             'grey','dodgerblue1',
   #                             'blue1','blue4')) +
   facet_grid(r.dim.3.c3 ~ r.dim.4.c3)+
   theme(legend.position='bottom')


ggplot(z[!is.na(pid.3) & pid.3!='Independent'], aes(x=r.dim.1,y=r.dim.2, col=factor(pid.3))) +
   geom_point(size=2, alpha=.8) +
   scale_color_manual(values=c('blue','red'))+
   labs(x='Dimension 1', y='Dimension 2')+
   theme_bw()+
   facet_grid(r.dim.3.c3 ~ r.dim.4.c3)+
   theme(legend.position='bottom')

z[,pid.fact:=factor(pid, labels=c('Strong Democrat',
                                  'Democrat',
                                  'Leaning Democrat',
                                  'Independent/Other',
                                  'Leaning Republican',
                                  'Republican',
                                  'Strong Republican'))]

ggplot(z[!is.na(att.fact)], aes(x=r.dim.1,y=r.dim.2, col=factor(att.fact))) +
   geom_point(size=2, alpha=.8) +
   # scale_color_brewer(name='Partisanship',
   #                    palette='RdBu')+
   labs(x='Dimension 1', y='Dimension 2')+
   theme_bw()+
   facet_grid(r.dim.3.c3 ~ r.dim.4.c3)+
   theme(legend.position='bottom')


ggplot(z[!is.na(pid.fact)], aes(x=r.dim.1,y=r.dim.2, col=factor(pid.fact))) +
   geom_point(size=2, alpha=.8) +
   scale_color_brewer(name='Partisanship',
                      palette='RdBu')+
   labs(x='Dimension 1', y='Dimension 2')+
   theme_bw()+
   facet_grid(r.dim.3.c3 ~ r.dim.4.c3)+
   theme(legend.position='bottom')


ggplot(z[!is.na(loc)], aes(x=r.dim.1,y=r.dim.2, col=factor(loc), shape=factor(loc))) +
   geom_point(size=3, alpha=.8) +
   theme_bw() +
   labs(x='Dimension 1', y='Dimension 2')+
   scale_shape_manual('',values=c(2,8,19))+
   # scale_color_manual(name='Ideological Self-Identification',
   #                    values=c('grey','black'))+
   facet_grid(r.dim.3.c3 ~ r.dim.4.c3)+
   theme(legend.position='bottom')


##################################################################
n.dim <- 4
dist.race <- gen.b.group.dists(pos.dat=reg.dat,
                               n.dims=n.dim,
                               group='race.fact',
                               which.cats=c('White','Black')) %>%
            as.vector
# dist.pid <- gen.b.group.dists(pos.dat=reg.dat,
#                                n.dims=5,
#                                group='pid.3',
#                                which.cats=c('Democrat','Republican')) %>%
#    as.vector

dist.sym <- gen.b.group.dists(pos.dat=reg.dat,
                              n.dims=n.dim,
                              group='sym.3',
                              which.cats=c('Liberal','Conservative')) %>%
                                 as.vector

dist.inc <- gen.b.group.dists(pos.dat=reg.dat,
                              n.dims=n.dim,
                              group='inc.fact',
                              which.cats=c('lowest','highest')) %>%
   as.vector


dist.sorted <- gen.b.group.dists(pos.dat=reg.dat,
                                 n.dims=4,
                                 group='sorted',
                                 which.cats=c('Republican.Conservative',
                                              'Democrat.Liberal')) %>%
   as.vector

dist.df <- data.table(dist=c(dist.race,
                             # dist.pid,
                             dist.sym,
                             dist.inc,
                             dist.sorted),
                      group=c(rep('Race: White/Black',
                                  length(dist.race)),
                              # rep('PID: Democrats/Republican', 
                              #     length(dist.pid)),
                              rep('Symbolic Ideo: Liberal/Conservative',
                                  length(dist.sym)),
                              rep('Income: Highest 30%/Lowest 30%',
                                  length(dist.inc)),
                              rep('Sorted: Lib. Dem/Cons. Rep',
                                  length(dist.sorted)))
                      )

meds <- dist.df[,median(dist),by=group]%>%
   setnames(c('group','median'))
ggplot(dist.df, aes(x=dist, col=group)) + 
   # geom_histogram(alpha=.2, position=position_identity(),
   #                bins=50,
   #                aes(y=..density..)) +
   geom_density(alpha=.2)+
   # geom_boxplot()+
   scale_color_discrete()+
   theme_bw() +
   labs(x='Distance (Posterior Median)', y='Density')

cols <- c('black','blue','red','green')
ggplot(data.frame(dist.race), aes(dist.race)) + 
   geom_density(col=cols[1], fill=cols[1],alpha=.3) + 
   geom_vline(xintercept=median(dist.race), col=cols[1], linetype=2)+
   # geom_density(data=data.frame(dist.pid), aes(dist.pid),col=cols[2],
   #                fill=cols[2], alpha=.3,
   #                position=position_identity()) +
   # geom_vline(xintercept=median(dist.pid), col=cols[2], linetype=2)+
   geom_density(data=data.frame(dist.sym), aes(dist.sym),col=cols[3],
                fill=cols[3], alpha=.3,
                position=position_identity()) +
   geom_vline(xintercept=median(dist.sym), col=cols[3], linetype=2) +
   geom_density(data=data.frame(dist.inc), aes(dist.inc),col=cols[4],
                fill=cols[4], alpha=.3,
                position=position_identity()) +
   geom_vline(xintercept=median(dist.inc), col=cols[4], linetype=2) +
   labs(x='Distance (Posterior Medians)', y='Density') +
   theme_bw()

gss.06.valid <- fread(paste0(data.path,'gss.2006.csv'),
                      na.strings=c("NA",""))
temp <- gss.06.valid[ ,list(id,marblk,acqblack)]

dists <- gen.ind.dists(fit)
neg.dists <- 1/(1+exp(dists))
temp.dat <- merge(reg.dat, temp, by.x='old.id', by.y='id', all.x=T)
temp.dat[,marblk:=recode(marblk,
                         'strongly oppose'=1,
                         'oppose'=2,
                         'neither favor nor oppose'=3,
                         'favor'=4,
                         'strongly favor'=5)]
n.miss <- !is.na(temp.dat$marblk)
temp.dat <- temp.dat[!is.na(marblk)]
temp.dat[, W:=dists[n.miss,n.miss] %*% marblk]

lm(marblk ~ W+ factor(acqblack) + s.pid + s.sym + race.fact + s.inc+ att.week + s.educ + age, data=temp.dat)%>%summary


# 
# hc <- hclust(as.dist(dists))
# new.dists <- dists[hc$order, hc$order]
# temp <- melt(new.dists)
# ggplot(temp, aes(x=Var1,y=Var2)) + geom_raster(aes(fill=value)) +
#    scale_fill_viridis()
# library(factoextra)
# library(NbClust)
# par(mfrow=c(1,3))
# 
# fviz_nbclust(as.matrix(pos.i[,-'ind']), 
#              hcut, 
#              method = "silhouette")
# fviz_nbclust(as.matrix(pos.i[,-'ind']), 
#              hcut, 
#              method = "wss")
# fviz_nbclust(as.matrix(pos.i[,-'ind']), 
#              hcut, 
#              method = "gap_stat")
# nb <- NbClust(pos.i[,-'ind'], method='complete')
# fviz_nbclust(nb)

## Plot Space --------------------------------------------------------------------
# 
# p.col <- viridis(2, begin=0, end=.5)
# pos.j <- gen.pos.df(fit=fit, 
#                     what='pos_j', 
#                     item=T, 
#                     stat=mean,
#                     sd=T, 
#                     new.labs='abbr')
# 
# J.r.pos.j <- rotate.pos(pos.j[,list(dim.1,dim.2,dim.3)], return.matrix=T)
# pos.j <- cbind(pos.j, J.r.pos.j$new.pos)
# 
# pos.i <- gen.pos.df(fit=fit,
#                     what='pos_i',
#                     item=F)
# J.r.pos.i <- as.matrix(pos.i[,list(dim.1,dim.2,dim.3)]) %*% J.r.pos.j$R %>%
#    data.table %>%
#    setnames(c('r.dim.1','r.dim.2','r.dim.3'))
# pos.i <- cbind(pos.i,J.r.pos.i)
# 
# 
# plot_ly(pos.i,
#         x = ~ r.dim.1,
#         y = ~ r.dim.2,
#         z = ~ r.dim.3) %>%
#    layout(scene = list(xaxis=list(range=c(-2,2)),
#                        yaxis=list(range=c(-2,2)),
#                        zaxis=list(range=c(-2,2))
#                   )
#    ) %>%
#    add_markers(marker=list(size=5,
#                            color= ~ r.dim.3,
#                            colorscale='Viridis',
#                            showscale=T,
#                            opacity=.3)
#    ) %>%
#    add_text(data=pos.j,
#                    inherit=T,
#                    text = ~labs)
# 
# 
# I.r.pos.i <- rotate.pos(pos.i[,list(dim.1,dim.2,dim.3)], return.matrix=T)
# r.i.pos <- data.table(I.r.pos.i$new.pos) %>%
#            setnames(c('i.dim.1','i.dim.2','i.dim.3'))
# pos.i <- cbind(pos.i, r.i.pos)
# 
# I.r.pos.j <- as.matrix(pos.j[,list(dim.1,dim.2,dim.3)]) %*% I.r.pos.i$R %>%
#    data.table %>%
#    setnames(c('i.dim.1','i.dim.2','i.dim.3'))
# pos.j <- cbind(pos.j,I.r.pos.j)
# 
# 
# plot_ly(pos.i,
#         x = ~ i.dim.1,
#         y = ~ i.dim.2,
#         z = ~ i.dim.3) %>%
#    layout(scene = list(xaxis=list(range=c(-2,2)),
#                        yaxis=list(range=c(-2,2)),
#                        zaxis=list(range=c(-2,2))
#    )
#    ) %>%
#    add_markers(marker=list(size=5,
#                            color= ~ i.dim.3,
#                            colorscale='Viridis',
#                            showscale=T,
#                            opacity=.3)
#    ) %>%
#    add_text(data=pos.j,
#             inherit=T,
#             text = ~labs)
# # 2D PLOTS!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# plot.j <- ggplot(pos.j, aes(x=dim.1, y=dim.2)) +
#                   geom_segment(aes(x=dim.1-sd.1,
#                                    xend=dim.1+sd.1,
#                                    y=dim.2,
#                                    yend=dim.2),
#                                col='grey',
#                                alpha=.7)+
#                   geom_segment(aes(x=dim.1,
#                                    xend=dim.1,
#                                    y=dim.2-sd.2,
#                                    yend=dim.2+sd.2),
#                                col='grey',
#                                alpha=.7)+
#                   geom_point()+
#                   geom_text(aes(label=labs),
#                             hjust=0,
#                          nudge_y=.15,
#                          nudge_x=.02) +
#                   theme_bw() +
#                   labs(x='Dimension 1', y='Dimension 2')
# 
# 
# 
# plot.i <- ggplot(pos.i, aes(x=dim.1, y=dim.2)) +
#    geom_point()+
#    theme_bw() +
#    labs(x='Dimension 1', y='Dimension 2')
# plot.ij <- ggplot(pos.j, aes(x=dim.1, y=dim.2)) +
#    geom_point(data=pos.i, col=p.col[2], alpha=.6) +
#    geom_segment(aes(x=dim.1-sd.1,
#                     xend=dim.1+sd.1,
#                     y=dim.2,
#                     yend=dim.2),
#                 col='grey',
#                 alpha=.7)+
#    geom_segment(aes(x=dim.1,
#                     xend=dim.1,
#                     y=dim.2-sd.2,
#                     yend=dim.2+sd.2),
#                 col='grey',
#                 alpha=.7)+
#    geom_point()+
#    geom_text(aes(label=labs),
#              hjust=0,
#              nudge_y=.15,
#              nudge_x=.02) +
#    theme_bw() +
#    labs(x='Dimension 1', y='Dimension 2')
# 
# plot.ij


gamma.df <- gen.reff.df(fit, what='gamma',
                        item=F,
                        stat=median,
                        error=c('q','posterior'),
                        n.samps=100
)
# gamma.df[, ind:=factor(ind, 
#                        levels=gamma.df[,ind][order(value)])
#          ]

gamma.draws <- gamma.df[variable %in% c('value','hi','lo')==F]
gamma.stats <- dcast(gamma.df[variable %in% c('value','hi','lo')], 
                     ind ~ variable,
                     value.var='value')

gamma.stats[, ind:=factor(ind, levels=gamma.stats[order(value),ind])]
ggplot(gamma.stats, aes(x=ind, y=exp(value))) + 
   geom_hline(yintercept=1,col='red',linetype=2, size=.8)+
   geom_linerange(aes(ymin=exp(lo),ymax=exp(hi)), 
                  col='grey',
                  alpha=.6) +
   geom_point(col='black', size=.8)+
   theme_bw() +
   theme(axis.title.y=element_blank(),
         axis.text.y=element_blank(),
         axis.ticks.y=element_blank(),
         panel.grid.minor.y=element_blank(),
         panel.grid.major.y=element_blank()
   ) +
   labs(y=expression(gamma)) +
   coord_flip()

ggplot(gamma.draws, aes(x=exp(value),group=variable)) + 
   geom_density(col=viridis(1, 
                            begin=.5,
                            end=.5, 
                            alpha=.2),
                size=.2) +
   # geom_density(data=gamma.stats,
   #              aes(x=exp(value)),
   #              inherit.aes=F,
   #               col='black',
   #              size=.8) +
   geom_hline(yintercept=0, col='lightgray', size=.8) +
   theme_bw() +
   labs(x=expression(gamma),y='') 

ggplot(gamma.draws, aes(x=value,group=variable)) + 
   geom_density(col=viridis(1, 
                            begin=.5,
                            end=.5, 
                            alpha=.2),
                size=.2) +
   # geom_density(data=gamma.stats,
   #              aes(x=value),
   #              inherit.aes=F,
   #              col='black',
   #              size=.8) +
   geom_hline(yintercept=0, col='lightgray', size=.8) +
   theme_bw() +
   labs(x=expression(log(gamma)), y='') +
   ggtitle('Distribution of (logged) gregariousness parameter')


# ggplot(gamma.df, aes(x=ind, y=value)) + 
#    geom_hline(yintercept=1,col='red',linetype=2, size=.8)+
#    geom_linerange(aes(ymin=lo,ymax=hi), 
#                   col=p.col[2],
#                   alpha=.6) +
#    geom_point(col='black', size=.8)+
#    theme_bw() +
#    theme(axis.title.y=element_blank(),
#          axis.text.y=element_blank(),
#          axis.ticks.y=element_blank(),
#          panel.grid.minor.y=element_blank(),
#          panel.grid.major.y=element_blank()
#    ) +
#    labs(y=expression(gamma)) +
#    coord_flip()




dims <- paste0('dim.',1:n.dim)
libs <- pos.j[labs=='Lib', -c('labs','ind')] %>% as.matrix

i.libs <- reg.dat[sym.3=='Liberal',..dims] %>% as.matrix
i.cons <- reg.dat[sym.3=='Conservative',..dims] %>% as.matrix
i.mod <- reg.dat[sym.3=='Moderate',..dims] %>% as.matrix
mean(vec_pdist(i.libs, libs))
mean(vec_pdist(i.mod,libs))
mean(vec_pdist(i.cons, libs))

blacks <- pos.j[labs=='Black', -c('labs','ind')] %>% as.matrix
i.black <- reg.dat[race.fact=='Black',..dims] %>% as.matrix
i.other.black <- reg.dat[race.fact!='Black',..dims] %>% as.matrix
mean(vec_pdist(i.black,blacks))
mean(vec_pdist(i.other.black,blacks))

whites <- pos.j[labs=='White', -c('labs','ind')] %>% as.matrix
i.white <- reg.dat[race.fact=='White',..dims] %>% as.matrix
i.other.white <- reg.dat[race.fact!='White',..dims] %>% as.matrix
mean(vec_pdist(i.white,whites))
mean(vec_pdist(i.other.white,whites))

dist.black <- vec_pdist(as.matrix(reg.dat[,..dims]), 
                        as.matrix(pos.j[labs=='Black',-c('labs','ind')]))
lm(dist.black ~ reg.dat$race.fact) %>% summary

dist.white <- vec_pdist(as.matrix(reg.dat[,..dims]), 
                        as.matrix(pos.j[labs=='White',-c('labs','ind')]))
lm(dist.white ~ reg.dat$race.fact) %>% summary

dist.lib <- vec_pdist(as.matrix(reg.dat[,..dims]), 
                        as.matrix(pos.j[labs=='Lib',-c('labs','ind')]))
lm(dist.lib ~ reg.dat$sym.3) %>% summary

dist.cons <- vec_pdist(as.matrix(reg.dat[,..dims]), 
                        as.matrix(pos.j[labs=='Con',-c('labs','ind')]))
lm(dist.cons ~ reg.dat$sym.3) %>% summary

dist.goatt <- vec_pdist(as.matrix(reg.dat[,..dims]),
                        as.matrix(pos.j[labs=='Goatt',-c('labs','ind')]))
lm(dist.goatt ~ reg.dat$attend) %>% summary

dist.noatt <- vec_pdist(as.matrix(reg.dat[,..dims]),
                        as.matrix(pos.j[labs=='Noatt',-c('labs','ind')]))
lm(dist.noatt ~ reg.dat$attend) %>% summary



# normalized distances between stimuli
delta.j <- as.matrix(fit$fit, 'delta') %>% 
   apply(2,median)
stand.j <- apply(as.matrix(pos.j[,-c('ind','labs')]),
                 2,
                 function(w) {w*delta.j/mean(delta.j)})
j.dists <- vec_pdist(stand.j,stand.j)
rownames(j.dists) <- colnames(j.dists) <- pos.j[,labs]
j.dists

p.labels <- c('In Prison',
              'Black',
              'Extre. Conservative',
              'Cohabiting Women',
              'Gay/Lesbian',
              'Own 2nd Home',
              'Rarely Attends Rel.',
              'Unemployed',
              'Asian',
              'Hispanic',
              'White',
              'Reg. Attends Rel.',
              'Extre. Liberal')
rownames(j.dists) <- colnames(j.dists) <- p.labels
hc <- hclust(as.dist(j.dists))
h.dists <- j.dists[hc$order, hc$order]
df <- melt(j.dists) %>% data.table
df[,value.2:=cut(value, breaks=quantile(value,
                                 probs=seq(0,1,1/5),
                                 na.rm=T),
                 include.lowest=T)]
temp <- df[Var1==Var2]
df <- df[Var1!=Var2]
# df <- df[as.numeric(Var1)>as.numeric(Var2)][
#    order(-value)]
ggplot(df, aes(x=Var1,y=Var2)) + 
   geom_tile(aes(fill=value), col='white', alpha=.9) +
   # scale_fill_viridis(name='Estimated\nDistances',
   #                    option='B', discrete=T, end=.75,
   #                    direction=-1) +
   scale_fill_gradient(low='darkblue',high='grey90')+
   geom_tile(data=temp, aes(x=Var1,y=Var2),
             fill='white', col='white')+
   geom_abline(intercept=0,slope=1)+
   coord_fixed() +
   theme_bw()+
   scale_x_discrete(expand=c(0,0))+
   scale_y_discrete(expand=c(0,0))+
   labs(x=NULL,y=NULL) +
   theme(axis.text.x=element_text(angle=90),
         axis.ticks=element_blank(),
         axis.line=element_blank(),
         panel.grid.major=element_blank())
# +
#    theme(axis.text.x=element_text(angle=90),
#          axis.ticks=element_blank(),
#          axis.line=element_blank(),
#          panel.border=element_blank(),
#          panel.grid.major=element_blank())

res2 <- smacofSym(j.dists,ndim=2)
res3 <- smacofSym(j.dists,ndim=3)
