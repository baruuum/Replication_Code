
## 
## Analyzing the Space
##


# load packages

library(dplyr)
library(data.table)
library(dtplyr)
library(ggplot2)
library(plotly)
library(rstan)
library(viridis)
library(loo)

# rstan_options(auto_write=TRUE)
# options(mc.cores=parallel::detectCores())


### Setting Paths
if (!exists('wd.base')) {
   wd.base <- ifelse(.Platform$OS.type=='windows',
      'C:/Users/struxture/Dropbox/###_work_current/#_weak_ties/analysis/',
      '/scratch/bp1094/weakties/')
   code.path <- paste0(wd.base,'##_code/')
   mcode.path <- paste0(code.path,'Model_Codes/')
   rawdata.path <- paste0(wd.base,'##_rawdata/')
   data.path <- paste0(wd.base,'data/')
   res.path <- paste0(wd.base,'results/')
   output.path <- paste0(wd.base,'output/')
}

# load functions
source(paste0(code.path,'0_Functions.R'))


## Load Data ---------------------------------------------------------------------

# what model?
tie.type <- 'acq.core.s'
models <- c('N.ER','N.RM','N2.euc','N2.euc.delta','N3.euc','N3.euc.delta','N4.euc','N4.euc.delta','N5.euc.delta')

for (mm in models) {
   model.no <- mm
   
   fit <- readRDS(paste0(res.path,
                         tie.type,'/',
                         ifelse(.Platform$OS.type=='windows','','model'),
                         model.no,'/',
                         paste0(tie.type,'.',
                                model.no,
                                '.rds'
                         )
   )
   )
   # number of posterior draws (assuming no thining)
   m.info <- fit$fit@stan_args[[1]]
   n.draws <- (m.info$iter-m.info$warmup)*length(fit$fit@stan_args)
   
   # no of respondents
   n.ind <- fit$data$n.inds
   # no of items
   n.ite <- fit$data$n.items
   
   # set path for GOF
   gof.path <- paste0(res.path,tie.type,'/GOF/')
   if (!dir.exists(gof.path)) 
      dir.create(gof.path, recursive=T)
   
   
   ### WAIC and DIC ###
   
   # set path for ICs
   ic.path <- paste0(gof.path, 'IC/')
   if (!dir.exists(ic.path)) 
      dir.create(ic.path, recursive=T)
   
   # get pointwise log-likelihood
   
   ll.mat <- p.wise.ll.model(fit)
   lambda.dat <- as.matrix(fit$fit, pars='lambda')
   
   y <- fit$data$long.dat[,response]
   ll.mat <- apply(lambda.dat,1,p.wise.ll,y=y) %>% t
   
   # check log-likelihood matrix
   if (sum(is.nan(ll.mat))>0)  
      stop('NaNs resulted in calculating log-likelihood')
   if (sum(is.infinite(ll.mat))>0)
      stop('Infinite log-likelihood')
   if (sum(is.na(ll.mat))>0)
      stop('Log-likelihood matrix has NAs')
   
   waic.stat <- waic(ll.mat)
   saveRDS(waic.stat, paste0(ic.path, 
                             'waic.',
                             model.no,
                             '.rds'))
   
   
   # Calculate DIC   
   dic.stat <- get.dic(fit, ll.mat)
   saveRDS(dic.stat, paste0(ic.path,
                            'dic.',
                            model.no,
                            '.rds'))
   
   # drop objects due to memory consumption
   rm(lambda.dat,ll.mat,waic.stat,dic.stat)
   gc()
   
   
   # Calculate Proportional Reduction in Error (baseline: Random Mixing Model)
   
   pre <- get.pre(fit.dat=fit,
                  mu='lambda',
                  logged=T,
                  n=300,
                  stat=median,
                  baseline='N.RM')
   saveRDS(pre,paste0(ic.path,
                      'pre.',
                      model.no,
                      '.rds'))
                  
   
   
   ### Compare Distances and Responses ###
   
   if (model.no %in% c('N.ER','N.RM')==F) {
      # generate path
      dist.path <- paste0(gof.path,'Dist/')
      if (!dir.exists(dist.path))
         dir.create(dist.path, recursive=T)
      
      
      # get stimuli positions
      pos.j.mcmc <- as.data.table(fit$fit, pars='pos_j') 
      pos.j.mcmc[,draw:=1:nrow(pos.j.mcmc)]
      j.draws <- melt(pos.j.mcmc, id.vars='draw')
      j.draws[,ind:=gsub('(.*\\[)([[:digit:]]+)(.*)','\\2',variable) %>%
                 as.numeric]
      j.draws[,dim:=gsub('(.*,)([[:digit:]])(.*)','\\2',variable) %>%
                 as.numeric]
      j.draws[,variable:=NULL]
      j.draws <- dcast(j.draws, draw + ind~dim, value.var='value')
      j.draws <- as.matrix(j.draws[,-2])
      
      n.dim <- ncol(j.draws)-1
      
      # get individual positions
      pos.i.mcmc <- as.data.table(fit$fit, pars='pos_i') 
      pos.i.mcmc[,draw:=1:nrow(pos.i.mcmc)]
      i.draws <- melt(pos.i.mcmc, id.vars='draw')
      i.draws[,ind:=gsub('(.*\\[)([[:digit:]]+)(.*)','\\2',variable) %>%
                 as.numeric]
      i.draws[,dim:=gsub('(.*,)([[:digit:]])(.*)','\\2',variable) %>%
                 as.numeric]
      i.draws[,variable:=NULL]
      i.draws <- dcast(i.draws, draw + ind ~ dim, value.var='value')
      i.draws <- as.matrix(i.draws[,-2])
      
      jj.d <- j.draws[,1]
      ii.d <- i.draws[,1]
      
      d.list <- lapply(1:n.draws, function(w) {
         
         pj <- j.draws[jj.d==w,2:(n.dim+1)]
         pi <- i.draws[ii.d==w,2:(n.dim+1)]
         
         return(vec_pdist(pi,pj))
         
      })
      
      # check results for 100 randomly sampled draws/inds/items
      ss <- sample(1:n.draws, 100, replace=F)
      
      for (s in 1:length(ss)) {
         
         s1 <- sample(1:n.ind,1)
         s2 <- sample(1:n.ite,1)
         
         ii <- i.draws[i.draws[,1]==s,]
         jj <- j.draws[j.draws[,1]==s,]
         
         d <- sqrt(sum((ii[s1,-1]-jj[s2,-1])^2))
         
         if(abs(d.list[[s]][s1,s2]-d) > .00000001) {
            
            cat(paste0('\nd: ',d))
            cat(paste0('\nd.list:', d.list[[s]][s1,s2]))
            
            stop('Something wrong with distance calculations!')
         }
      }
      
      post.m.dist <- Reduce('+',d.list)/length(d.list)
      message('Saving Posterior Mean Distances ...')
      saveRDS(post.m.dist, paste0(dist.path,
                                  'p.mean.dist.',
                                  model.no,
                                  '.rds'))
      
      dist.df <- melt(post.m.dist) %>% data.table %>%
         setnames(c('id','item','dist'))
      dist.df <- merge(dist.df, fit$data$long.dat, by=c('id','item'))
      dist.df[,response:=factor(response, labels=c('0',
                                                   '1',
                                                   '2-5',
                                                   '6-10',
                                                   'more than 10'))]
      l.vec <- sub('acq|trt|','',fit$data$item.sum[,labs])
      l.vec <- paste0(toupper(substr(l.vec,1,1)),
                      substr(l.vec,2,nchar(l.vec)))
      
      dist.df[,item:=factor(item, labels=l.vec)]
      
      
      message('Plotting Distances and Responses ...')
      pdf(paste0(dist.path,'pr.plot.',model.no,'.pdf'), 
          width=12,
          heigh=10)
      ggplot(dist.df, aes(x=response, y=dist)) + 
         geom_jitter(size=.8, 
                     col=viridis(1,begin=.5), 
                     width=.2,
                     alpha=.2)+
         # stat_boxplot(geom='errorbar', width=.5)+
         geom_boxplot(outlier.size=0,
                      outlier.shape=NA,
                      coef=0,
                      col=viridis(1,begin=.2),
                      alpha=.1,
                      width=.6) + 
         theme_bw() + 
         facet_wrap(~item, scale='free') +
         ggtitle('Posterior Mean Distances and Observed Responses')
      dev.off()
      
      rm(dist.df,d.list)
      gc()
   }
   ## Posterior Predictive Checks ---------------------------------------------------
   
   ppc.path <- paste0(gof.path,'/PPC/')
   if(!dir.exists(ppc.path))
      dir.create(ppc.path, recursive=T)
   
   
   ppc.df <- gen.ppc.df(fit,
                        quants=c(.025,.975),
                        'lambda',
                        logged=T,
                        n.samps=500,
                        compare.to='N.RM')
   
   pdf(paste0(ppc.path,'ppc.comp.',model.no,'.pdf'),
       width=12,
       height=10)
   ggplot(ppc.df$post.df, aes(x=fact.value)) +
      geom_col(data=ppc.df$dat.df,
               aes(y=probs),
               col='white',
               fill='grey',
               alpha=.5) +
      facet_wrap(~new.labs) +
      geom_errorbar(aes(ymin=V1, ymax=V2, col=gg, group=gg),
                    width=.3,
                    position=position_dodge(.9),
                    size=1)+
      scale_color_manual(name='Models',
                         values=c('red','blue'))+
      labs(x='Number of Ties', y='Proportion')+
      theme_bw() +
      ggtitle(paste0('Posterior Predictive Checks (Model: ',
                     model.no,')'))
   dev.off()
   
   pdf(paste0(ppc.path,'ppc.',model.no,'.pdf'),
       width=12,height=10)
   ggplot(ppc.df$post.df[gg=='Baseline'], aes(x=fact.value)) +
      geom_col(data=ppc.df$dat.df,
               aes(y=probs),
               col='white',
               fill=viridis(1,begin=.5,end=.5),
               alpha=.5) +
      facet_wrap(~new.labs) +
      geom_errorbar(aes(ymin=V1,ymax=V2),
                    col=viridis(1),
                    width=.2,
                    position=position_dodge())+
      labs(x='Number of Ties', y='Proportion') +
      theme_bw() +
      ggtitle(paste0('Posterior Predictive Checks (Model: ',
                     model.no,')'))
   dev.off()
   
}
# ## Compare Distances to Other Measures in the GSS --------------------------------
# 
# 
# # load data if necessary
# if (!exists('gss.06.valid')) {
#    gss.06.valid <- fread(paste0(data.path,'gss.2006.csv'))
# }
# 
# # variables from GSS used
# d.vars <- c('partyid',
#             'wrkstat',
#             'sex',
#             'race',
#             'coninc',
#             'attend',
#             'educ',
#             'age',
#             'marital',
#             'degree',
#             'polviews',
#             'size')
# 
# o.vars <- c('RACOPEN',
#             'RACDIF1',
#             'RACDIF2',
#             'RACDIF3',
#             'RACDIF4',
#             'LIVEWHTS',
#             'LIVEBLKS',
#             'MARWHT',
#             'MARBLK',
#             'MARASIAN',
#             'MARHISP',
#             'CLOSEBLK',
#             'CLOSEWHT')
# o.vars <- tolower(o.vars)
# # predictors
# var.list <- c('id',d.vars,o.vars)
# 
# # merge datasets
# d.dat <- gss.06.valid[,..var.list]
# ids <- fit$data$ind.sum
# a.dat <- merge(ids, d.dat, by.x='old.id', by.y='id')
# 
# d.black <- dist.df[item=='Black']
# d.black <- merge(d.black, a.dat[,list(new.id,marblk,closeblk,liveblks,race)],
#                  by.x='id', by.y='new.id', all=T)
# d.black[,mar:=recode(marblk,
#                      'strongly favor'=1,
#                      'favor'=2,
#                      'neither favor nor oppose'=3,
#                      'oppose' = 4,
#                      'strongly oppose'=5)]
# # ggplot(d.black, aes(x=closeblk,y=dist)) + 
# #    geom_jitter() +
# #    geom_smooth(se=F)
# lm(mar~dist, data=d.black)%>%summary
# lm(mar~dist+factor(response), data=d.black)%>%summary
# lm(mar~dist+factor(response)+factor(race), data=d.black)%>%summary
# 
# lm(closeblk~dist, data=d.black)%>%summary
# lm(closeblk~dist+factor(response), data=d.black)%>%summary
# lm(closeblk~dist+factor(response)+factor(race), data=d.black)%>%summary
# 
# lm(closeblk ~ dist, data=d.black)%>%summary
# lm(closeblk ~ dist , data=d.black)%>%summary
# 

###################################################

# beta.df <- gen.reff.df(fit, what='beta',
#                        item=T,
#                        stat=median,
#                        error=c('q','posterior'))
# 
# beta.df[, labs:=factor(labs,
#                        levels=beta.df[variable=='value',labs][order(-value)])]
# beta.draws <- beta.df[variable %in% c('value','hi','lo')==F]
# beta.stats <- dcast(beta.df[variable %in% c('value','hi','lo')],
#                     labs ~ variable,
#                     value.var='value')
# 
# plot.beta <- ggplot(beta.draws, aes(x=labs,y=value)) +
#    geom_hline(yintercept=0,
#               col='red',
#               linetype=2,
#               size=.6)+
#    geom_jitter(col=p.col[2],alpha=.15,
#                size=1) +
#    theme_bw() +
#    # geom_boxplot()+
#    geom_errorbar(data=beta.stats,
#                 aes(ymin=lo,ymax=hi),
#                 col='black',
#                 width=.5,
#                 alpha=.8) +
#    labs(x='Groups', y=expression(log(beta)))
