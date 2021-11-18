

library(dplyr)
library(data.table)
library(dtplyr)
library(ggplot2)
library(plotly)
library(rstan)
library(viridis)

rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())




### Setting Paths
if (!exists('wd.base')) {
   wd.base <- 'C:/Users/struxture/Dropbox/###_work_current/#_weak_ties/analysis/'
   code.path <- paste0(wd.base,'##_code/')
   mcode.path <- paste0(code.path,'Model_Codes/')
   rawdata.path <- paste0(wd.base,'##_rawdata/')
   data.path <- paste0(wd.base,'data/')
   res.path <- paste0(wd.base,'results/')
   output.path <- paste0(wd.base,'output/')
}

source(paste0(code.path,'0_functions.R'))

tie.type <- 'acq.core.s'
model.no <- 'N2.euc'


fit <- readRDS(paste0(res.path,
                     tie.type,'/',
                     model.no,'/',
                     paste0(tie.type,'.',
                            model.no,
                            '.rds'
                           )
               )
       )


# ## WAIC?
library(loo)
lambda.dat <- as.data.table(fit$fit, pars='lambda')
y <- fit$data$long.dat[,response]
ll.mat <- apply(lambda.dat,1,p.wise.ll,y=y) %>% t
# check for NaNs
if (sum(is.nan(ll.mat))>0)  
   stop('NaNs resulted in calculating log-likelihood')

w.stat <- waic(ll.mat)
loo.stat <- loo(ll.mat)

p.col <- viridis(2, begin=0, end=.5)
pos.j <- gen.pos.df(fit=fit, 
                    what='pos_j', 
                    item=T, 
                    stat=mean,
                    sd=T, 
                    new.labs='abbr')

J.r.pos.j <- rotate.pos(pos.j[,list(dim.1,dim.2,dim.3)], return.matrix=T)
pos.j <- cbind(pos.j, J.r.pos.j$new.pos)

pos.i <- gen.pos.df(fit=fit,
                    what='pos_i',
                    item=F)
J.r.pos.i <- as.matrix(pos.i[,list(dim.1,dim.2,dim.3)]) %*% J.r.pos.j$R %>%
   data.table %>%
   setnames(c('r.dim.1','r.dim.2','r.dim.3'))
pos.i <- cbind(pos.i,J.r.pos.i)


plot_ly(pos.i,
        x = ~ r.dim.1,
        y = ~ r.dim.2,
        z = ~ r.dim.3) %>%
   layout(scene = list(xaxis=list(range=c(-2,2)),
                       yaxis=list(range=c(-2,2)),
                       zaxis=list(range=c(-2,2))
                  )
   ) %>%
   add_markers(marker=list(size=5,
                           color= ~ r.dim.3,
                           colorscale='Viridis',
                           showscale=T,
                           opacity=.3)
   ) %>%
   add_text(data=pos.j,
                   inherit=T,
                   text = ~labs)


I.r.pos.i <- rotate.pos(pos.i[,list(dim.1,dim.2,dim.3)], return.matrix=T)
r.i.pos <- data.table(I.r.pos.i$new.pos) %>%
           setnames(c('i.dim.1','i.dim.2','i.dim.3'))
pos.i <- cbind(pos.i, r.i.pos)

I.r.pos.j <- as.matrix(pos.j[,list(dim.1,dim.2,dim.3)]) %*% I.r.pos.i$R %>%
   data.table %>%
   setnames(c('i.dim.1','i.dim.2','i.dim.3'))
pos.j <- cbind(pos.j,I.r.pos.j)


plot_ly(pos.i,
        x = ~ i.dim.1,
        y = ~ i.dim.2,
        z = ~ i.dim.3) %>%
   layout(scene = list(xaxis=list(range=c(-2,2)),
                       yaxis=list(range=c(-2,2)),
                       zaxis=list(range=c(-2,2))
   )
   ) %>%
   add_markers(marker=list(size=5,
                           color= ~ i.dim.3,
                           colorscale='Viridis',
                           showscale=T,
                           opacity=.3)
   ) %>%
   add_text(data=pos.j,
            inherit=T,
            text = ~labs)
# 2D PLOTS!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
plot.j <- ggplot(pos.j, aes(x=dim.1, y=dim.2)) +
                  geom_segment(aes(x=dim.1-sd.1,
                                   xend=dim.1+sd.1,
                                   y=dim.2,
                                   yend=dim.2),
                               col='grey',
                               alpha=.7)+
                  geom_segment(aes(x=dim.1,
                                   xend=dim.1,
                                   y=dim.2-sd.2,
                                   yend=dim.2+sd.2),
                               col='grey',
                               alpha=.7)+
                  geom_point()+
                  geom_text(aes(label=labs),
                            hjust=0,
                         nudge_y=.15,
                         nudge_x=.02) +
                  theme_bw() +
                  labs(x='Dimension 1', y='Dimension 2')



plot.i <- ggplot(pos.i, aes(x=dim.1, y=dim.2)) +
   geom_point()+
   theme_bw() +
   labs(x='Dimension 1', y='Dimension 2')
plot.ij <- ggplot(pos.j, aes(x=dim.1, y=dim.2)) +
   geom_point(data=pos.i, col=p.col[2], alpha=.6) +
   geom_segment(aes(x=dim.1-sd.1,
                    xend=dim.1+sd.1,
                    y=dim.2,
                    yend=dim.2),
                col='grey',
                alpha=.7)+
   geom_segment(aes(x=dim.1,
                    xend=dim.1,
                    y=dim.2-sd.2,
                    yend=dim.2+sd.2),
                col='grey',
                alpha=.7)+
   geom_point()+
   geom_text(aes(label=labs),
             hjust=0,
             nudge_y=.15,
             nudge_x=.02) +
   theme_bw() +
   labs(x='Dimension 1', y='Dimension 2')

plot.ij

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


gamma.df <- gen.reff.df(fit, what='gamma',
                        item=F,
                        stat=median,
                        error=c('q','posterior'),
                        n.samps=300
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
                  col=p.col[2],
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


pred.y <- gen.pred.dist(fit.dat = fit, 
              mu = 'lambda',
              logged=T,
              n = 200)
pred.dat <- as.data.table(t(pred.y$pred))
pred.dat <- cbind(fit$data$long.dat[,2],pred.dat)
pred.dat <- melt(pred.dat, id.vars='item')
pred.dat.n <- pred.dat[, .N, by=list(item,variable,value)]
pred.dat.n[,props:=N/sum(N), by=list(item,variable)]
Qs <- pred.dat.n[,list(quantile(props, probs=.025),
                          quantile(props, probs=.975)), 
                 by=list(item,value)][
                    order(item,value)
                    ]
actual.y <- cbind(fit$data$long.dat[,2],pred.y$outcome)
actual.p <- actual.y[,.N, by=list(item,V2)]
actual.p[, probs:= N/sum(N), by='item']
actual.p[, fact.value:=factor(V2,                   
                              levels=c(0,1,2,3,4),
                              labels=c('0','1','2-5','6-10','more than 10')
                              )
         ]

                              

new.labs <- copy(fit$data$item.sum)
new.labs[, new.labs:=sub('acq|trt','',labs)]
new.labs[,new.labs:=paste0(toupper(substr(new.labs,1,1)),
                      substr(new.labs,2,nchar(new.labs))
)]
new.labs[, new.labs:=factor(new.labs)]

actual.p <- merge(actual.p, new.labs[,.(item.no,new.labs)], by.x='item',
                  by.y='item.no', all.x=T)

Qs <- merge(Qs,new.labs, by.x='item',by.y='item.no', all.x=T)
Qs[, fact.value:=factor(value, 
                   levels=c(0,1,2,3,4),
                   labels=c('0','1','2-5','6-10','more than 10')
                  )
   ]


ggplot(Qs, aes(x=fact.value)) +
   geom_col(data=actual.p,
              aes(y=probs),
            col='white',
            fill=p.col[2],
            alpha=.5)+
   geom_errorbar(aes(ymin=V1, ymax=V2),
                  col=p.col[1],
                 width=.3) +
   facet_wrap(~new.labs) +
   theme_bw()




