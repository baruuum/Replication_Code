###############################################################################
##                                                                           ##
##  Generating Plots from Models                                             ##
##                                                                           ##
###############################################################################

# check base directory
if (!exists('wd.base'))
   stop('Specify wd.base')

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

# load functions
if (!exists('gen.agg.trends') | !exists('inv.logit'))
      source(paste0(code.path, '0.Functions.R'))

## Calculate Posterior Probs --------------------------------------------------

message('\n\nStart Plotting Results ------------------------')

# load fitted models
stan.res <- lapply(1:4, function(w) {
   readRDS(
      paste0(stan.path, '/domain_', w, '/d', w, '.linear.bn.rds')
   )
})


# get mu_gammas 
# note: rescaled such that zero will correspond to 1972 
#       and one unit increase corresponds to 2 decades
mu.gamma.res <- lapply(
   1:4, function (w) {
      d <- as.data.table(stan.res[[w]] , pars = 'mu_gamma')
      d[, `mu_gamma[4]` := `mu_gamma[4]` * 2]
      d[, `mu_gamma[5]` := `mu_gamma[5]` * 2]
      d[, `mu_gamma[6]` := `mu_gamma[6]` * 2]
      d[, `mu_gamma[1]` := `mu_gamma[1]` - 1.1 * `mu_gamma[4]`]
      d[, `mu_gamma[2]` := `mu_gamma[2]` - 1.1 * `mu_gamma[5]`]
      d[, `mu_gamma[3]` := `mu_gamma[3]` - 1.1 * `mu_gamma[6]`]
      d[, domain := w]
   }
) %>% 
   rbindlist 

# save data for later use
saveRDS(mu.gamma.res, paste0(data.path,'mu.gamma.res.rds'))


## Plots for Linear Trends ----------------------------------------------------

message('Plotting Linear Trends ...')

### Coefficients Plot

# reshape data to long-form
mu.gamma.res <- melt(mu.gamma.res, id.vars = 'domain') 

# quantiles to plot
qs <- c(.025, .1, .5, .9, .975)

# coefficient names
coef.names <- cbind(
   1:6,
   c('Intercept',
     'Independent',
     'Democrat',
     'Time',
     'Time %*% Independent',
     'Time %*% Democrat'),
   c('Intercept',
     'Independent',
     'Democrat',
     'Time',
     'Time $\times$ Independent',
     'Time $\times$ Democrat')
)

# dataset of quantiles
mu.gamma.df <- mu.gamma.res[
   
   , as.list(quantile(value, probs = qs)), by = list(domain, variable)
   
   ] %>% 
   
   # get coefficietn number
   mutate(
      variable = gsub('(^.*\\[)([0-9])(\\])', '\\2', variable)
   ) %>%
   
   # match coefficient number with coef. name
   mutate(variable = factor(
      variable,
      levels = coef.names[, 1],
      labels = coef.names[, 2])
   ) %>%
   
   # add domain names
   mutate(domain = factor(
      domain,
      levels = 1:4,
      labels = c('Economic',
                 'Civil Rights',
                 'Moral',
                 'Foreign Policy / Security')
      )
   ) %>%
   
   # add new column names
   setnames(c('domain', 'variable', paste0('q', qs)))


# dummy data to control axes of facets in ggplot
dummy.dat <- mu.gamma.df[
   , list(min(q0.025), max(q0.975)), by = list(domain, variable)
][
   domain %in% c('Economic','Civil Rights', 'Moral'), V1 := min(V1)
][
   domain %in% c('Economic','Civil Rights', 'Moral'), V2 := max(V2)
][
   , V3 := 0
] %>%
   setnames(c('domain', 'variable','q0.025','q0.975','q0.5'))

# store coefficients plot for latter
coef.plot <- ggplot(mu.gamma.df, aes(x = variable, y = q0.5)) + 
   geom_hline(yintercept = 0, linetype = 3, col = 'grey50', size = .4) + 
   geom_point(size = 1.2) + 
   geom_linerange(
      aes(ymin = q0.025, ymax = q0.975), size = .3
   ) + 
   geom_linerange(
      aes(ymin = q0.1, ymax = q0.9), size = .8
   ) + 
   geom_blank(data = dummy.dat, 
              aes(ymin = q0.025, ymax = q0.975)
   ) + 
   coord_flip() + 
   facet_wrap( ~ domain, nrow = 1, scales = 'free_x') + 
   scale_x_discrete(
      limits = rev(levels(mu.gamma.df$variable)),
      labels = parse(
         text = rev(levels(mu.gamma.df$variable))
      )
   ) + 
   labs(x = '', y = '') + 
   theme_bw() +
   theme(
      axis.text = element_text(size = 10), 
      strip.text = element_blank(), 
      strip.background = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_rect(color = 'black')
   )


### Linear Trends

# load data feeded to STAN
stan.dat <- readRDS(
   paste0(stan.path, '/domain_', 3, '/d', 3, '.linear.stan.dat.rds')
)

# # time matrix
time.mat <- cbind(1, sort(unique(stan.dat$x$c.year)))

# generate predited (mean) trends
trend.list <- lapply(
   stan.res, function (w) {

      # gen mean trends
      z <- gen.agg.trends(w, time.mat, mean.only = T, coefs = T) 
      
      # number of draws
      dd <- length(z)
      
      # number of time points measured
      tt <- nrow(z[[1]][[1]])
      
      # get predicted mean trends
      res <- lapply(z, `[[`, 'summary') %>%
         do.call(rbind, .) %>%
         cbind(draw = rep(1:dd, each = tt))
      
      # get coefficieints
      coefs <- lapply(z, `[[`, 'coefs') %>%
         do.call(rbind, .) %>%
         cbind(draw = rep(1:dd, each = 3))
      
      return(list(trends = res, coefs = coefs))

   }
) 

# add domain numbers and reshape into data.table
agg.trend.dat <- lapply(
   1:4, function (w) {
         cbind(trend.list[[w]]$trends, domain = w)
      }
   ) %>%
   do.call(rbind, .) %>%
   data.table

# generate data to plot
df <- agg.trend.dat[
   # get posterior median
   , lapply(.SD, median), 
   by = list(time, domain), 
   .SDcols = c('reps','inds','dems')
] %>%
   
   # reshaped into long-format
   melt(id.vars = c('time','domain')) %>%
   
   # set names
   setnames(c('variable', 'value'), c('pid', 'median'))

# turn variables into factors to control plotting order
df[
   ,  domain := factor(
      domain,
      levels = 1:4,
      labels = c('Economic',
                 'Civil Rights',
                 'Moral',
                 'Foreign Policy')
   )
][
   , pid := factor(
      pid,
      levels = c('reps','inds','dems'),
      labels = c('Republican',
                 'Independent',
                 'Democrat')
   )
]

# generate plot of agg. mean trends (posterior median)
l.trends.plot <- 
   ggplot(df, aes(x = time, y = median, linetype = pid)) +
   geom_line(size = .6) +
   scale_linetype_manual(name = 'Partisanship',
                         values = c(
                            'Republican' = 2,
                            'Independent' = 3,
                            'Democrat' = 1
                         )) +
   theme_bw() +
   facet_wrap( ~ domain, ncol = 4) +
   scale_y_continuous(
      name = '% Liberal',
      limits = c(.2, .7),
      breaks = seq(.2, .8, .2)
   ) +   
   scale_x_continuous(
      limits = c(-2.2, 2.2),
      breaks = c(-2.2, 0, 2.2),
      labels = c("'72", "'94", "'16")
   ) +
   theme_bw() +
   theme(
      axis.title = element_blank(),
      axis.text = element_text(size = 10),
      strip.text.y = element_blank(),
      strip.text.x = element_text(size = 10,
                                  hjust = .1,
                                  face = 'bold'),
      strip.background = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_rect(color = 'black'),
      legend.position = 'none'
   )

# add legends
text.dat <- data.table(
   time = rep(-2, 3),
   pid = unique(df$pid),
   median = c(.39, .475, .545),
   domain = unique(df$domain)[1]
)
text.labs <- data.table(
   labs = paste0(unique(df$pid), 's'),
   angles = c(-17.5, 5, 11)
)

# add text to trend plot
l.trends.plot <- l.trends.plot + 
   geom_text(data=text.dat, 
             aes(label = text.labs$labs,
                 angle = text.labs$angles),
             hjust = 0,
             size = 3)

   
### Combine `coef.plot` and `l.trends.plot`
   
# adjust widths of plots
coef.grob <- ggplotGrob(coef.plot)
trend.grob <- ggplotGrob(l.trends.plot)
trend.grob$widths <- coef.grob$widths

pdf(paste0(graphics.path, 'Figure7.pdf'), width = 8, height=5)
grid.arrange(trend.grob, coef.grob)
grid.text("% Liberal", 
          x =.1, 
          y =.74, 
          rot = 90, 
          gp = gpar(fontsize = 12))
dev.off()


## Plots for Non-linear Trends & Issue-wise Partisan Differences -------------

message('Plotting Non-linear Trends ...')

trends <- lapply(1:4, function(w) {
   
   rbind(
      readRDS(
         paste0(stan.path, '/domain_', w, '/d', w, '.agg.trends.rds')
      )[
         , domain := w
      ][
         , issue := 'Aggregate'
      ],
      
      readRDS(
         paste0(stan.path, 'domain_', w, '/d', w, '.i.trends.rds')
      )[
         , domain := w
      ]
   )

})  %>% rbindlist

# add domain names
trends[
   , domain := factor(
      domain,
      levels = 1:4,
      labels = c('Economic',
                 'Civil Rights',
                 'Moral',
                 'Foreign Policy / Security')
   )
]
# add "s" to the pid defs.
levels(trends$pid) <- paste0(levels(trends$pid), 's')
  

# define labels for the partisan groups and their positions
text.dat <- data.table(
   time = rep(.2, 3) + c(.2, 0, .3),
   pid = unique(trends$pid)[1:3],
   m = c(.34, .49, .58),
   domain = unique(trends$domain)[1]
)
text.labs <- data.table(
   labs = unique(trends$pid)[1:3],
   angles = c(-24, 5, 20)
)

# merge issue-wise trends with original data to get missing pattern
m.pattern <- unique(long.dat[,.(c.year,data.source, i.vars.label)])
i.trends <- trends[pid == 'Diffs']
i.trends <- merge(i.trends, m.pattern, 
                  by.x = c('time','issue'),
                  by.y = c('c.year','i.vars.label'))

# aggregate trends
agg.preds <- ggplot(trends[issue == 'Aggregate'],
                aes(
                   x = time,
                   y = m,
                   linetype = pid,
                   group = interaction(pid, domain)
                )) +
   geom_line(size=.6) +
   geom_text(data=text.dat,
             aes(label = text.labs$labs,
                 angle = text.labs$angles),
             hjust = 0,
             size = 3) +
   scale_linetype_manual(name='Partisanship',
                         values=c('Republicans'=2,
                                  'Independents'=3,
                                  'Democrats'=1)) +
   theme_bw() +
   facet_wrap( ~ domain, nrow = 1) +
   scale_y_continuous(name = '% Liberal\n',
                      limits = c(.2, .7),
                      breaks = seq(.2, .8, .2)) +
   scale_x_continuous(
      name = '',
      limits = c(-2.2, 2.2),
      breaks = c(-2.2, 0, 2.2),
      labels = c("'72", "'94", "'16")
   ) +
   theme_bw() +
   theme(
      axis.title.x = element_blank(),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      strip.text.y = element_blank(),
      strip.text.x = element_text(size = 10,
                                  hjust = .1,
                                  face = 'bold'),
      strip.background = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_rect(color = 'black'),
      legend.position = 'none'
   )


# partisan difference (issue-level)
i.preds <- ggplot(i.trends,
             aes(x = time, 
                 y = m,
                 group = issue)) +
   geom_hline(yintercept = 0, linetype = 2, col = 'grey50', size = .3)+
   geom_line(size=.3, alpha = .8) + 
   facet_wrap(~ domain, nrow = 1) +
   theme_bw() + 
   scale_y_continuous(
      name = '% Liberal (Dem. - Rep.)\n',
      limits = c(-.05,.5),
      breaks = seq(0,.5,.25)
   ) +
   scale_x_continuous(
      name = '',
      limits=c(-2.2,2.2),
      breaks=c(-2.2,0,2.2),
      labels=c("'72","'94","'16")
   ) +
   theme(
      axis.text.y=element_text(size=10),
      axis.text.x=element_text(size=10),
      strip.text=element_blank(),
      strip.background=element_blank(),
      panel.grid=element_blank(),
      panel.border=element_rect(color='black')
   )

# adjust widths of the plot
g.agg.preds <- ggplotGrob(agg.preds)
g.i.preds <- ggplotGrob(i.preds)
g.agg.preds$widths <- g.i.preds$widths

# print plot
pdf(paste0(graphics.path, 'Figure8.pdf'), width = 8, height = 5)
grid.arrange(g.agg.preds, g.i.preds)
dev.off()
