
###############################################################################
##                                                                           ##
## Adjusting for Demographic Chanes (Appendix)                               ##
##                                                                           ##
###############################################################################


## Basic Setup ----------------------------------------------------------------

if (!exists('wd.base'))
   stop('Specify wd.base')
if (!exists('n.cores'))
   stop('Specify the number of cores for parallel computing (n.cores)!')

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
if (!exists('get.se.lincom'))
   source(paste0(code.path, '0.Functions.R'))

## Read in Data and Prepare for Analysis --------------------------------------

# read data
nes.dat <- readRDS(
   paste0(data.path, 'nes.merged.n.recoded.72.16.rds')
)
nes.var.info <- readRDS(paste0(data.path, 'nes.var.sum.rds'))
gss.dat <- readRDS(paste0(data.path, 'gss.recoded.rds'))
gss.var.info <- readRDS(paste0(data.path, 'gss.var.sum.rds'))

# get names of moral variables
nes.moral <- nes.var.info[i.vars.class == 3, i.vars.label]
gss.moral <- gss.var.info[i.vars.class == 3, i.vars.label]
moral.vars <- c(nes.moral, gss.moral)

# differentiate between GSS and NES abortion item
moral.vars[duplicated(moral.vars)]  <- 'abortion.gss'
moral.vars[moral.vars == 'abortion'] <- 'abortion.nes'

# summary table
var.sum <- data.table(
   i.vars.label = moral.vars,
   gss = c(rep(0,length(nes.moral)), rep(1, length(gss.moral)))
)

# change abortion var name in datasets
setnames(gss.dat, 'abortion', 'abortion.gss')
setnames(nes.dat, 'abortion', 'abortion.nes')


### prepare variables

# age
age.cuts <- c(30, 45, 60)

gss.dat[
   , age.cat := findInterval(age, age.cuts, left.open = T)
][
   , age.1 := as.numeric(age.cat == 1)
][
   , age.2 := as.numeric(age.cat == 2)
][
   , age.3 := as.numeric(age.cat == 3)
]

nes.dat[
   , age.cat := findInterval(age, age.cuts, left.open = T)
][
   , age.1 := as.numeric(age.cat == 1)
][
   , age.2 := as.numeric(age.cat == 2)
][
   , age.3 := as.numeric(age.cat == 3)
]

# dummies for income
gss.dat[
   , fam.inc.2 := as.numeric(fam.inc == 2)
][
   , fam.inc.3 := as.numeric(fam.inc == 3)
]

nes.dat[
   , fam.inc.2 := as.numeric(fam.inc == 2)
][
   , fam.inc.3 := as.numeric(fam.inc == 3)
]


## Predict %Liberal with Logistic Regression ----------------------------------

# predictors except for partisanship
predictors <- c('educ','fam.inc.2', 'fam.inc.3',
                'c.south','female','black',
                'age.1', 'age.2', 'age.3')
# years measured
years = c(nes.dat$year %>% unique, gss.dat$year %>% unique) %>% sort

# get plotting order used in the descriptive trends
p.order <- readRDS(
   paste0(data.path, 'p.order.rds')
)

# make cluster and register
cl <- makeCluster(n.cores)
registerDoParallel(cl)
                  
# fit regressions
preds <- foreach (ii = 1:length(moral.vars), 
                  .combine = 'rbind',
                  .packages = c('data.table','dplyr')) %dopar% { 

   issue <- moral.vars[ii]
   
   # to which data set does issue belong?
   if (var.sum[i.vars.label == issue]$gss == 0) {
      dat <- nes.dat[
         , c(issue, predictors, 'pid.3', 'weight','year'), with = F
         ]
   } else {
      dat <- gss.dat[
         , c(issue, predictors, 'pid.3', 'weight','year'), with = F
         ]
   }
   
   # get missing pattern by year
   mis.pattern <- dat[, sum(!is.na(get(issue))), by=year]
   
   # use only years where issue is measured
   dat <- dat[year %in% mis.pattern[V1!=0,year]]
   
   # create dummy for below-midpoint response
   dat[, y := as.numeric(
      get(issue) < (max(get(issue),na.rm=T) + min(get(issue), na.rm=T))/2)]
   
   # formula for glm
   f <- paste0('y ~ factor(pid.3) * factor(year) +', 
               paste0(predictors, '*factor(year)', collapse = '+'))
   
   # mean standardize predictors (so that prediction is at means)
   m.predictors <- dat[, colMeans(.SD, na.rm=T) ,.SDcols = predictors]
   for (vv in seq_along(m.predictors)) {
      nn <- names(m.predictors)[vv]
      dat[, (nn) := get(nn) - m.predictors[vv]]
   }
   
   # fit model
   fit <- glm(as.formula(f), 
              family = quasibinomial(link = 'logit'), 
              data=dat, 
              weights=weight)
   
   # The following might be used as well:
   # survey::svyglm(as.formula(f), 
   #   design = survey::svydesign(id = ~1, weights = ~weight, data = dat),
   #   family = binomial(link = 'logit'))
   
      # get reg coefficients
   coefs <- coef(fit)
   c.names <- names(coefs)
   # get covariance matrix
   Sigma <- vcov(fit)
   
   # get starting year
   min.y <- fit$model[, 'factor(year)'] %>% 
      as.character %>% 
      as.numeric %>% 
      min
   
   # names of year dummy names
   year.str <- grep('^factor\\(year\\)[0-9]*$', c.names, value=T)
   # names of year*pid interction names
   inter.str <- grep('factor\\(pid\\.3\\)3\\:', c.names, value=T)
   
   # get intercept
   const <- coefs[c.names == '(Intercept)']
   
   # predicted Democratic trend
   d.tmp <- c(const=0, coefs[
      grepl('^factor\\(year\\)[0-9]*$', c.names)
      ])
   dems <- d.tmp + const
   
   # predicted Republican trend
   r.const <- coefs[c.names == 'factor(pid.3)3']
   r.tmp <- c(const = 0, coefs[
      grepl('factor\\(pid\\.3\\)3\\:', names(coefs))])
   reps <- dems + r.tmp + r.const
   
   # standard error of predictions (Democrats)
   se.const <- Sigma[1,1] %>% sqrt
   se.dems <- sapply(year.str, function(w) {
      get.se.lincom(Sigma, c('(Intercept)', w))
   }) %>%
      c(`(Intercept)`=se.const, .)
   
   # standard error of predictions (Republicans)
   tmp <- c('(Intercept)', 'factor(pid.3)3')
   tmp.list <- mapply(c, year.str, inter.str, SIMPLIFY = F) %>% 
      lapply(`c`, tmp) %>%
      `c`(list(tmp), .)
   se.reps <- sapply(tmp.list, get.se.lincom, Sigma=Sigma)
   
   # generate outcome data
   res <- data.table(year = gsub('factor\\(year\\)','',names(dems)),
                     dems,
                     se.dems,
                     reps,
                     se.reps,
                     i.vars.label = issue)
   
   res <- res[
      year == 'const', year := as.character(min.y)
   ][
      , year := as.numeric(year)
   ]
   
   return(res)
      
}

# stop cluster
stopCluster(cl)


# reshape into long format 
pred.dat <- melt(preds, 
            id.vars = c('year','i.vars.label'),
            measure.vars = list(c('dems','reps'),
                                c('se.dems','se.reps')),
            value.name=c('xb','se'),
            variable.name = 'pid')

# change pid into factor
pred.dat[
   , pid := factor(pid,
                   levels = 1:2,
                   labels = c('Democrats','Republicans'))
][ # transform xb into predicted probabilities at means
   , pr.agree := inv.logit(xb)
][ # lower bound of 95% CI
   , lb := inv.logit(xb - 1.96*se)
][ # upper bound
   , ub := inv.logit(xb + 1.96*se)
][ # adjust plotting order by changing factor levels
   ,i.vars.label := factor(
      i.vars.label, 
      levels = p.order$moral)
]%>%
   setorder(i.vars.label, year)

pdf(
   paste0(graphics.path, 'FigureS5.pdf'),
   width = 12,
   height = 2.2*7
)
print(
   ggplot(pred.dat, 
          aes(x = year,
              y = pr.agree,
              col = pid,
              fill = pid)
      ) +
      geom_line(size = .5) +
      geom_point() +
      geom_ribbon(
         aes(ymin = lb, ymax = ub),
         linetype = 'blank',
         alpha = .3,
         show.legend = F
      )+
      scale_color_manual(
         name = 'Partisanship',
         values = c(
            'Republicans' = 'red',
            'Independents' = 'grey',
            'Democrats' = 'blue'
         )
      ) +
      scale_fill_manual(
         name = 'Partisanship',
         values = c(
            'Republicans' = 'red',
            'Independents' = 'grey',
            'Democrats' = 'blue'
         )
      ) +
      scale_y_continuous(name = '% Liberal',
                         limits = c(0, 1),
                         breaks = c(.2, .5, .8)) +
      scale_x_continuous(
         limits = c(1972, 2016),
         breaks = c(1972, 1994, 2016),
         labels = c("'72", "'94", "'16")
      ) +
      labs(x = 'Time') +
      theme_bw() +
      theme(
         axis.title = element_blank(),
         axis.text.y = element_text(size = 12),
         axis.text.x = element_text(size = 12),
         strip.text.y = element_blank(),
         strip.text.x = element_text(size = 12,
                                     hjust = .1,
                                     face = 'bold'),
         strip.background = element_blank(),
         panel.grid = element_blank(),
         panel.border = element_rect(color = 'black'),
         legend.position = c(.25, .06),
         legend.text = element_text(size = 12),
         legend.title = element_text(size = 12, face = 'bold'),
         plot.margin = unit(c(0, .1, .05, .1),
                            units = 'cm')
      ) +
      facet_wrap( ~ i.vars.label, ncol = 6)
)
dev.off()


#### END OF CODE ####