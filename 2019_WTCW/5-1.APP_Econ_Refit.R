###############################################################################
##                                                                           ##
##  Supplementary Analysis: Analyzing Only "Absolute" Opinion on Economic    ##
##                          Issues                                           ##
##                                                                           ##
###############################################################################


## Basic Setup ----------------------------------------------------------------

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

# load functions if necessary
if (!exists('gen.stan.summary'))
   source(paste0(code.path, '0.Functions.R'))

# stan options
options(mc.cores = n.cores)
n.warmup <- 2000
n.iter <- 3000
n.refresh <- 0

# generate directory to store results
app.path <- paste0(wd.base,'appendix/econ_refit/')
if (!dir.exists(app.path)) {
   
    message(paste0('Creating Directory ~/appendix/ into ',
                  wd.base)
           )
   
   dir.create(app.path, recursive=T)
}

# read in data
long.dat <- readRDS(
   paste0(data.path, 'long.comb.dat.rds')
)


# exclude "relative" issues
rel.item <- grep('FS\\.|immig|service',
                 unique(long.dat$i.vars.label),
                 value = T)

## Refit Econ Model ----------------------------------------------------------

message('\n\nRefitting Econ Model (Appendix) ...')

m.dat <- long.dat[i.vars.class == 1]
m.dat <- m.dat[i.vars.label %in% rel.item == F]

issue.map <- unique(m.dat$i.vars.no)
issue.map <- data.table(old = issue.map, new = 1:length(issue.map))
saveRDS(issue.map,
        paste0(app.path, 
               'econ.app.issuemap.rds')
)

# add new var.no's
m.dat <- merge(m.dat,
               issue.map,
               by.x = 'i.vars.no',
               by.y = 'old',
               all.x = T)

# stan data
stan.dat <- list()
stan.dat$N <- nrow(m.dat)
stan.dat$jj <- m.dat$new
stan.dat$J <- length(unique(stan.dat$jj))
stan.dat$y <- m.dat$pr.agree
stan.dat$x <- m.dat[, .(ind, dem, c.year, c.year.2)] %>% 
   mutate(ind.year   = ind * c.year,
          dem.year   = dem * c.year,
          ind.year.2 = ind * c.year.2,
          dem.year.2 = dem * c.year.2) %>%
   cbind(1,.)
stan.dat$K <- ncol(stan.dat$x)

# save data
saveRDS(stan.dat,
        paste0(
           app.path,
           'econ.app.stan.dat.rds'
        )
)

# load stan model
stan.model <- readRDS(
   paste0(stan.path, 'models/stan.bn.model.rds')
)

		
# sample
fit <- sampling(
   stan.model,
   data = stan.dat,
   pars = c(
      'gamma',
      'mu_gamma',
      'sigma_gamma',
      'alpha',
      'beta',
      'nu',
      'Rho',
      'log_lik'
   ),
   warmup = n.warmup,
   iter = n.iter,
   chains = n.cores,
   refresh = n.refresh,
   control = list(adapt_delta = .999,
                  max_treedepth = 15),
   seed = 52343041
)

# save results
saveRDS(fit, paste0(app.path, 'econ.app.fit.rds'))

gen.stan.summary(fit,
                 c('mu_gamma',
                   'sigma_gamma',
                   'nu',
                   'Rho'),
                 app.path,
                 add.to.name = '.econ')


## Summarize Results ----------------------------------------------------------

message('\n\nPlotting raw coefficients ...')

# get samples
full.samples <- as.data.table(fit)

# extract regression coefficients
full.samples <- full.samples[
   , !grepl('log_lik|lp__', names(full.samples)), with = F
]

# reshape data into long format
df <- suppressWarnings(melt(
   full.samples[, grepl('mu_gamma', names(full.samples)), with = F]
))[
   order(variable)
]

# mu_gamma number
df[, coef.no := gsub('(mu\\_gamma\\[)(.*)(\\])', '\\2', variable) %>%
      as.integer]

# create number to label table
var.names <- data.table(
   coef.no = 1:stan.dat$K,
   coef.label = names(stan.dat$x)
)[
   , coef.label := gsub('c\\.', '', coef.label) %>%
      gsub('(year\\.)([0-9])', 'Time^\\2', .) %>%
      gsub('year$', 'Time', .) %>%
      gsub('V1', 'Intercept', .) %>%
      gsub('ind', 'Independent', .) %>%
      gsub('dem', 'Democrat', .) %>%
      gsub('\\.', ' %*% ', .)
]

# merge
df <- merge(df, var.names, by = 'coef.no', all.x = T)

# change labels into factors (to control plotting order)
df[, coef.label := factor(coef.label,
                          levels = var.names[order(-coef.no), coef.label])]

# calculate posterior median and 95% posterior intervals
df[, m := median(value), by = coef.label]
df[, lb := quantile(value, .025), by = coef.label]
df[, ub := quantile(value, .975), by = coef.label]

# drop posterior draws and use only summary stats
df.2 <- unique(df[, .(coef.label, m, lb, ub)])

# save data
saveRDS( df.2, paste0(app.path, 'econ.app.gamma.rds'))

# plot
pdf(
   paste0(graphics.path,'FigureS6B.pdf'),
   width = 5,
   height = 5
)

print(
   ggplot(df.2, aes(x = coef.label, y = m)) +
      geom_hline(aes(yintercept = 0), linetype = 2, size = .3) +
      geom_linerange(aes(ymin = lb, ymax = ub),
                     col = 'grey50',
                     size = .6) +
      geom_point(col = 'black', size = 2) +
      scale_x_discrete(name = '',
                       labels = parse(text = levels(df.2$coef.label)))+
      labs(y = 'Coefficient') +
      coord_flip() +
      theme_bw() + 
      theme(panel.grid = element_blank())
)

dev.off()


### Save other parameters

# get names of parms
aux.parms <- c(
   'psi', 
   'nu', 
   grep('sigma_gamma', names(full.samples), value = T)
)

# select names in data
aux.parms <- aux.parms[aux.parms %in% names(full.samples)]

# number of random effects
x <- suppressWarnings(
   gsub('(sigma\\_gamma\\[)([0-9]*)(\\])', '\\2', aux.parms) %>% 
      as.numeric %>% 
      max(na.rm=T)
)

# get posterio sapmles
aux.dat <- full.samples[
   , ..aux.parms
][ # invert scale of nu (to original scale)
   , nu := 1/nu
]

# calculate variances
for (v in 1:x) {
   aux.dat[, paste0('var',v) := get(paste0('sigma_gamma[',v,']'))]
}
aux.parms <- gsub('(sigma\\_gamma\\[)([0-9]*)(\\])','var\\2',aux.parms)

# calculate median and posterior intervals
suppressWarnings(
   aux.df <- melt(aux.dat[, ..aux.parms])[
      , list(
         median(value), quantile(value, .025), quantile(value, .975)
      ),
       by = variable] %>%
      setnames(c('coef.label', 'm', 'lb', 'ub'))
)

# save 
saveRDS(aux.df, paste0(app.path, 'econ.app.aux.rds'))


## Tables ---------------------------------------------------------------------

gamma.dat <- rbind(df.2, aux.df)

nn <- names(gamma.dat)
gamma.dat[, (nn[nn != 'coef.label']) :=
             lapply(.SD,
                    function (x) {
                       z <- formatC(round(x, 3), format = 'f', digits = 3)
                       z[grepl('NA', z)] <- ''
                       return(z)
                    }),
          .SDcols = nn[nn != 'coef.label']]

# put credible intervals in parentheses
gamma.dat[, posti := gen.posti(lb, ub)]

# drop unnecessary columns
gamma.dat <- gamma.dat[ , .(coef.label, m, posti)] %>%
   setnames(c('Variables', 'Median', '95\\% Cred. Int.'))

# change orders of Time into LaTeX format
gamma.dat[, Variables := gsub('(\\^)([0-9])', '$^\\2$', Variables)]

# change psi or nu into LaTex format
gamma.dat[
   Variables %in%  c('psi','nu'), Variables := paste0('$\\',Variables,'$')
   ]

n.fixed <- sum(!grepl('var|nu|psi|sigma', gamma.dat$Variables))
# change Sigma
gamma.dat[
   grepl('var[0-9]*',Variables), 
   Variables := paste0('$\\Sigma(\\text{',
                       gamma.dat$Variables[1:n.fixed],
                       '})$')
]

# generate xtable
x.tab <- xtable(
   gamma.dat, align = c('l', 'l', rep('r', ncol(gamma.dat) - 1))
) 

# get printed form
capture.output(
   {
      x.tab <- print(
         x.tab,
         include.rownames = F,
         sanitize.text.function = function(x) {
            x
         }
      )
   },
   file = ifelse(.Platform$OS.type == 'windows',
                 'NUL', '/dev/null')
)

# change * to \times
x.tab <- gsub('\\%\\*\\%',' $\\\\times$ ',x.tab) 

# print table into file
capture.output( 
   cat(x.tab),
   file = paste0(app.path, 'econ.coef.tab.txt')
)



## Predict Aggregate Trends ---------------------------------------------------

message('Plotting aggregate trends ...')

time.vec <- stan.dat$x$c.year %>% unique %>% sort
time.mat <- cbind(1, time.vec, time.vec^2) 

# generate predited (mean) trends
trend.list <- gen.agg.trends(fit, time.mat, mean.only = T)

# turn to data.table object                    
agg.trend.dat <- lapply(trend.list, `[[`, 'summary') %>%
   do.call(rbind, .) %>%
   data.table

# add identifier for posterior draw
agg.trend.dat[, draw := rep(1:length(trend.list),
                            each = nrow(time.mat))]

# reshape into long format
trend.df <- melt(agg.trend.dat,
                 id.vars = c('time', 'draw'),
                 variable.name = 'pid')

# calculate median and 95% posterior interval
trend.df[, m := median(value), by = list(time, pid)]
trend.df[, lb := quantile(value, .025), by = list(time, pid)]
trend.df[, ub := quantile(value, .975), by = list(time, pid)]

# keep only summaries
trend.df <- unique(trend.df[, .(time, pid, lb, m, ub)])

# turn pid into factor 
trend.df[, pid := factor(
   pid,
   levels = c('dems', 'inds', 'reps'),
   labels = c('Democrat',
              'Independent',
              'Republican')
)]

# save data
saveRDS(trend.df, paste0(app.path, 'econ.app.agg.trends.rds'))

# plot
pdf(
   paste0(graphics.path, 'FigureS6A.pdf'),
   width = 5,
   height = 4
)

print(
   ggplot(trend.df[pid != 'Diff'],
          aes(x = time,
              y = m,
              linetype = pid,
              fill = pid)
   ) +
      geom_line(size = .8) +
      labs(y = '% Liberal', x = 'Time')+
      scale_linetype_discrete(name = 'Partisanship')+   
      scale_x_continuous(
         limits = c(-2.2, 2.2),
         breaks = c(-2.2, 0, 2.2),
         labels = c("'72", "'94", "'16")
      ) +
      theme_bw() +
      theme(
         legend.position = 'bottom',
         legend.key.width = unit(1, 'cm'),
         axis.text = element_text(size = 10),
         panel.grid = element_blank(),
         panel.border = element_rect(color = 'black')
      )
)

dev.off()


#### END OF CODE ####

