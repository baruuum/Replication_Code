###############################################################################
##                                                                           ##
##  Analyzing Posterior Samples from Fitted Models                           ##
##                                                                           ##
###############################################################################


## Basic Setup ----------------------------------------------------------------

# check base directory
if (!exists('wd.base'))
   stop('Specify wd.base')

# which domain?
if (!exists('domain'))
   stop('Specify which domain to check')

message(paste0('\n\nWorking on domain ', domain,' ------------------'))
	
# which models?
if (!exists('s.models'))
   stop('Need "s.models" object containing selected models')

# number of posterior draws for ppc
if (!exists('n.ppc'))
   stop('Specify how many posterior draws to use for ppc (n.ppc)')

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

# path to domain
domain.path <- paste0(stan.path, 'domain_', domain, '/')
 
# load functions
if (!exists('gen.agg.trends')) 
   source(paste0(code.path, '0.Functions.R'))

## Data Setup -----------------------------------------------------------------

message('Setting up the data ...')

# get model string
model.str <- s.models[domain]

# functional form for time trend
t.trend.str <- sub('\\..*','',model.str)
t.order <- ifelse(t.trend.str == 'linear',
                  1, ifelse(t.trend.str == 'quad',
                            2,3))

# get domain data
m.dat <- long.dat[i.vars.class == domain]

# read issue mapping
issue.map <- readRDS(
   paste0(domain.path, 'd', domain,'.issuemap.rds')
)

# merge new issue.no's to data
m.dat<- merge(m.dat, issue.map, by.x='i.vars.no', by.y='old')

# read in data feeded to STAN
stan.dat <- readRDS(
   paste0(domain.path, 'd', domain, '.', t.trend.str, '.stan.dat.rds')
)

# read fitted model
fit <- readRDS(
   paste0(domain.path, 'd', domain, '.', model.str, '.rds')
)
	


## Plot Raw Coefficients ------------------------------------------------------
	
message('Extracting Posterior Samples ...')

# get samples
full.samples <- as.data.table(fit)

# extract regression coefficients
full.samples <- full.samples[
   , !grepl('log_lik|lp__', names(full.samples)), with = F
]

# reshape data into long format
df <- suppressWarnings(
   melt(full.samples[
      , grepl('mu_gamma', names(full.samples)), with = F
   ]))[order(variable)]

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
saveRDS(df.2, paste0(domain.path, 'd', domain, '.gamma.rds'))


### Save other parameters

# get names of parms
aux.parms <- c(
   'psi', 
   'nu', 
   grep('sigma_gamma', names(full.samples), value = T)
)
# select names in data
aux.parms <- aux.parms[ aux.parms %in% names(full.samples) ]

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
   aux.dat[
      , paste0('var',v) := get(paste0('sigma_gamma[',v,']'))
   ]
}

aux.parms <- gsub('(sigma\\_gamma\\[)([0-9]*)(\\])', 'var\\2', aux.parms)

# calculate median and posterior intervals
suppressWarnings(
   aux.df <- melt(aux.dat[, ..aux.parms])[
      , list(median(value), quantile(value, .025), quantile(value, .975)), 
      by = variable
   ] %>%
      setnames(c('labs', 'm', 'lb', 'ub'))
)

# save 
saveRDS(
   aux.df,
   paste0(domain.path, 'd', domain, '.aux.rds')
)



## Predict Aggregate Trends ---------------------------------------------------

message('Generating Trend Data ...')

time.vec <- stan.dat$x$c.year%>%unique%>%sort
if (t.order==1) {
   
   time.mat <- cbind(1, time.vec)
   
} else if (t.order == 2) {
   
   time.mat <- cbind(1, time.vec, time.vec ^ 2)
   
} else time.mat <- cbind(1, time.vec, time.vec ^ 2, time.vec ^ 3)

# generate predited (mean) trends
trend.list <- gen.agg.trends(fit, time.mat)

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
   levels = c('dem', 'ind', 'rep', 'dr.diff'),
   labels = c('Democrat',
              'Independent',
              'Republican',
              'Diff')
)]

# save data
saveRDS(trend.df, paste0(domain.path,
                         'd', domain,
                         '.agg.trends.rds'))



## Predicted Trends for Individual Issues -------------------------------------

# turn to data.table object                    
i.trend.dat <- lapply(trend.list, `[[`, 'issue') %>%
   do.call(rbind, .) %>%
   data.table

i.trend.dat[
   , dr.diff := dem - rep
][
   , draw := rep(1:length(trend.list), each = max(issue) * nrow(time.mat))
]   

i.trend.df <- melt(i.trend.dat,
                   id.vars = c('issue','time','draw'),
                   variable.name = 'pid')

# calculate median and 95% posterior interval
i.trend.df[
   , `:=`(
         m = median(value), 
         lb = quantile(value, .025),
         ub = quantile(value, .975)
      ),
   by = list(time, issue, pid)]


# keep only summaries
i.trend.df <- unique(i.trend.df[, .(time, issue, pid, lb, m, ub)])

# turn pid into factor 
i.trend.df[, pid := factor(
   pid,
   levels = c('dem', 'ind', 'rep', 'dr.diff'),
   labels = c('Democrat',
              'Independent',
              'Republican',
              'Diff')
)]

# get issue names
i.names <- unique(m.dat[, .(i.vars.label, new)])
i.trend.df[, issue := factor(issue,
                             levels = i.names$new,
                             labels = i.names$i.vars.label)]

# save data
saveRDS(i.trend.df, paste0(domain.path,
                           'd', domain,
                           '.i.trends.rds'))



## Posterior Predictive Checks ------------------------------------------------

message('Plotting posterior predictive distribution ...')

# get plotting order used in the descriptive trends
p.order <- readRDS(
   paste0(data.path, 'p.order.rds')
)

# reorder moral domain to match descriptives
x <- p.order$moral
y <- p.order$moral.sub

z <- match(y, x)
x.new <- x[c(z, setdiff(1:length(x), z))]
p.order$moral <- x.new

# drop "moral.sub" 
p.order$moral.sub <- NULL


### Sample from posterior predictive dist

# sample n.ppc posterior draws
sampled <- sample(1:nrow(full.samples), n.ppc, replace = F)

# get alpha (first shape param.)
alpha.draws <- full.samples[
   sampled, grepl('alpha', names(full.samples)), with = F
] %>% 
   as.matrix

# get beta (second shape param.)
beta.draws <- full.samples[
   sampled, grepl('beta', names(full.samples)), with = F
] %>% as.matrix

# draw from beta-dist using alpha and beta
y.pred.list <- lapply(
   1:nrow(alpha.draws), function (w) {
      
      a <- alpha.draws[w, ]
      b <- beta.draws[w, ]
      y <- rbeta(length(a), a, b)
   }
)

### Generate data for prediction

df <- data.table(stan.dat$x[, .(c.year, V1, ind, dem)],
                 stan.dat$y,
                 stan.dat$jj) %>%
   setnames(c('year', 'rep', 'ind', 'dem', 'pr.agree', 'new.vars.no'))

# get issue labels
df <- merge(df,
            unique(m.dat[, .(new, i.vars.label)]),
            by.x = 'new.vars.no',
            by.y = 'new',
            all.x = T)

# generate original pid variable
df[, pid := ifelse(ind == 0 & dem == 0,
                   1, ifelse(ind == 1 & dem == 0,
                             2, 3))]
# drop old ones
df[, `:=`(ind = NULL, dem = NULL, rep = NULL)]

# merge with samples
ppc.df <- cbind(df, do.call(cbind, y.pred.list))

# reshape into long format
ppc.df <- melt(ppc.df,
               id.vars = c('year',
                           'new.vars.no',
                           'i.vars.label',
                           'pid',
                           'pr.agree'))

# create factor for pid
ppc.df[, pid := factor(
   pid,
   levels = 1:3,
   labels = c('Republican',
              'Independent',
              'Democrat')
)]

# create factor of issue labels
ppc.df[, i.vars.label := factor(i.vars.label,
                                levels = p.order[[domain]])]

n.issues  <- ppc.df$i.vars.label %>% unique %>% length
n.rows <- ceiling(n.issues / 6)

pdf(
   paste0(graphics.path, 'FigureS',domain,'.pdf'),
   width = 12,
   height = 2.2 * n.rows
)
print(
   ggplot(ppc.df[pid != 'Independent'],
          aes(x=year,
              y=value,
              col=pid,
              group=interaction(variable,pid))) +
      geom_line(size=.4, alpha=.1) +
      geom_line(aes(y=pr.agree), size=.5)+
      scale_color_manual(name='Partisanship',
                         values=c('Republican'='red',
                                  'Independent'='grey',
                                  'Democrat'='blue')) +
      scale_y_continuous(name='% Liberal',
                         limits=c(0,1),
                         breaks=c(.2,.5,.8)
      ) +
      scale_x_continuous(limits=c(-2.2,2.2),
                         breaks=c(-2.2,0,2.2),
                         labels=c("'72","'94","'16")
      ) +
      labs(x='Time')+
      theme_bw() +
      theme(
         axis.title=element_blank(),
         axis.text.y=element_text(size=12),
         axis.text.x=element_text(size=12),
         strip.text.y=element_blank(),
         strip.text.x=element_text(size=12,
                                   hjust=.1,
                                   face='bold'),
         strip.background=element_blank(),
         panel.grid=element_blank(),
         panel.border=element_rect(color='black'),
         legend.position='none',
         plot.margin=unit(
            c(0,.1,.05,.1),
            units='cm')
      )+
      facet_wrap(~i.vars.label, ncol=6)
)
dev.off()

#### END OF CODE ####

