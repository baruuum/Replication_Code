##############################################################################
##                                                                          ##
##  Generating Summary Tables from Models                                   ##
##                                                                          ##
##############################################################################


## Basic Setup ---------------------------------------------------------------

# check base directory
if (!exists('wd.base'))
   stop('Specify wd.base')

if (!exists('s.models'))
   stop('Need "s.models" object')

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
if (!exists('tables.path'))
   tables.path <- paste0(wd.base, 'tables/')

if (!dir.exists(tables.path)) {
   message(paste0('Creating Directory ~/tables/ into ', wd.base))
   dir.create(tables.path)
}

# load functions
if (!exists('gen.posti'))
   source(paste0(code.path, '0.Functions.R'))

## Divergences ? -------------------------------------------------------------

message('\n\nSummarizing Convergence Statistics --------------------')


# load STAN results
stan.res <- lapply(
   1:4, function (w) {
      readRDS(
         paste0(stan.path, '/domain_',w,'/d',w,'.', s.models[w],'.rds')
      )
   })

# get summary of diagnostics
diags <- lapply(
   stan.res, function (w) {
      
      n.warmup <- w@stan_args[[1]]$warmup
      n.iter <- w@stan_args[[1]]$iter
      
      mixing <- summary(w)$summary[, c('n_eff','Rhat')] 
      neff <- min(mixing[,1], na.rm=T)
      neff2 <- mean(mixing[,1], na.rm=T)
      rhat <- max(mixing[,2], na.rm=T)
      
      div <- sapply(
         get_sampler_params(w),
         function (z) {
            a <- z[(n.warmup + 1):n.iter, c('divergent__', 'treedepth__')]
            c(sum(a[,1]), max(a[,2]))
            
         }
      )
      
      c(min.neff = neff, 
        mean.neff = neff2,
        max.rhat = rhat, 
        n.div = sum(div[1,]), 
        max.depth = max(div[2,])
      )
   }
) %>% 
   do.call(rbind, .) %>%
   cbind(domain = 1:4,.)

message('\nSummary of Diagnostics :')
message(paste0(capture.output(diags), collapse = '\n'))

# save diagnostics
capture.output(diags, file = paste0(tables.path, 'div_stats.txt'))


## Summary -------------------------------------------------------------------

message('Generating LaTex Table of Model Coefficients ...')

### table of raw gamma coefficients

# get gamma coefficients
gamma.res <- lapply(1:4, function(w) {
   
   tmp <- readRDS(
      paste0(stan.path, '/domain_', w, '/d', w, '.gamma.rds')
   )
   
   tmp2 <- readRDS(
      paste0(stan.path, '/domain_', w, '/d', w, '.aux.rds')
   ) %>%
      setnames(names(tmp))
   
   tmp <- rbind(tmp, tmp2)
   names(tmp)[names(tmp) != 'coef.label'] <- 
      paste0(names(tmp)[names(tmp) != 'coef.label'], '.', w)
   return(tmp)

})


# merge all together
gamma.dat <- Reduce(
   function(dt1,dt2) merge(dt1,dt2, by='coef.label', all=T), 
   gamma.res)

# generate copy
gamma.df <- copy(gamma.dat[!grepl('nu|var[0-9]|psi', coef.label)])

# round entries to the (dig)th digit and change to character
# note: NA values are changed to empty spaces
nn <- names(gamma.dat)
gamma.dat[, (nn[nn != 'coef.label']) :=
             lapply(.SD,
                    function (x) {
                       z <- formatC(round(x, dig), format = 'f', digits = dig)
                       z[grepl('NA', z)] <- ''
                       return(z)
                    }),
          .SDcols = nn[nn != 'coef.label']]


# order to display coefficients
r.order <- expand.grid(
   c('','Independent','Democrat'),
   c('Time','Time^2','Time^3')
) %>%
   apply(1, paste0, collapse=" %*% ") %>%
   c('Intercept', 'Independent', 'Democrat', .) %>%
   sub('^ \\%\\*\\% ','',.) 

ext.r.order <- c(r.order, 'nu', 
                 setdiff(gamma.dat[, coef.label], c('nu',r.order)))

# reorder dataset
gamma.dat <- gamma.dat[match(ext.r.order, coef.label)]


# put credible intervals in parentheses
gamma.dat[, posti.1 := gen.posti(lb.1, ub.1)]
gamma.dat[, posti.2 := gen.posti(lb.2, ub.2)]
gamma.dat[, posti.3 := gen.posti(lb.3, ub.3)]
gamma.dat[, posti.4 := gen.posti(lb.4, ub.4)]

# drop unnecessary columns
gamma.dat <- gamma.dat[
   , .(coef.label, m.1, posti.1, m.2, posti.2, 
       m.3, posti.3, m.4, posti.4)
] %>%
   setnames(
      c(
         'Variables',
         'Median',
         '95\\% Cred. Int.',
         'Median',
         '95\\% Cred. Int.',
         'Median',
         '95\\% Cred. Int.',
         'Median',
         '95\\% Cred. Int.'
      )
   )

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
   Variables:=paste0(
      '$\\Sigma(\\text{',
      gamma.dat$Variables[1:n.fixed],
      '})$')
   ]

# generate xtable
x.tab <- xtable(gamma.dat, 
                align=c('l','l',rep('r',ncol(gamma.dat)-1))) 

# get printed form
capture.output(
   {
      x.tab <- print(x.tab, 
                     include.rownames=F, 
                     sanitize.text.function=function(x){x})
   },
   file=ifelse(.Platform$OS.type=='windows',
               'NUL','/dev/null')
)

# add model names
x.tab <- sub('Variables',
             paste0('&',
                    '\\\\multicolumn{2}{c}{Economic}&',
                    '\\\\multicolumn{2}{c}{Civil Rights}&',
                    '\\\\multicolumn{2}{c}{Moral}&',
                    '\\\\multicolumn{2}{c}{Foreign Policy/Security}',
                    ' \\\\\\\\ ',
                    '\\\\cline{2-9}',
                    '\n',
                    'Variables'),
             x.tab)

# change * to \times
x.tab <- gsub('\\%\\*\\%',' $\\\\times$ ',x.tab) 

# print table into file
capture.output( 
   cat(x.tab),
   file = paste0(tables.path, 'Table2.txt')
)



## WAIC and LOOIC -------------------------------------------------------------

# read GOF data and turn into data.table

ic.res <- lapply(
   1:4, function (w) {
      # load data
      tmp <- readRDS(paste0(stan.path, 
                            'domain_',
                            w,
                            '/d',
                            w,
                            '.gof.rds'))
      
      # rename "Time Trend" column
      setnames(tmp, 'Time Trend', 'tt')
      tmp[, tt := ifelse(grepl('Linear', tt),
                         'Linear',
                         ifelse(grepl('Quad', tt),
                                'Quadratic',
                                'Cubic'))] %>% 
      setnames('tt', 'Time Trend')
      
      # add domain number
      tmp[, domain := w]
      
      # add number of models in domain
      tmp[, n.models := .N]
      
      # drop standard errors for clarity
      tmp <- tmp[, !grepl('s\\.e\\.', names(tmp)), with = F]
      
      return(tmp)
      
   }
) %>% rbindlist

# add domain names and keep only the first ones

ic.res[ # create one lag
   
   , domain.2:=shift(domain, n=1L, type= 'lag', fill = 999)
   
][ # flag first row of each domain
   
   , first.row := domain != domain.2
   
][ # recode domain into labels
   
   , domain := dplyr::recode(domain,
                      '1' = 'Economic',
                      '2' = 'Civil Rights',
                      '3' = 'Moral',
                      '4' = 'Foreign Policy')
   
][ # for all non-first rows, assign empty string to domain name
   
   first.row != T, domain := ''
   
][ # in first-rows create "multirow" string (for LaTex)

   first.row == T, domain := paste0('\\multirow[t]{',
                                    n.models,
                                    '}{*}{',
                                    domain,
                                    '}')
][ # drop unnecessary variables
   
   , `:=`(first.row = NULL, domain.2 = NULL, n.models = NULL)
   
] %>% # put domain in first column and rename that column
   setcolorder('domain') %>%
   setnames('domain', 'Issue Domain')

# create LaTex output
capture.output(
   {
      x.tab <- print(
         xtable(ic.res,
                align = c(rep('l', 4), 'r', 'r')),
         include.rownames = F,
         sanitize.text.function = function(u) {u}
      )
   },
   file = ifelse(.Platform$OS.type == 'windows',
                 'NUL', '/dev/null')
)

# add note to code
x.tab <- paste0('% NOTE: PACKAGE multirow MUST BE INSTALLED TO COMPILE ',
                'THE FOLLOWING TABLE IN LateX !!! \n',
                x.tab)
# add some space between issue domains
x.tab <- gsub('(\\n)(\\s+)(\\\\multirow)', '[1em] \n \\3', x.tab)

# print table to file
capture.output(
   cat(x.tab),
   file = paste0(tables.path, 'Table1.txt')
)

### END OF CODE ###