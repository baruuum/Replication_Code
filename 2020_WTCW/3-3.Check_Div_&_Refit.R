###############################################################################
##                                                                           ##
##  Check for Divergences and Refitting                                      ##
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

# which domain?
if (!exists('domain'))
   stop('Specify which domain to check')
# load seeds
if (!exists('stan.seeds'))
   stan.seeds <- fread(paste0(raw.data.path, 'stan.seeds.csv'))


# set path to domain 
check.path <- paste0(stan.path, 'domain_', domain, '/')


## Check for Divergences ------------------------------------------------------

### Get name-list of models fitted

# functional forms fitted (time)
if (domain == 4) {
    func.forms <- c('linear','quad')
} else func.forms <- c('linear','quad','cubic')

# distributions fitted (outcome,rand.eff)
dist.forms <- c('nn','bn')

# model names
m.names <- expand.grid(func.forms, dist.forms) %>%
   data.table %>%
   setnames(c('t','m')) %>%
   mutate(mm = paste0(t,'.',m)) %>%
   `[[`('mm') 

# get convergence summary
conv.summary <- lapply(
   m.names, function (w) {
      x <- readLines(paste0(check.path, w, '.txt'))
      # get divergence part of text-file
      div.line <- which(grepl('Divergences', x)) + 1
      # get divergences
      divs <- strsplit(x[div.line],' ') %>%
         unlist %>%
         `[`(-1) %>%
         as.numeric %>%
         sum(na.rm=T)
      
      # get rhat part of text-file
      rhat.line <- which(grepl('Rhat statistics',x)) + 2
      # get median rhat
      med.rhat <- strsplit(x[rhat.line],'\\s+')  %>%
         unlist %>%
         as.numeric %>%
         median(na.rm=T)
      
      # get effective sample size part
      neff.line <- which(grepl('eff\\. sample sizes',x)) + 2
      # get median neff
      med.neff <- strsplit(x[neff.line],'\\s+')  %>%
         unlist %>%
         as.numeric %>%
         median(na.rm=T)
      
      res <- c(divs,med.rhat,med.neff)
      names(res) <- c('divergences','med.rhat','med.neff')
      return(res)
      
   }
) %>% 
   do.call(rbind,.) %>%
   data.table

# add model names
conv.summary[,model := m.names] %>%
   setcolorder('model')

# print results to file
capture.output(
   print(conv.summary, row.names=F),
   file=paste0(check.path, 'd', domain, '.conv.summary.txt')
)

# get models with low neff and high rhat
# note: thresholds are median Rhat = 1.1 and 
#       median Effective Samp. Size = 3000
not.conv <- conv.summary[med.rhat > 1.1 | med.neff < 3000, model]

# send out some messages
message(
   paste0('\n\n\n\n\n\n\n\n\n\nChecking convergence and divergent transitions for domain ',
          domain, ' ... ')
)

if (length(not.conv) == 0) {
   
   message('\nRhat and effective sample sizes seem to be okay for all models.')
   
} else {
   
   message('Neff low and/or Rhat high for following models :')
   message(
      paste0('High Rhat (median Rhat > 1.1): ',
             paste0(conv.summary[med.rhat>1.1, model],
                    collapse=', ')
             )
      )
   message(
      paste0('Low Neff (median Neff < 3000): ',
             paste0(conv.summary[med.neff < 3000, model],
                    collapse=', ')
            )
      )

}


## Refit Models with Divergent Transitions ------------------------------------

# remove all files ending with refit.txt (past results)
rr.files <- grep('refit\\.txt$', dir(check.path), value = T)
if (length(rr.files)!=0) 
	file.remove(paste0(check.path, rr.files))

# names of models
divs <- conv.summary[divergences > 0, model]

if (length(divs) == 0) {
   
   message(
      paste0('\nNo divergent transitions found for models in domain ',
             domain
      )
   )
} else {
   # send out message
   message(
      paste0(
         '\nSome of the chains for the following models resulted ',
         'divergent transitions (Domain ',
         domain,
         ') :\n\n\t',
         paste0(
            paste0(
               divs,
               '(',conv.summary[divergences > 0, divergences],')'
            ), collapse=',  '
         ),
         '\n\n(Number in parentheses indicate # of div. transitions across all chains)'
      )
   )


   ## Refit Models (if necessary)  ------------------------------------
   
   ## REFITTING MODELS WITH LOW RHAT AND HIGH NEFF ONLY !!!
   
   message('Refitting models with divergent transitions ...')
   message('(Models with low Neff or high Rhat are not refitted !)')
   
   refit.models <- setdiff(divs, not.conv)
   
   for (dd in refit.models) {
      
      # read in old model
      model <- readRDS(
         paste0(
            check.path,
            'd',domain,'.',
            dd,'.rds'
         )
      )
      
      # get functional form of time
      f.form <- sub('(.*)(\\..*)','\\1',dd)
		
		# read in data
      stan.dat <- readRDS(
         paste0(
            check.path,
            'd',domain,
				'.', f.form,
            '.stan.dat.rds'
         )
      )

      # select pars to sample
      pp <- model@model_pars
      pp <- pp[pp != 'lp__' & !grepl('star|xb|L_', pp)]
      
      # get compiled model
      mod.name <- paste0(sub('(.*\\.)(.*)', '\\2', dd), '.model')
      if (!exists(mod.name)) {
        stan.model <- readRDS(
           paste0(
              stan.path,
              'models/stan.',
              sub('(.*\\.)(.*)', '\\2', dd), 
              '.model.rds'
           )
        )
      } else stan.model <- get(mod.name)
      
      # get seed
      s.seed <- stan.seeds[model == paste0(dd, '.refit') & 
                              domain == domain]$seed

      # get stan model
      refit <- sampling(
         stan.model,
         pars = pp,
         data = stan.dat,
         chains = length(model@stan_args),
         iter = model@stan_args[[1]]$iter + add.iter,
         warmup = model@stan_args[[1]]$warmup + add.iter,
         refresh = model@stan_args[[1]]$refresh,
         seed = s.seed,
         control = list(adapt_delta = a.delta,
                        stepsize = s.size,
                        max_treedepth = m.depth)
      )
      
      saveRDS(refit,
              paste0(
                 check.path,
                 'd', domain, '.', dd, '.refit.rds'
              )
      )
      
      gen.stan.summary(refit,
                       pp[!(pp %in% c('xi',
                                      'alpha',
                                      'beta',
                                      'log_lik'))],
                       check.path,
                       refitting = T)
        message('\n\nDone!')
        message(
            paste0('Model summaries are found in file ',
                   dd, '.refit.txt')
        )
        
		rm(refit)
     
   }
   
}


## Update Convergence Summary -------------------------------------------------

# get updated convergence summary
conv.summary <- lapply(
   m.names, function (w) {
      
      # read in updated model if existent
      if (file.exists(paste0(check.path, w, '.refit.txt'))) {
         
         x <- readLines(paste0(check.path, w, '.refit.txt'))
         re.f <- 1
         
      } else {
         
         x <- readLines(paste0(check.path, w, '.txt'))
         re.f <- 0
         
      }
      
      # get divergence part of text-file
      div.line <- which(grepl('Divergences', x)) + 1
      # get divergences
      divs <- strsplit(x[div.line],' ') %>%
         unlist %>%
         `[`(-1) %>%
         as.numeric %>%
         sum(na.rm=T)
      
      # get rhat part of text-file
      rhat.line <- which(grepl('Rhat statistics',x)) + 2
      # get median rhat
      med.rhat <- strsplit(x[rhat.line],'\\s+')  %>%
         unlist %>%
         as.numeric %>%
         median(na.rm=T)
      
      # get effective sample size part
      neff.line <- which(grepl('eff\\. sample sizes', x)) + 2
      # get median neff
      med.neff <- strsplit(x[neff.line], '\\s+')  %>% 
         unlist %>%
         as.numeric %>%
         median(na.rm=T)
      
      res <- c(divs, med.rhat, med.neff, re.f)
      names(res) <- c('divergences', 'med.rhat', 'med.neff', 'refitted')
      return(res)
      
   }
) %>% 
   do.call(rbind, .) %>%
   data.table

# add model names
conv.summary[,model := m.names] %>%
   setcolorder('model')
conv.summary[
   , model := ifelse(refitted == 1, paste0(model,'.refit'), model)
][ 
   , refitted := NULL
]

# print results to file
capture.output(
   print(conv.summary, row.names=F),
   file=paste0(check.path, 'd', domain, '.conv.summary.updated.txt')
)


#### END OF CODE ####     
