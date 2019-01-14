###############################################################################
##                                                                           ##
##  Fit STAN models                                                          ##
##                                                                           ##
###############################################################################


## Basic Setup ----------------------------------------------------------------

if (!exists('wd.base'))
   stop('Specify wd.base!')

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

# load seeds
if (!exists('stan.seeds'))
   stan.seeds <- fread(paste0(raw.data.path, 'stan.seeds.csv'))


# read in data
long.dat <- readRDS(paste0(data.path, 'long.comb.dat.rds'))

# load functions if necessary
if (!exists('gen.stan.summary'))
   source(paste0(code.path, '0.Functions.R'))

## Fit Models -----------------------------------------------------------------


message('\n\nStart Fitting Models -------------')

for (dd in 1:4) {

   tmp.path <- paste0(stan.path, 'domain_', dd, '/')
   
   if (!dir.exists(tmp.path)) {
      message(
         paste0(
            'Crearing subfolder named domain_',
            dd,
            ' into ',
            stan.path
         )
      )
      dir.create(tmp.path)
   }
   
	# save issue mapping 
   # (issue numbers start from 1 for all domains in data
   #  feeded into STAN)
   m.dat <- long.dat[i.vars.class == dd]
	issue.map <- unique(m.dat$i.vars.no)
	issue.map <- data.table(old = issue.map, new = 1:length(issue.map))
	saveRDS(issue.map, paste0(tmp.path, 'd', dd, '.issuemap.rds'))
	
	# add new var.no's
	m.dat<- merge(m.dat,
	              issue.map, 
	              by.x='i.vars.no',
	              by.y='old',
	              all.x=T)
	
	# stan data
	stan.dat <- list()
	stan.dat$N <- nrow(m.dat) 
	stan.dat$jj <- m.dat$new   
	stan.dat$J <- length(unique(stan.dat$jj))
	stan.dat$y <- m.dat$pr.agree 

	# data for linear trends
	stan.dat.linear <- stan.dat
	stan.dat.linear$x <- m.dat[, .(ind, dem, c.year)] %>%
	   mutate(ind.year = ind * c.year,
	          dem.year = dem * c.year) %>%
	   cbind(1, .)
	stan.dat.linear$K <- ncol(stan.dat.linear$x)
	
	# data for quadratic trends
	stan.dat.quad <- stan.dat
	stan.dat.quad$x <- m.dat[, .(ind, dem, c.year, c.year.2)] %>%
	   mutate(
	      ind.year   = ind * c.year,
	      dem.year   = dem * c.year,
	      ind.year.2 = ind * c.year.2,
	      dem.year.2 = dem * c.year.2
	   ) %>%
	   cbind(1, .)
	stan.dat.quad$K <- ncol(stan.dat.quad$x)

	# data for cubic trends
	stan.dat.cubic <- stan.dat
	stan.dat.cubic$x <- m.dat[, .(ind, dem, c.year, c.year.2, c.year.3)] %>%
	   mutate(
	      ind.year   = ind * c.year,
	      dem.year   = dem * c.year,
	      ind.year.2 = ind * c.year.2,
	      dem.year.2 = dem * c.year.2,
	      ind.year.3 = ind * c.year.3,
	      dem.year.3 = dem * c.year.3
	   ) %>%
	   cbind(1, .)
	stan.dat.cubic$K <- ncol(stan.dat.cubic$x)
	
	# save data
	saveRDS(stan.dat.linear,
	        paste0(tmp.path, 'd', dd, '.linear.stan.dat.rds'))
	saveRDS(stan.dat.quad,
	        paste0(tmp.path, 'd', dd, '.quad.stan.dat.rds'))
	saveRDS(stan.dat.cubic,
	        paste0(tmp.path, 'd', dd, '.cubic.stan.dat.rds'))
	
	
	# loop over linear, quadratic, and cubic models
	for (tt in c('linear','quad','cubic')) {
		
	   if (dd == 4 & tt == 'cubic') next
	   
		### Normal-Normal model 
	
		message(paste0(
			'Fitting Normal-Normal model with ',
			tt, ' time trend ( Domain = ',
			dd,' )'))
		
		# load normal-normal model if not already loaded
	   if (!exists('nn.model')) {
	      message('loading model ...')
	      nn.model <- readRDS(paste0(stan.path,
	                                 'models/stan.nn.model.rds'))
	   }
	   
	   # get seed
	   s.seed <- stan.seeds[model == paste0(tt,'.nn') & domain == dd]$seed
	   
	   # fit model
	   fit <- sampling(
	      nn.model,
	      data = get(paste0('stan.dat.', tt)),
	      pars = c('gamma',
	               'mu_gamma',
	               'sigma_gamma',
	               'sigma_e',
	               'Rho',
	               'log_lik'),
	      warmup = n.warmup,
	      iter = n.iter,
	      chains = n.cores,
	      refresh = n.refresh,
	      seed = s.seed
	   )
	   
		# save fit
		saveRDS(fit, paste0(tmp.path, 'd', dd, '.', tt, '.nn.rds'))
		
		# print summaries into separate file
		gen.stan.summary(fit,
		                 c('gamma',
		                   'mu_gamma',
		                   'sigma_gamma',
		                   'sigma_e',
		                   'Rho'),
		                 tmp.path)
		
        message('\n\nDone!')
        message(
            paste0('Model summaries are found in file ',
                   tt, '.nn.txt')
        )
        
		# remove fit
		rm(fit)
	
		### Beta-Normal Model
	
		message(paste0(
		   'Fitting Beta-Normal model with ',
		   tt,
		   ' time trend ( Domain = ',
		   dd,
		   ' )'
		))
		
		if (!exists('bn.model')) {
		   message('loading model ...')
		   bn.model <- readRDS(paste0(stan.path,
		                              'models/stan.bn.model.rds'))
		}
		
		# get seed
	   s.seed <- stan.seeds[model == paste0(tt,'.bn') & domain == dd]$seed

		fit <- sampling(
		   bn.model,
		   data = get(paste0('stan.dat.', tt)),
		   pars = c(
		      'alpha',
		      'beta',
		      'gamma',
		      'mu_gamma',
		      'sigma_gamma',
		      'nu',
		      'Rho',
		      'log_lik'
		   ),
		   warmup = n.warmup,
		   iter = n.iter,
		   chains = n.cores,
		   refresh = n.refresh,
		   seed = s.seed
		)

		saveRDS(fit,
		        paste0(tmp.path,
		               'd', dd, '.', tt, '.bn.rds'))
		
		gen.stan.summary(fit,
		                 print.pars = c('gamma',
		                                'mu_gamma',
		                                'sigma_gamma',
		                                'nu',
		                                'Rho'),
		                 tmp.path)
        
        message('\n\nDone!')
        message(
            paste0('Model summaries are found in file ',
                   tt, '.bn.txt')
        )
		
		rm(fit)
	
		# call garbage collector
		gc() 
	
	}
}	

message('Done!\n')

#### END OF CODE ####