###############################################################################
###                                                                         ###
### Function Definitions                                                    ###
###                                                                         ###
###############################################################################


gen.summary <- function(dat, var.list, weight.var) {
   
   ## FUNCTION TO GENERATE %LIBERAL RESPONSES ON ISSUES
   
   ## INPUT : 1) dat        = data (wide format, issues entered as columns)
   ##         2) var.list   = character vector containing names of variables
   ##                         for which percentages should be calculated
   ##         3) weight.var = variable containing survey weights
   ##
   ## OUTPUT: data.table object where each row is an issue and columns
   ##         contain %Liberal responses for all respondents and partisan
   ##         groups
   ##
   ## Notes : 1) all issues should be coded such that higher values 
   ##            correspond to more conservative positions
   ##         2) %Liberal is calculated as the percentage of individuals 
   ##            that gave a response below the midpoint of the scale of 
   ##            each issue
   
   # checking inputs
   if (!is.character(var.list)) 
      stop('var.list should be a character vector')
   if (!isTRUE(weight.var %in% names(dat)))
      stop('Could not find weight variable in data')
   
   # detect columns with no valid responses 
   # (i.e., issues not measured in given year)
   miss.pattern <- sapply(dat[,..var.list], function(z) sum(!is.na(z)))
   
   res <- lapply(
      var.list, 
      function(z) {

         if (miss.pattern[z]==0) {
            # no responses -> return NA and skip to next
            return(rep(NA,4))
         }
         
         # get var
         v <- dat[[z]]
         # midpoint of scale
         midpoint <- (max(v,na.rm=T) + min(v, na.rm=T))/2
         # generate midpoint var
         dat$below <- as.numeric(v < midpoint)
         # set up survey design
         d <- svydesign(ids=~1, 
                        weights=~get(weight.var), 
                        data=dat)
         # prop. below midpoint (% liberal)
         props <- prop.table(
            svytable(~below, design=d)
            )[2]
         # by pid.3 (note: leaners included in partisan groups)
         p.tab <- prop.table(
            svytable(~ below + pid.3, 
                     design=d
                     ),
            margin=2
            )
         return(c(props,p.tab[2,]))
      }
   ) %>%
   do.call(rbind,.) %>%
   data.table %>%
   setnames(c('props','props.d','props.i','props.r'))
   
   # add labels
   res[,i.vars.label:=var.list]   
   
   return(res)
}


drop.empty.facets  <- function(g, n.col,last.row) {

   ## FUNCTION TO ADD BLANK FACETS TO ggplot OBJECTS
   ##
   ## INPUT : g        = a ggplot object
   ##         n.col    = number of columns of the desired layout
   ##         last.row = number of columns the current ggplot object has
   ## 
   ## OUTPUT: a ggplot object with n.col columns where the (last.row+1)th
   ##         to the (n.col)th facets are white space
   
   # generate grob
   q <- ggplotGrob(g)
   # close opened window
   dev.off()
   # number of empty columns in last row
   empty.col <- (last.row + 1):n.col
   # creat matrix of panel indices
   panel.index <- matrix(
      c(
         rep(1:n.col,2),
         rep(1:2,each=n.col)
      ),
      ncol=2,
      byrow=F
   )
   # choose indices to erase
   panel.mat <- panel.index[(2*last.row+1):(2*n.col),]
   # create string of elements to drop
   rm.str <- c(
      paste0(
         'panel-',
         panel.mat[,1],
         '-',
         panel.mat[,2]
      ),
      paste0('panel-',empty.col,'-2'),
      paste0('strip-t-',empty.col),
      paste0('axis-b-',empty.col),
      paste0('axis-t-',empty.col)
   )
   # identify position of grobs
   rm.pos <- q$layout$name %in% rm.str
   # delete grobs
   q$grobs[rm.pos] <- NULL
   # adjust layout
   q$layout <- q$layout[!rm.pos,]
   # return adjusted object
   return(q)
}


gen.stan.summary <- function(fit, 
                             print.pars, 
                             tmp.path, 
                             refitting = F, 
                             add.to.name = NULL) {
   
   ## FUNCTION TO GENERATE SUMMARIES FROM stanfit OBJECTS
   
   ## Input : fit = fitted model (stanfit object)
   ##         print.pars = parameter names for which summaries 
   ##                     should ne printed
   ## Output: None (saves summary into a text file)
   
   
   # order of time-polynomial
	if ('mu_gamma' %in% fit@model_pars) {
		l.order <- ifelse(fit@par_dims$mu_gamma == 6, 
								'linear',
								ifelse(fit@par_dims$mu_gamma == 9,
										 'quad',
										 'cubic')
		)
	} else {
		l.order <- ifelse(fit@par_dims$gamma[1] == 2, 
								'linear',
								ifelse(fit@par_dims$gamma[1] == 3,
										 'quad',
										 'cubic')
		)
	}
	
   # model specification
   m.spec <- ifelse(fit@model_name == 'Normal-Normal',
                    'nn',
                    ifelse(fit@model_name == 'Beta-Normal',
                           'bn',
                           ifelse(fit@model_name == 'Beta-t',
                                  'bt', fit@model_name)))
   m.name <- paste0(l.order, '.', m.spec)
   
   # generate output and save
   capture.output(
      {
         print(
            fit, 
            pars=print.pars
         )
         
         z <- get_sampler_params(
            fit, 
            inc_warmup=F
         )
         cat(
            '\n\nDivergences per chain:\n'
         )
         print(
            sapply(z, 
                   function(w) sum(w[,'divergent__'])
            )
         )
         cat('\n\nMax Treedepths per chain:\n')
         print(
            sapply(z, 
                   function(w) summary(w[,'treedepth__'])
            )
         )
         
         cat('\n\nSummary of Rhat statistics:\n')
         print(
            summary(fit)$summary[,'Rhat'] %>%
               summary
         )
         
         cat('\n\nSummary of eff. sample sizes:\n')
         print(
            summary(fit)$summary[,'n_eff'] %>%
               summary
         )
         
      },
      file = paste0(
         tmp.path,
         m.name, 
			ifelse(refitting, '.refit',''),
			add.to.name,
         '.txt'
      ),
      type='output'
   )
   
}


## Functions to Analyze Posterior Samples -------------------------------------


gen.agg.trends <- function(fitted.model,
                           time.mat, 
                           post.samps = NULL,
                           mean.only = F,
                           coefs = F) {
   
   ## FUNCTION TO GENERATE SAMPLES FROM PREDICTED (MEAN) TIME TREND
   
   ## INPUT : fitted.model = stanfit object of fitted model
   ##         time.mat     = matirx of measured times (first column should
   ##                        be a column of ones)
   ##         post.samps   = vector of posterior samples to analyze
   ##         mean.only    = whether to analyze only mu_gamma
   ##         coefs        = whether time-trend coefficients
   ##                        should be returned (only applicable if
   ##                        mean.only == TRUE)
   ##
   ## OUTPUT: posterior draw from predicted (mean) time trend
   ##         for each partisan group & the difference between
   ##         Democrats and Republicans
   
   # mu_gammas (hyperparameter, means)
   mu.gamma <- as.matrix(fitted.model, pars = 'mu_gamma')
   
   # number of predictors in model
   K <- fitted.model@par_dims$mu_gamma
   # number of total posterior draws
   n.draw <- nrow(mu.gamma)
   
   # number of posterior samples to analyze
   if (!is.null(post.samps)) {
      samps <- post.samps
      mu.gamma <- mu.gamma[samps,]
   } else samps <- 1:nrow(mu.gamma)
   
   # order of time trend
   O <- ncol(time.mat)
   # index vectors for partisan groups
   # note: indicates the column numbers in mu_gamma and gamma
   #       corresponding to the coefficients of 
   #       republicans, independents, and democrats
   
   rep.cols <- c(1,4:(O + 2))
   ind.cols <- c(2, seq(max(rep.cols) + 1, O*3 - 1, 2))
   dem.cols <- c(3, seq(max(rep.cols) + 2, O*3, 2))
   
   # only aggregate trends?
   if (mean.only) {
      
      m.res <- lapply(
         samps,
         function (w) {
                
            # get trend coefficients for partisan groups
            g <- mu.gamma[w,]
            reps <- g[rep.cols]
            inds <- reps + g[ind.cols]
            dems <- reps + g[dem.cols]
            
            # calculate trends
            a.trends <- inv.logit(
               time.mat %*% cbind(reps,inds,dems)
            ) %>%
               cbind(., time = time.mat[,2])

            # coefficients?
            if (coefs) {
               
               t.coefs <- rbind(reps, inds, dems) %>%
                  cbind(., 1:3)
               colnames(t.coefs) <- c(paste0('coef.',1:O),'pid')
                  
            } else t.coefs <- NULL
         
            return(list(summary = a.trends, 
                        issue = NULL, 
                        coefs = t.coefs))
         
         }
      ) 
      
      return(m.res)
   
   } 
      
      
   # get gammas (individual issues)
   gamma <- as.matrix(fitted.model, pars = 'gamma')
   
   if(!is.null(post.samps)) 
      gamma <- gamma[samps,]
   
   
   # load gammas (reg. params) and transform into matrix
   # rows = issues; cols = coefficients
   gamma.mats <- lapply(
         1:n.draw, 
         function(w) matrix(gamma[w,], ncol = K)
      )
   
   # generate aggregate trends
   trends <- lapply(
      1:n.draw,
      function(w) {
         
         # get gamma and mu.gamma
         g <- gamma.mats[[w]]
         m <- mu.gamma[w,]
         
         ## trend coefficients for partisan groups ##
         
         c.list <- list()
         
         # notes on c.list 
         #
         # 1) list of 3 elements corresponding to pid
         # 2) for each entry of the list
         #    nrow = issues
         #    ncol = intercept, time, time^2, ...
         
         # republicans
         c.list$rep <- g[,rep.cols]
         # independents
         c.list$ind <- c.list$rep + g[, ind.cols]
         # democrats
         c.list$dem <- c.list$rep + g[, dem.cols]
         
         # get time trend on each issue
         # length = pid
         # for each entry:
         #     nrow = time points
         #     ncol = issues
         i.trend <- lapply(c.list, function(w) {
            inv.logit(time.mat %*% t(w))
         })
         
         # number of issues and time points
         n.issues <- ncol(i.trend[[1]])
         n.times <- nrow(time.mat)
         
         # combine into matrix
         # nrow = n.issues * n.times
         # ncol = pid + 2 (i.e., issue.no & time)
         i.mat <- lapply(1:n.issues, function(w) {
            sapply(i.trend, `[`, 1L:n.times, w)
         }) %>% 
            do.call(rbind, .) %>%
            cbind(., 
                  issue = rep(1:n.issues, each = nrow(time.mat)),
                  time  = rep(time.mat[,2], n.issues)
            )
         
         
         # aggregate trend
         a.coefs <- cbind(
            m[rep.cols],
            m[rep.cols] + m[ind.cols],
            m[rep.cols] + m[dem.cols]
         )
         a.trend <- inv.logit(time.mat %*% a.coefs)
         
         # add time column
         a.trend <- cbind(a.trend, time.mat[,2]) 
         colnames(a.trend) <- c('rep','ind','dem','time')
         
         
         return(list(summary = a.trend, issue = i.mat, coefs = NULL))
         
      }
   )
   
   return(trends)
   
}


## Miscellaneous Functions ----------------------------------------------------

# inverse logit transform
inv.logit <- function(x) {
   
   ## LOGISTIC TRANSFORM
   
   ## INPUT : x = real number
   ##
   ## OUTPUT: real number in the interval (0,1)
   
   1/(1+exp(-x))
   
}


gen.posti <- function(lb,ub) {
   
   ## FUNCTION TO PUT PUT INTERVALS IN PARANTHESES
   
   ## INPUT:  lb = lower limit of interval
   ##         ub = upper limit of interval
   ##
   ## OUTPUT: string with (lb, ub) and empty string if lb, and ub are empty
   
   ifelse(lb=='' & ub=='', '', paste0('(',lb,',',ub,')'))
}

get.se.lincom <- function(Sigma, coef.names) {
   
   ## FUNCTION TO CALCULATE STANDARD ERROR OF SUM OF VARIABLES
   
   ## INPUT :  Sigma      = covariance matrix of variables (has to be named)
   ##          coef.names = names of variables to be summed
   ##
   ## Output: standard error of sum
   
   sqrt(sum(Sigma[coef.names,coef.names]))
   
}

## Byte-complie Functions -----------------------------------------------------

gen.summary <- compiler::cmpfun(gen.summary)
drop.empty.facets <- compiler::cmpfun(drop.empty.facets)
gen.stan.summary <- compiler::cmpfun(gen.stan.summary)
gen.agg.trends <- compiler::cmpfun(gen.agg.trends)
inv.logit <- compiler::cmpfun(inv.logit)
gen.posti <- compiler::cmpfun(gen.posti)
gen.se.lincom <- compiler::cmpfun(get.se.lincom)

# store function names
if (!exists('fun.names')) {
   
   fun.names <- c('gen.summary',
                  'drop.empty.facets',
                  'gen.stan.summary',
                  'gen.agg.trends',
                  'inv.logit',
                  'gen.posti',
                  'gen.se.lincom')

   message(
      paste0('\nList of Defined Functions :\n\n', 
             paste0(fun.names, collapse = '\n'),
             '\n\n(stored in object fun.names)')
   )

}
#### END OF CODE ####