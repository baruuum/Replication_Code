###############################################################################
##                                                                           ##
##  Comparing Models using LOOIC and WAIC                                    ##
##                                                                           ##
###############################################################################


## Basic Setup ----------------------------------------------------------------

# check wd.base
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


## Load Models and Compare using WAIC and LOO ---------------------------------

# send out message

message('\n\n\nComparing Models with LOOIC and WAIC ...')

for (domain in 1:4) {
   ### Calculate WAIC and LOO
   
   # path to domain
   domain.path <- paste0(stan.path, 'domain_', domain, '/')
   
   # get convergence summaries
   tmp.str <- paste0(domain.path, 
                     'd', 
                     domain, 
                     '.conv.summary.updated.txt')
   
   if (exists(tmp.str)) {
      conv.sum <- fread(tmp.str)
   } else conv.sum <- fread(sub('\\.updated','',tmp.str))
      
   # select only models which have converged
   m.names <- conv.sum[med.rhat < 1.1 & med.neff > 3000, model]
   
	message(paste0('\n\nWorking on domain ', domain, ' ...'))
	
   if (length(m.names) != nrow(conv.sum)) {
      
      message(
         paste0('\nComparing only models for which Rhat and Neff ',
                'statistics were okay ... \n',
                'This leaves out the following model(s): \n\n',
                paste0(conv.sum[!(model %in% m.names), model],
                       collapse='\n'),
					'\n'
         )
      )
      
   } else {
      
      message('Comparing all fitted models ...')
      
   }
                
   
   # read results
   m.list <- lapply(
      m.names, function(w) {
         readRDS(paste0(domain.path, 'd', domain, '.', w,'.rds'))
      })
   
   # extract log-likelihoods (all saved under name "log_lik")
   ll.list <- lapply(m.list, extract_log_lik)
   
   # calcualte WAIC
   waic.list <- lapply(ll.list, waic)
   names(waic.list) <- m.names
   
   beta.n <- grep('\\.bn', m.names, value = T)
   waic.list[beta.n]
   
   # calculate LOOIC
   loo.list <- lapply(ll.list, loo, cores = 1)
   names(loo.list) <- m.names
   
   # save objects
   saveRDS(waic.list, paste0(domain.path, 'd', domain, '.waic.list.rds'))
   saveRDS(loo.list, paste0(domain.path, 'd', domain, '.loo.list.rds'))
   
   # print results
   cat('WAIC & LOOIC ------------------\n')
   print(waic.compare <- compare(x = waic.list))
   print(loo.compare <- compare(x = loo.list))
   
   ### Generate tables
   
   # if GOF file exists, delete it
   if (file.exists(paste0(domain.path, 'goflatex.txt'))) {
      file.remove(paste0(domain.path, 'goflatex.txt'))
   }
   
   # extract waic and se
   w.table <- as.data.table(waic.compare[, c('waic', 'se_waic')],
                            keep.rownames = 'model')
   
   # extract loo and se
   l.table <- as.data.table(loo.compare[, c('looic', 'se_looic')],
                            keep.rownames = 'model')
   
   # merge waic and loo tables
   gof.res <- merge(w.table,
                    l.table,
                    by = 'model')
   
   # extract time trend
   gof.res[, time.trend := gsub('^(.*)(\\..*)',
                                '\\1',
                                model)]
   # change name to upper case
   gof.res[, time.trend :=
              paste0(
                 toupper(substr(time.trend, 1, 1)),
                 substr(time.trend, 2, nchar(time.trend))
              )]
   
   # change abbreviations
   gof.res[, time.trend := ifelse(time.trend == 'Quad',
                                  'Quadratic',
                                  time.trend)]
   # get model specification
   gof.res[, spec := gsub('(.*\\.)(.*)', '\\2', model)]
   
   # distribution of outcome
   gof.res[, outcome := ifelse(substr(spec, 1, 1) == 'b',
                               'Beta',
                               'Normal')]
   
   # drop unnecssary columns
   gof.res <- gof.res[, .(time.trend, 
                          outcome, 
                          looic, 
                          se_looic, 
                          waic, 
                          se_waic)
                      ]
   
   # reorder rows based on LOOIC and WAIC
   gof.res <- gof.res[order(looic, waic)]
   
   # round numbers
   ic.cols <- grep('ic$', names(gof.res), value=T)
   se.cols <- grep('se\\_', ic.cols, value=T)
   gof.res[, (ic.cols) := lapply(.SD, round, dig), .SDcols = ic.cols]
   # put standard erros in parentheses
   gof.res[, (se.cols) := 
              lapply(.SD, function(x) {
                 paste0('(', x, ')')
                 }
              ), 
           .SDcols = se.cols]
   # set column names
   setnames(gof.res,
            c('Time Trend',
              'Outcome Dist.',
              'LOOIC',
              '(s.e.)',
              'WAIC',
              '(s.e.)')
            )
   
	# save object
	saveRDS(gof.res, paste0(domain.path, 'd', domain, '.gof.rds'))
	
   # generate Latex code
   capture.output({tex.code <- xtable(gof.res, 
                      align = c('l', 'l', 'l', 'r', 'l', 'r', 'l')
                      ) %>% 
                    print(include.rownames = F)},
                  file=ifelse(.Platform$OS.type=='windows',
                              'NUL','/dev/null')
                  )
						
   # add zeros to match widths
   tex.code <- gsub('(\\([1-9]*)(\\))', '\\1\\.00\\)', tex.code)
   tex.code <- gsub('(\\.[1-9])(\\))', '\\10\\)', tex.code)
   
   # print into file
   capture.output( 
      cat(tex.code),
      file = paste0(domain.path,
                    'd', domain, '.',
                    'gof.latex.txt'
      )
   )

   rm(m.list, ll.list, waic.list, loo.list, tex.code, gof.res)
   gc()
   
}

#### END OF CODE ####
