
###############################################################################
## 
## Summarizing modules from three period analysis
##
## Creator     : Barum Park
## Last Update : 02/21/2020
##
###############################################################################


## Basic Configurations -------------------------------------------------------

library(here)
library(igraph)
library(data.table)
library(ggplot2)

## Load datasets & Results ----------------------------------------------------

# check whether p_vec is present
if (!exists("p_vec"))
    stop("cannot find p_vec")

# get original names of occupations
org_labs = fread(here("data", "labmap_NoNEC.csv"))

# change options to capture all warnings
options(nwarning = 50000)

## Summaries of Detected Communities ------------------------------------------

message("\n\n\n***** Summarizing three-period results *****\n")

for (period in p_vec) {
    
    message(paste0("\nSummarizing modules of period ",
                   gen_p_labs(period, full = TRUE),
                   " ..."))
    # path
    dir_path = here("output", "data", "three_period", period)
    
    # get results
    modules = readRDS(
        paste0(dir_path, 
               "/modules_", period, "_NoNEC_weighted_complete5.rds")
    )
    
    # summarize modules
    m_sum = module_summary(modules, org_labs, verbose = 0)
    
    # check warnings
    ws = capture.output(warnings())[-1]
    ws = ws[!grepl("eigen|closeness", ws)]
    
    # stop if any warnings are not related to eigenvector or closeness
    if (length(ws) > 0) {
        
        stop(paste(ws, collapse = "\n"))
        
    } else {
        
        message("All warnings closness and eigenvector centrality related")
        
    }
    
        
    message("Saving module summary ...")
    
    # generate file path
    fn = paste0(dir_path, "/module_summary")
    
    # create directory if needed
    if (!dir.exists(paste0(dir_path, "/module_summary"))) {
        
        message(paste0("Creating module_summary subdirectory into ",
                       dir_path, " ..."))
        
        dir.create(paste0(dir_path, "/module_summary"))
        
    }

    # save module summary into rds-format
    saveRDS(m_sum, paste0(dir_path, "/module_summary.rds"))
    
    # save to csv
    dt_sum = module_summary_to_dt_list(m_sum)
    
    for (f in 1:length(dt_sum))
        fwrite(
            x         = dt_sum[[f]],
            file      = paste0(dir_path, 
                               "/module_summary/module", 
                               f, 
                               ".csv"),
            row.names = FALSE,
            col.names = TRUE
        )
    
}

# removed unnecssary objects
rm(modules, m_sum, dt_sum, ws, org_labs, dir_path)

message("\nDone!")

### END OF CODE ###