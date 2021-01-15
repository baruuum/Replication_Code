###############################################################################
## 
## Compare three-period modules with the Weeden-Grusky class scheme
##
## Creator     : Barum Park
## Last Update : 02/21/2020
##
###############################################################################



if (!exists("p_vec"))
    stop("cannot find p_vec")

# load libraries
library(here)
library(data.table)

message("\n\n\n***** Comparing modules with WG scheme *****\n")

## Add WG classes to old dictionary --------------------------------------------

message("Creating new dictionary occupation id dictionary ...")

occ_dict = fread(here("data", "labmap_NoNEC.csv"))
wg_scheme = readstata13::read.dta13(here("rawdata", "occ_class_scheme.dta")) %>%
    data.table

# check whether all labels are present
if (any(!occ_dict$occ_1990 %in% wg_scheme$occ1990))
    stop("some occupations in dict are not present in wg_scheme")

# merge    
occ_crosswalk = merge(
    occ_dict, 
    wg_scheme[, .(macro_adj, meso_adj, micro_adj, occ1990)], 
    by.x = "occ_1990", 
    by.y = "occ1990", 
    all.x = TRUE)

# check
if (any(is.na(occ_crosswalk$occ_label)))
    stop("some occupation labels are not matched")

# save
message("Save new dictionary (WG classes added) ...")
fwrite(occ_crosswalk, here("data", "labmap_NoNEC_with_WG.csv"))


## Load three-period module data ------------------------------------------------

# load data
mod_list = purrr::map(
    p_vec,
    ~ readRDS(
        here("output", 
             "data", 
             "three_period", 
             .x, 
             paste0("modules_", 
                    .x, 
                    "_NoNEC_weighted_complete5.rds"
             )
        )
    )
)
names(mod_list) = p_vec

## Summarize and compare modules with WG ----------------------------------------
message("Summarizing occupations within each period ...")

# generate full list of occupations and 
# flag occs with highest within-module pagerank
full_occs = purrr::imap(mod_list, function(mod, indx) 
    {
        tmp_df = data.table(
            occ_label  = mod$names, 
            membership = mod$membership, 
            period     = indx
        )
        
        # get centrality data
        c_dat = readRDS(
            here("output", 
                 "data", 
                 "three_period", 
                 indx, 
                 "module_summary.rds"
            )
        )
        
        c_occs = purrr::imap(c_dat, function(w, j)
            {
                # occupation name with highest pagerank
                pr = w$central_occs[
                    which(grepl("pagerank", w$central_occs[, "max_centrality"])),
                    "occ_name"
                ]
                
                data.table(page_rank = pr, module = as.integer(gsub("module_", "", j)))
                
            }
        ) %>%
            rbindlist
        
        # flag occupations with highest pagerank
        tmp_df = merge(
            tmp_df, 
            c_occs, 
            by.x = "membership",
            by.y = "module",
            all.x = TRUE)
        
        tmp_df[, page_rank := ifelse(occ_label == page_rank, 1L, 0L)]
        
        # check results
        if (!isTRUE(
                all.equal(
                    tmp_df[page_rank == 1, .(occ_label, membership)],
                    c_occs, 
                    check.attributes = F
                )
            )
        )
            stop("merger failed")
        
        return(tmp_df)
        
    }
) %>% rbindlist


message("Generating crosstable of modules and W-G classes ...")

# merge full_list with crosswalk and generate tables
dat = merge(full_occs, occ_crosswalk, by = "occ_label", all.x = T)

# turn period into a factor with full period labels
p_labs = gen_p_labs(p_vec, full = T)
dat[, period := factor(period, 
                       levels = p_vec,
                       labels = p_labs)]

tab_list = purrr::map(
    p_labs, ~ table(dat[period == .x, .(membership, meso_adj)])
)


message("Transforming tables into LaTeX format ...")

# labels
class_no = occ_crosswalk$meso_adj %>% unique %>% sort
class_labs = paste0(c(as.roman(class_no), ""), "&")

# write out as LaTeX table
latex_tabs = purrr::map(tab_list, function(tt) 
    {
    
        # arrangement of columns
        ar = rep("c", nrow(tt)) %>%
            paste0(collapse=":") %>%
            paste0("l|",.)
    
        # generate table
        tmp_tab = t(tt) %>% 
            # add margins
            rbind(., colSums(.)) %>% 
            # transpose to reorder by margin counts 
            t %>% 
            # order by margin counts
            .[order(-.[, ncol(tt) + 1L]), ] %>% 
            # transpose back to original format
            t 
        
        c_names = colnames(tmp_tab)
        
        # write out
        apply(tmp_tab, 1L, paste0, collapse = "&") %>% 
            # erase zeros
            gsub("\\&0", "&", .) %>% 
            # erase leading-zeros
            gsub("^0", "", .) %>% 
            # add meso labels
            cbind(class_labs, .) %>% 
            # add lines
            cbind(c(rep("\\\\ \\hdashline", ncol(tt) - 1),
                    rep("\\\\ \\hline", 2))) %>%
            # paste together
            apply(1L, paste0, collapse=" ") %>% 
            # add LaTex commands
            c("\\begin{table}[h]",
              "\\renewcommand{\\arraystretch}{1.1}",
              "\\setlength\\tabcolsep{1.5pt}",
              "\\setlength\\dashlinedash{0.2pt}",
              "\\setlength\\dashlinegap{1.5pt}",
              "\\setlength\\arrayrulewidth{0.3pt}",
              paste0("\\begin{tabular}{|", ar, "|} \\hline"), 
              paste0("&", paste0(c_names, collapse = "&"), "\\\\ \\hline"),
              .,
              "\\end{tabular}",
              "\\end{table}") 
        
    }
        
)


message("\nSaving LaTeX tables ...")

# write-out
for (indx in seq_along(latex_tabs)) {

    if (indx == 1L) {
        
        message(
            paste0(
                "File Path : ",
                here("output", "tables", paste0("Table_2.txt"))
            )
        )
        
        writeLines(
            latex_tabs[[indx]], 
            here("output", "tables", paste0("Table_2.txt"))
        )
        
    }
    
    fp = here("output", "tables", paste0("Table_D4_part", indx, ".txt"))
    
    message(
        paste0(
            "File Path : ",
            fp
        )
    )
              
    writeLines(
        latex_tabs[[indx]], 
        fp
    )
    
}


message("\nSaving summary of all occupations by period and module...")

# flag max page-rank occ
dat[, occ_label_flagged := ifelse(page_rank,
                                  paste0("*", occ_label),
                                  occ_label)]

# summarize by period and module
agg_dat = dat[
    , 
    .(occupations = paste0(occ_label_flagged, collapse = ", ")), 
    by = list(period, membership)
][
    order(period, membership)
]

# save results
message(
    paste0(
        "Data Path : ",
        here("output", "tables", "Table_D1_to_D3.csv")
    )
)

fwrite(agg_dat, here("output", "tables", "Table_D1_to_D3.csv"))

message("\nDone!")

### END OF CODE ###