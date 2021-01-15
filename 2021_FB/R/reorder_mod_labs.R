order_mod_labs = function(mod_df) 
{
    
    # label vector
    l_vec = mod_df[
        , unique(c(org_labels, dest_labels))
    ] %>%
        na.omit
    
    # check whether there are parentheses in l_vec
    p_in_vec = sum(grepl("\\(",l_vec)) > 0
    
    # split label vector
    tmp_split = strsplit(l_vec, "\\.") 
    
    # maximum length of label
    max_length = purrr::map_int(tmp_split, length) %>% max
    
    # add zeros to labels of length less than max_length
    g_levels = purrr::map(tmp_split, function(x) {
        c(x, rep(0, max_length- length(x)))
    }) %>%
        do.call("rbind",.) %>%
        data.table
    
    # dealing with parentheses
    if (p_in_vec) {
        g_levels = purrr::map(seq_along(g_levels), function(w) 
            {
            
                # split values by "(" and erase ")"
                tmp = strsplit(g_levels[[w]],"\\(") %>% 
                    purrr::map(~ gsub("\\)", "", .x))
                
                # save max length of results
                t_max_length = purrr::map_int(tmp, length) %>% max
                
                # fill zeros and combine
                t1 = lapply(tmp, function(x) {
                    c(x, rep(0, t_max_length - length(x)))
                }) %>%
                    do.call("rbind",.) %>%
                    data.table
                
            }
        ) %>% 
            do.call("cbind", .)
    }
    
    
    # get columns with alphabetic characters
    a_cols = purrr::map_lgl(
        g_levels, ~ sum(grepl("[[:alpha:]]", .x)) > 0
    ) 
    
    if (any(a_cols)) {
        
        g_levels = g_levels[
            , 
            lapply(.SD, function(a) 
                {
                    w = suppressWarnings(as.numeric(a))
                    w[is.na(w)] = purrr::map_dbl(
                        x[is.na(w)], 
                        ~ (utf8ToInt(tolower(.x)) - utf8ToInt("a") + 1L) / 100
                        
                    )
                    
                    return(w)
                
                }
                
            )
        ]
    }
    
    # change elements to numeric vectors
    g_levels = g_levels[, lapply(.SD, as.numeric)]
    
    # order columns
    g_ord = c(
        which(names(g_levels) == "V1"),
        which(names(g_levels) != "V1")
    )
    setcolorder(g_levels, g_ord)
    
    # generate column labels
    col_labs = paste0("v", seq_along(g_levels) + 1L)
    
    # combine label vector with splitted version and sort results
    g_levels = cbind(l_vec, g_levels) %>%
        setnames(c("old_labels", col_labs)) %>%
        setorderv(col_labs)
    
    g_levels = g_levels[
        , new_labels := 1:nrow(g_levels)
    ][ 
        , list(old_labels, new_labels)
    ]
    
    return(g_levels)
    
}
