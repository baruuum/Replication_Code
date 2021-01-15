#' Reorder labels of matched modules over time based on overlap (forward matching)
#' 
#' Very inefficient and ad-hoc method to reorder labels for plotting purposes
#' 
#' @param mod_df a \code{data.table} object with matched modules and overlap information
#' @param threshold the threshold at which to cut the overlaping proportion
#' @return returns \code{mod_df} with new labels for the modules added and an additional year of observations for plotting purposes
new_mod_labels_forward = function(mod_df, threshold = 0.5)
{
    
    # check names of mod_df
    if (!all(c("org", "dest", "props", "count") %in% names(mod_df)))
        stop("mod_df must have columns org, dest, count, and props")
    
    # minimum and maximum period
    p_min = min(mod_df$period)
    p_max = max(mod_df$period)
    
    ## start matching and labeling, first period
    
    # assign org.labels for first period
    mod_df[period == p_min, org_labels := paste0(org)]
    
    # save the largest label
    max_label = mod_df[period == p_max, max(org)]
    
    # match labels 
    tmp_res = match_modules_forward(
        mod_df, p_min, max_label, threshold, TRUE
    )
    
    # get modified data and new max_label
    mod_df =  tmp_res$data
    max_label = tmp_res$max_label
    
    ## match remaining periods
    
    for (s in (p_min + 1L):p_max) {
        
        # generate lookup table
        l_tab = mod_df[
            period == s - 1L, 
            .(org_labels = unique(dest_labels)), 
            by = dest
        ][
            !is.na(org_labels)
        ] %>%
            setorder(dest)
        
        # check results
        if (
            !isTRUE(
                all.equal(
                    sort(l_tab$dest),
                    sort(mod_df[period == s, unique(org)])
                )
            )
        ) {
            
            message(paste0("Relabeling Forwards: Mapping from period ",
                           s - 1L," to ", s,
                           " is not one-to-one \n",
                           "This should only happen if there exists a ",
                           "module that was not mapped to any other in this",
                           " time interval "))
            
            m_dum = !(mod_df[period == s, unique(org)] %in% l_tab$dest)
            m_mod = mod_df[period==s, unique(org)][m_dum]
            
            for (cc in seq_along(m_mod)) {
                l_tab = rbind(l_tab, list(m_mod[cc], max_label + 1L))
                max_label = max_label + 1L
            }
            
            setorder(l_tab, dest)
        
        }
        
        
        # match labels (org)
        mod_df[period == s, org_labels := l_tab[org]$org_labels]
        
        # match destinations
        tmp_res = match_modules_forward(mod_df, s, max_label, threshold, TRUE)
        mod_df = tmp_res$data
        max_label = tmp_res$max_label
        
        
    }
    
    
    # generate data for last period
    last_p = mod_df[
        period == p_max,
        .(org_labels = dest_labels, org = dest)
    ][
        ,
        `:=`(
            period = p_max + 1, 
            dest = NA, 
            dest_labels = NA,
            props = NA,
            count = NA
        )
    ] %>%
        setcolorder(
            c("org",
              "dest",
              "props",
              "count",
              "period",
              "org_labels",
              "dest_labels")
        )
    # combine data
    df_forward = rbind(mod_df, last_p)   
    
    # readjust periods
    df_forward[period != p_max + 1, period_1 := period + 1]      
    
    # reorder y-axis
    g_levels = order_mod_labs(df_forward)
    
    # merge labels
    df_forward = merge(
        df_forward, 
        g_levels, 
        by.x = "org_labels",
        by.y = "old_labels",
        all.x = T
    ) %>%
        setnames("new_labels", "new_org_labels") %>%
        merge(
            g_levels,
            by.x = "dest_labels",
            by.y = "old_labels",
            all.x = T) %>%
        setnames("new_labels","new_dest_labels")
    
    # transform new labels into factors
    df_forward[, `:=`(
        new_org_labels=factor(new_org_labels,
                              levels = g_levels[[2]],
                              labels = g_levels[[1]]),
        new_dest_labels=factor(new_dest_labels,
                               levels = g_levels[[2]],
                               labels = g_levels[[1]])
        )
    ]
    
    # check whether number of clusters remains invariant to relabeling
    pp = mod_df[, unique(period)]
    tmp_test = rep(NA, length(pp))
    for (p in seq_along(pp)) {
        
        x1 = mod_df[period == pp[p] & !is.na(org), length(unique(org))]
        x2 = df_forward[period == pp[p], length(unique(new_org_labels))]
        tmp_test[p] = (x1 == x2)
    }
    
    if (!all(tmp_test)) {
        stop("relabeling changed cluster sizes!")
    }
    
    return(df_forward)
    
}
