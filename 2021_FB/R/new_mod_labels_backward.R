#' Reorder labels of matched modules over time based on overlap (backward matching)
#' 
#' Very inefficient and ad-hoc method to reorder labels for plotting purposes
#' 
#' @param mod_df a \code{data.table} object with matched modules and overlap information
#' @param threshold the threshold at which to cut the overlaping proportion
#' @return returns \code{mod_df} with new labels for the modules added and an additional year of observations for plotting purposes
new_mod_labels_backward = function(mod_df, threshold = 0.5)
{
    
    if (!all(c("org", "dest", "props", "count") %in% names(mod_df)))
        stop("mod_df must have columns org, dest, count, and props")
    
    ## DELETE THIS ##
    mod_df = copy(res_back)

    # first and last period
    p_min = min(mod_df$period)
    p_max = max(mod_df$period)
    
    ## start matching and relabeling (first period)
    
    # assign labels for first period
    mod_df[period == p_min, org_labels := paste0(org)]
    
    # save largest label
    max_label = mod_df[period == p_min & !is.na(org), max(org)]
    
    # match clusters for first period
    tmp_res = match_modules_backward(
        mod_df, p_min, p_min, max_label, threshold, TRUE
    )
    
    mod_df = tmp_res$data
    max_label = tmp_res$max_label
    
    ## match the remaining periods
    for (s in (p_min + 1):p_max) {
        
        # generate lookup table from previous period
        l_tab = mod_df[period == s - 1, .(dest_labels = unique(dest_labels)), by = dest]
        
        # match column `org` in mod_df
        if (!all(sort(l_tab$dest), sort(mod_df[period == s, unique(org)])))
            stop("mismatch between org and l_tab")
        
        # match origins
        mod_df[period == s, org_labels := l_tab[org]$dest_label]
        
        # check matching
        if (any(is.na(mod_df[period == s]$org_labels))) 
            stop("Unmatched modules found in backwars matching")
    
        # match destinations
        tmp_res = match_modules_backward(
            mod_df, s, p_min, max_label, threshold, TRUE
        )
        mod_df = tmp_res$data
        max_label = tmp_res$max_label
        
    }
    
    # generate data for first period
    first_p = mod_df[
        period == p_min, .(dest_labels = org_labels, dest = org)
    ][
        ,`:=`(period     = p_min - 1, 
              org        = NA,
              org_labels = NA, 
              props      = NA,
              count      = NA)
    ] %>%
      setcolorder(
          c("org",
            "dest",
            "props",
            "count",
            "period",
            "org_labels",
            "dest_labels")
      ) %>%
        na.omit("dest")
    
    df_backward = rbind(first_p, mod_df)   
    df_backward[period != p_min-1, period_1 := period - 1]      
    
    
    # reorder y-axis
    g_levels = order_mod_labs(df_backward)
    
    # merge labels
    df_backward = merge(
        df_backward, 
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
            all.x = T
        ) %>%
        setnames("new_labels", "new_dest_labels")
    
    # transform new labels into factors
    df_backward[, `:=`(
            new_org_labels = factor(
                new_org_labels,
                levels = g_levels[[2]],
                labels = g_levels[[1]]
            ),
            new_dest_labels = factor(
                new_dest_labels,
                levels = g_levels[[2]],
                labels = g_levels[[1]]
            )
        )
    ]

    # check whether number of clusters remains invariant to relabeling
    pp = mod_df[, unique(period)]
    tmp_test = rep(NA, length(pp))
    for (p in seq_along(pp)) {
        
        x1 = mod_df[period == pp[p] & !is.na(org), length(unique(org))]
        x2 = df_backward[period == pp[p], length(unique(new_org_labels))]
        tmp_test[p] = (x1 == x2)
    }
    
    if (!all(tmp_test)) {
        stop("relabeling changed module sizes")
    }

    return(df_backward)
    
}