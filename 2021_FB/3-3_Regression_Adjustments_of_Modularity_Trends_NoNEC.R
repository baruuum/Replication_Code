
###############################################################################
## 
## Adjusted Trends in Modularity
##
## Creator     : Barum Park
## Last Update : 02/21/2020
##
## Notes 
##
## 1) Changes in the coding scheme of occupations occured in the years
##
##          1983, 1992, 2003, and 2011 
##
##    where the first year is not part of the analysis. For details on how the
##    coding scheme changed, see Kambourov & Manovskii (2013) or consult the
##    BLS website at https://www.bls.gov/cps/cpsoccind.htm
##
###############################################################################

# load libraries
library(igraph)
library(data.table)
library(ggplot2)
library(gridExtra)

if (!exists("p_term"))
    stop("cannot find p_term")

if (!exists("adj_win_vec"))
    stop("cannnot find adj_win_vec")



message("\n\n\n***** Regression adjustments for modularity trends *****\n")


# Read in data and set parameters----------------------------------------------------

message("Loading in Data ...")

# read in modularity data
mod_df = fread(
    here("output", "data", "Figure_E6.csv")
)[
    d_spec == "Not-elsewhere-classified Occupations Excluded" & w_size == 1
][
    , `:=`(year = mid_year,
           mid_year = NULL,
           d_spec = NULL, 
           w_size = NULL, 
           w_modularity = NULL)
]
    
# scheme-change years
change_endyears = c(1992, 2003, 2011)
change_cuts = change_endyears - .5



# Regression Adjustements for Scheme Changes ---------------------------------------

for (w in seq_along(adj_win_vec)) 
{

    for (y in seq_along(change_cuts)) 
    {
        
        # column name to store adjusted results
        adj_name = paste0("adj_mod_", adj_win_vec[w])
        
        # create adj_mod_<window_size> column and populate with unadjusted mod
        if (y == 1)
            mod_df[, (adj_name) := lr_modularity]
        
        # extract data and order according to year
        dat = mod_df[
            abs(change_cuts[y] - year) < adj_win_vec[w]
        ][
            order(year)
        ]
        
        # dummy for pre- and post- years 
        # note: first post-year is the first year after theme change
        dat[, pre_post := ifelse( year < change_cuts[y], 0, 1)]
        
        # center year variable at first post-year
        dat[, c_year := year - change_endyears[y]]
        
        # generate formula
        f = paste0("lr_modularity ~ pre_post + poly(c_year,", p_term,")")
        
        # get predicted modularity for no scheme change
        pred_mod = coef(lm(as.formula(f), data = dat))["(Intercept)"]
        
        # get delta (predicted - observed)
        delta = as.numeric(pred_mod - dat[c_year == 0, "lr_modularity"])
        
        # adjust modularity by adding delta to post-cut years
        mod_df[
            year > change_cuts[y],
            (adj_name) := get(adj_name) + delta
        ]
        
    }
 
}

# reshape data.table for plotting
p_df = melt(
    mod_df, 
    id.vars = "year",
    variable.name = "adjustments",
    value.name = "modularity"
)

# generate labels
tmp = names(table(p_df$adjustments))
new_labs =  c(
    "Observed",
    paste0(
        "Adjusted (Half-window = ",
        sub("(.*)([[:digit:]])$", "\\2", tmp[grepl("adj_mod", tmp)]),
        ")"
    )
)


# plot results
message("\nPlotting scheme-change adjustments (Figure E9) ...")

pdf(here("output", "figures", "Figure_E9.pdf"),
    width = 8, height = 5)
print(
    ggplot(
        p_df, 
        aes(
            x = year,
            y = modularity,
            linetype = adjustments
        )
    ) +
    purrr::map(
        change_cuts, 
        ~ geom_vline(
            aes(
                xintercept = .x),
            col="grey50",
            linetype = 2
        )
    ) + 
    geom_line() +
    scale_linetype_manual(
        name = "",
        values = 1:(length(adj_win_vec) + 1), 
        labels = new_labs
    ) +
    labs(
        x = "\nYear", y = "LinkRank Modularity"
    ) + 
    scale_x_continuous(breaks = seq(1989, 2015, 2)) + 
    fnb_mod_y_axis +
    fnb_theme +
    theme(
        legend.direction = "vertical",
        legend.position = "right",
        legend.justification = "center",
        legend.title = element_blank()
    )
)    
dev.off()

message("File Path : ", here("output", "figures", "Figure_E9.pdf"))

fwrite(p_df, here("output", "data", "Figure_E9.csv"))
message("Data Path : ", here("output", "data", "Figure_E9.csv"))

message("\nDone!")


### END OF CODE ###