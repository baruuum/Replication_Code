###############################################################################
## 
## Analysis of skill requirements
##
## Creator     : Barum Park
## Last Update : 02/25/2020
##
###############################################################################


# Basic Setup -----------------------------------------------------------------

# load libraries
library(here)
library(igraph)
library(data.table)
library(ggplot2)

if (!requireNamespace("readstata13", quietly = TRUE))
    stop("package readstata13 has to be installed")

if (!exists("w_size_vec"))
    stop("cannot find w_size_vec")

# wg-class names
class_names = c("macro", "meso", "micro")

# name of partitions
p_names = c("module", class_names)

# colors for the lines (Mobility and the rest)
p_cols = c("black", rep("grey50", 3L))

message("\n\n\n***** Analyzing individual earnings *****\n")



# Load Data and Merge with Clusters -------------------------------------------

message("Loading data ...")

# CPS and Skill Data

# individual level outcomes (ASEC)
ind_dat = readstata13::read.dta13(
    here("rawdata", "ASEC_individual_level.dta"),
    convert.factors = FALSE
) %>% 
    data.table %>%
    setnames("occ1990", "occ_1990") %>%
    setcolorder("occ_1990")

# get occupation-scheme mapping
labmap = fread(here("data", "labmap_NoNEC_with_WG.csv"))

# load modules (NoNEC/complete5)
mod_dat = readRDS(
    here("data", "moving_window", "MV5_NoNEC_complete5.rds")
) %>%
    # turn into data.table
    # note: imap works as the list is named as "midyear_<year>"
    purrr::imap(
        function(w, indx) 
        {
            data.table(
                occ_label = w$names,
                module = w$membership,
                year = as.integer(gsub("midyear_", "", indx))
            )
        }
    ) %>%
    # rbind data.tables
    rbindlist %>%
    # add occ_1990 labels and WG-scheme to dataset
    merge(
        labmap[, .(occ_label, occ_1990, macro_adj, meso_adj, micro_adj)], 
        by="occ_label",
        all.x = TRUE
    ) %>%
    # rename wg-classes
    setnames(paste0(class_names, "_adj"), class_names)

# check for missing values
if (any(is.na(mod_dat)))
    stop("missing values in mod_dat")

# individual level wage data
reg_ind_dat = merge(
    ind_dat[
        , .(occ_1990, 
            year, 
            log_adj_wageinc,
            age, 
            age_sq,
            sex, 
            edu_4cate, 
            race_3cate)
        ], 
    mod_dat, 
    by = c("occ_1990", "year"),
    all.x = TRUE
)

# create two datasets for wages: 
#
# in addition to dropping missings for which occupational partitions could not be 
# defined, create
#
# 1) a dataset in which only missing values for earnings are dropped
reg_ind_dat_wages = na.omit(
    reg_ind_dat[, c("log_adj_wageinc", "year", p_names), with = FALSE]
)

# and 2) another dataset in which missing values for all variables are dropped
reg_ind_dat_all = na.omit(reg_ind_dat)

# analyzed years
years = reg_ind_dat_wages$year %>% unique %>% sort



# Analyzing earnings -------------------------------------------------------------

message("\nAnalyzing annual earnings by year...")

inc_res = purrr::map(p_names, function(pp) 
    {
    
        # generate formula for regression
        f = as.formula(paste0("log_adj_wageinc ~ factor(", pp, ")"))
        
        # run regression for each year and extract r-squared
        fitstat_res = purrr::map(years, function(y) 
            {
            
                # year-specific data
                reg_dat = reg_ind_dat_wages[year == y]
                
                # check data
                if (any(is.na(reg_dat)))
                    stop("missing values in reg_dat")
                
                # fit regression
                fit = lm(f, data = reg_dat) 
                
                # get r-squared
                r2 = get_r_squared(fit)
                
                return(
                    c(r2   = r2, 
                      aic  = AIC(fit),
                      bic  = BIC(fit),
                      n    = nrow(reg_dat), 
                      year = y)
                )
            
            }
        
        ) %>% 
            do.call("rbind", .) %>%
            data.table
    
        fitstat_res[, partition := pp]
        
        return(fitstat_res)
        
    }
    
) %>% 
    rbindlist 

# change partition into factor
inc_res[
    , partition := factor(
        partition,
        levels = p_names,
        labels = c("Mobility", upper_first_char(class_names))
    )
][
    # scale bic to have mean zero each year
    , sbic := bic - mean(bic), by = year
][
    # scale aic to have mean zero each year
    , saic := aic - mean(aic), by = year
]


message("\nPlotting r-squared and BIC over time (Figure 9) ...")


f9_a = ggplot(
    inc_res,
    aes(
        x = year,
        y = r2,
        col = partition,
        linetype = partition
    )
) +
    geom_line() +
    scale_color_manual(
        name = "",
        values = p_cols
    ) +
    scale_linetype_manual(
        name = "",
        values = 1:4
    ) +
    scale_x_continuous(
        name = "",
        breaks = seq(1991, 2013, 2),
        expand = c(.025,.025)
    ) +
    scale_y_continuous(
        name = "",
        limits = c(.07, .28),
        breaks = seq(0.05, .3, .05)
    ) +
    fnb_theme + 
    theme(
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.justification = "right",
        legend.margin = margin(rep(0, 4)),
        axis.text.x = element_text(angle = 45,
                                   hjust = .5,
                                   vjust = .5)
    ) + 
    ggtitle("R-squared")


f9_b = ggplot(
    inc_res,
    aes(
        x = year,
        y = sbic, 
        fill = partition,
        group = partition
    )
) + 
    geom_vline(
        xintercept = seq(1991, 2012, 1) + .5,
        linetype = 2,
        alpha = .15,
        size = .25
    )+
    geom_bar(
        stat = "identity",
        position = position_dodge(width = .75),
        width = .75,
        col = "black",
        alpha = .75
    ) +
    scale_x_continuous(
        name = "",
        breaks = seq(1991, 2013, 1),
        expand = c(0.01, 0.01)
    ) +
    scale_y_continuous(
        name = "", 
        limits = c(-6000, 4000)
    ) +
    scale_fill_grey(start = 0, end = 1) +
    fnb_theme + 
    theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.justification = "right",
        legend.key.size = unit(1.2, "char"),
        legend.margin = margin(rep(0, 4)),
        axis.text.x = element_text(angle = 45,
                                   hjust = .5,
                                   vjust = .5)
    ) +
    ggtitle("BIC (scaled)")


pdf(here("output", "figures", "Figure_9.pdf"), width = 12, height = 4)
grid.arrange(f9_a, f9_b, nrow = 1, widths = c(.4, .6))
dev.off()

message(paste0("File Path : ", here("output", "figures", "Figure_9.pdf")))
fwrite(
    inc_res[, -c("aic", "bic", "saic", "n"), with = FALSE], 
    here("output", "data", "Figure_9.csv")
)
message(paste0("Data Path : ", here("output", "data", "Figure_9.csv")))


message("\nPlotting (scaled) AIC statistics (Figure E14) ...")

pdf(here("output", "figures", "Figure_E14.pdf"), width = 10, height = 4)
print(
    ggplot(
        inc_res,
        aes(
            x = year,
            y = saic, 
            fill = partition,
            group = partition
        )
    ) + 
    geom_vline(
        xintercept = seq(1991, 2012, 1) + .5,
        linetype = 2,
        alpha = .15,
        size = .25
    )+
    geom_bar(
        stat = "identity",
        position = position_dodge(width = .75),
        width = .75,
        col = "black",
        alpha = .75
    ) +
    scale_x_continuous(
        name = "",
        breaks = seq(1991, 2013, 1),
        expand = c(0.01, 0.01)
    ) +
    scale_y_continuous(
        name = "AIC (scaled)", 
        limits = c(-6000, 4000)
    ) +
    scale_fill_grey(start = 0, end = 1) +
    fnb_theme + 
    theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.justification = "right",
        legend.key.size = unit(1.2, "char"),
        legend.spacing.x = unit(.5, "char"),
        legend.margin = margin(rep(0, 4)),
        axis.text.x = element_text(angle = 45,
                                   hjust = .5,
                                   vjust = .5)
    ) 
)
dev.off()


message(paste0("File Path : ", here("output", "figures", "Figure_E14.pdf")))
fwrite(
    inc_res[, c("saic", "year", "partition"), with = FALSE], 
    here("output", "data", "Figure_E14.csv")
)
message(paste0("Data Path : ", here("output", "data", "Figure_E14.csv")))



# Calculate incremental r-squared ------------------------------------------------


message("\nCalculating incremental r-squared values (after adding controls) ...")

r2_diff_res = purrr::map(p_names, function(pp) 
    {
    
        # generate to formulae
        
        cont_vars = "age + age_sq + factor(sex) + factor(edu_4cate) + race_3cate"
        
        # 1) one with only controls
        f_control = as.formula(
            paste0("log_adj_wageinc ~ ", cont_vars)
        )
        
        # and 2) one with partitions added
        f_part = as.formula(
            paste0("log_adj_wageinc ~ ", cont_vars, "+ factor(", pp, ")")
        )
            
        # run regression for each year and extract r-squared difference
        fitstat_res = purrr::map(years, function(y) 
            {
            
                # year-specific data
                reg_dat = reg_ind_dat_all[year == y]
                
                # check data
                if (any(is.na(reg_dat)))
                    stop("missing values in reg_dat")
                
                # fit regression on controls
                fit_control = lm(f_control, data = reg_dat) 
                
                # fit regression with partitions
                fit_part = lm(f_part, data = reg_dat)
                
                # return r-squared difference
                return(
                    c(r2   = get_r_squared(fit_part) - get_r_squared(fit_control),
                      n    = nrow(reg_dat), 
                      year = y)
                )
            
            }
        
        ) %>% 
            do.call("rbind", .) %>%
            data.table
        
        fitstat_res[, partition := pp]
        
        return(fitstat_res)
    
    }

) %>% 
    rbindlist 

# change partition into factor
r2_diff_res[
    , partition := factor(
        partition,
        levels = p_names,
        labels = c("Mobility", upper_first_char(class_names))
    )
]



message("\nPlotting incremental r-squared (Figure E13) ...")

pdf(here("output", "figures", "Figure_E13.pdf"),
    width = 7, height = 6)
print(
    ggplot(
        r2_diff_res,
        aes(
            x = year,
            y = r2,
            col = partition,
            linetype = partition
        )
    ) +
    geom_line() +
    scale_color_manual(
        name = "",
        values = p_cols
    ) +
    scale_linetype_manual(
        name = "",
        values = 1:4
    ) +
    scale_x_continuous(
        name = "",
        breaks = seq(1991, 2013, 2)
    ) +
    scale_y_continuous(
        name = expression(paste(Delta, "R-squared")),
        limits = c(0, .125)
    ) +
    fnb_theme +
    theme(
        legend.position=c(.02,.02),
        legend.justification=c(0,0)
    )
)
dev.off()

message(paste0("File Path : ", here("output", "figures", "Figure_E13.pdf")))
fwrite(
    r2_diff_res[, -c("n"), with = FALSE], 
    here("output", "data", "Figure_E13.csv")
)
message(paste0("Data Path : ", here("output", "data", "Figure_E13.csv")))

message("\nDone!")


#### END OF CODE ####
