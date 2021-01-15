
###############################################################################
## 
## Analysis of skill requirements
##
## Creator     : Barum Park
## Last Update : 02/21/2020
##
###############################################################################


## Basic Setup ----------------------------------------------------------------

# load libraries
library(here)
library(igraph)
library(data.table)
library(ggplot2)

if (!requireNamespace("readstata13", quietly = TRUE))
    stop("package readstata13 has to be installed")

if (!exists("w_size_vec"))
    stop("cannot find w_size_vec")

message("\n\n\n***** Analyzing skills requirements of ocupations *****\n")

## Load Data and Merge with Clusters ------------------------------------------

message("Loading data ...")

# load occupational level skill data
skill_dat = readstata13::read.dta13(
    here("rawdata", "occlevel_skills.dta"), 
    convert.factors = FALSE
) %>%
    data.table %>%
    setcolorder("occ1990") %>%
    # add `s_` in front of all skill columns
    setnames(
        c("occ_1990", paste0("s_",names(.)[2:ncol(.)]))
    )

# get occupation-scheme mapping
labmap = fread(here("data", "labmap_NoNEC_with_WG.csv"))

# wg-class names
class_names = c("macro", "meso", "micro")

# name of partitions
p_names = c("module", class_names)

# colors for the lines (Mobility and the rest)
p_cols = c("black", rep("grey50", 3L))

# name of skill vars
s_names = grep("^s_", names(skill_dat), value = T)

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

# create dataset for regression
reg_skill_dat = merge(
    mod_dat,
    skill_dat,
    by = "occ_1990",
    all.x = TRUE
) %>%
    # drop missing cases
    na.omit



# Analyzing skill measures -------------------------------------------------------

message("\nAnalyzing skill measures ...")

# years
years = reg_skill_dat$year %>% unique %>% sort

# fit models
skill_res = purrr::map(p_names, function(pp) 
    {
    
        res = purrr::map(s_names, function(s) 
            {
        
                # generate formula for regression
                f = as.formula(paste0(s, "~ factor(", pp, ")"))
                
                # run regression for each year and extract r-squared
                fitstat_res = purrr::map(years, function(y) 
                    {
                    
                        # year-specific data
                        dd = reg_skill_dat[year==y]
                        
                        # fit linear model
                        fit = lm(f, data=dd) 
                        
                        # get r-squared
                        r2 = fit %>%
                            summary %>%
                            `[`("r.squared") %>%
                            unlist 
                        
                        # drop name attribute
                        names(r2) = NULL
                        
                        # get ICs and return
                        return(
                            c(r2 = r2, 
                              aic = AIC(fit), 
                              bic = BIC(fit), 
                              year = y)
                        ) 
                
                    }
                
                ) %>% 
                    do.call("rbind", .) %>%
                    data.table %>%
                    .[,skill := s]
        
                return(fitstat_res)
        
            }
            
        ) %>% 
            rbindlist
    
        res[, partition := pp]

        return(res)
    
    }
    
) %>% 
    rbindlist 

# preparing for plotting
skill_res[
    # drop "s_" prefix
    , skill := sub("^s_", "", skill)
][
    # change first character to upper case
    ,skill := upper_first_char(skill, TRUE)
][
    # change names of skills to be interpretable
    skill == "Computer", skill := "Computer, General Usage"
][
    skill == "Sciandeng", skill := "Science & Engineering"
][
    skill == "Techmisc", skill := "Misc. Technical"
]

# melt into long format
skill_df_long = melt(
    skill_res,
    id.vars = c("skill", "partition", "year")
)[
    # change partition into factor
    , partition := factor(
        partition,
        levels = p_names,
        labels = c("Mobility", upper_first_char(class_names))
    )
]

# new plotting order for skill measures
s_order = c(
    "Verbal", 
    "Quantitative",
    "Analytic",
    "Creative", 
    "Programming", 
    "Computer, General Usage", 
    "Science & Engineering",
    "Misc. Technical",
    "Managerial",
    "Carework" 
)
skill_df_long[, skill := factor(skill, levels = s_order)]


message("\nPlotting r-squared statistics (Figure 8a) ...")

pdf(here("output", "figures", "Figure_8a.pdf"), width = 14, height = 7)
print(
    ggplot(
        skill_df_long[variable == "r2"],
        aes(
            x = year,
            y = value, 
            col = partition,
            linetype = partition
        )
    ) + 
    geom_line() + 
    facet_wrap( ~ skill, nrow = 2) + 
    scale_color_manual(
        name = "",
        values = p_cols,
        guide = guide_legend(byrow = T)
    ) +
    scale_linetype_manual(
        name = "",
        values = 1:4,
        guide = guide_legend(byrow = T)
    ) +
    scale_y_continuous(
        name = "R-squared",
        limits=c(.2, .9),
        breaks=c(.2, .5, .8)
    ) +
    scale_x_continuous(
        name = "\n ",
        breaks = seq(1991, 2013, 2)
    ) +
    fnb_theme +
    theme(
        strip.text = element_text(face = 1, size = 14),
        legend.text = element_text(size = 12),
        legend.position = c(.01, .55),
        legend.justification = c(0, 0),
        legend.title = element_blank(),
        legend.direction = "vertical",
        axis.text.x = element_text(vjust = .5, angle = 45)
    )
)
dev.off()

message(paste0("File Path : ", here("output", "figures", "Figure_8a.pdf")))
fwrite(skill_df_long[variable == "r2"], here("output", "data", "Figure_8a.csv"))
message(paste0("Data Path : ", here("output", "data", "Figure_8a.csv")))


message("\nPlotting BIC statistics (Figure 8b) ...")

pdf(here("output", "figures", "Figure_8b.pdf"), width = 14.1, height = 7)
print(
    ggplot(
        skill_df_long[variable == "bic"],
        aes(
            x = year,
            y = value, 
            col = partition,
            linetype = partition
        )
    ) + 
    geom_line() + 
    facet_wrap( ~ skill, nrow = 2) + 
    scale_color_manual(
        name = "",
        values = p_cols
    ) +
    scale_linetype_manual(
        name = "",
        values = 1:4
    ) +
    scale_y_continuous(
        name = "BIC"
    ) +
    scale_x_continuous(
        name = "\nYear (Mid-Year of Moving Window)",
        breaks = seq(1991, 2013, 2)
    ) +
    fnb_theme +
    theme(
        strip.text = element_text(face = 1, size = 14),
        legend.position = "none",
        axis.text.x = element_text(vjust = .5, angle = 45)
    )
)
dev.off()

message(paste0("File Path : ", here("output", "figures", "Figure_8b.pdf")))
fwrite(skill_df_long[variable == "bic"], here("output", "data", "Figure_8b.csv"))
message(paste0("Data Path : ", here("output", "data", "Figure_8b.csv")))


message("\nPlotting AIC statistics (Figure E12) ...")

pdf(here("output", "figures", "Figure_E12.pdf"), width = 14.1, height = 7)
print(
    ggplot(
        skill_df_long[variable == "aic"],
        aes(
            x = year,
            y = value, 
            col = partition,
            linetype = partition
        )
    ) + 
        geom_line() + 
        facet_wrap( ~ skill, nrow = 2) + 
        scale_color_manual(
            name = "",
            values = p_cols
        ) +
        scale_linetype_manual(
            name = "",
            values = 1:4
        ) +
        scale_y_continuous(
            name = "AIC"
        ) +
        scale_x_continuous(
            name = "\nYear (Mid-Year of Moving Window)",
            breaks = seq(1991, 2013, 2)
        ) +
        fnb_theme +
        theme(
            strip.text = element_text(face = 1, size = 14),
            legend.text = element_text(size = 12),
            legend.position = c(.005, .01),
            legend.justification = c(0, 0),
            legend.title = element_blank(),
            legend.direction = "vertical",
            axis.text.x = element_text(vjust = .5, angle = 45)
        )
)
dev.off()

message(paste0("File Path : ", here("output", "figures", "Figure_E12.pdf")))
fwrite(skill_df_long[variable == "aic"], here("output", "data", "Figure_E12.csv"))
message(paste0("Data Path : ", here("output", "data", "Figure_E12.csv")))


#### END OF CODE ####