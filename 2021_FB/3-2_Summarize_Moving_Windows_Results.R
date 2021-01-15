###############################################################################
## 
## Summarize moving window analysis results
##
## Creator     : Barum Park
## Last Update : 02/21/2020
##
###############################################################################


# Basic Configurations --------------------------------------------------------

if (!exists("w_size_vec"))
    stop("cannot find w_size_vec")

message("\n\n\n***** Summarizing moving window results *****\n") 

# data path
data_path = here("data", "moving_window")



# Load data -------------------------------------------------------------------

message("Loading results ...")

# NoNEC
nonec_mv = purrr::map(
    w_size_vec, ~ readRDS(paste0(data_path, "/MV", .x, "_NoNEC.rds"))
)

# FullOccs
fulloccs_mv = purrr::map(
    w_size_vec, ~ readRDS(paste0(data_path, "/MV", .x, "_FullOccs.rds"))
)

# Female
female_mv = purrr::map(
    w_size_vec, ~ readRDS(paste0(data_path, "/MV", .x, "_Female_NoNEC.rds"))
)

# Male
male_mv = purrr::map(
    w_size_vec, ~ readRDS(paste0(data_path, "/MV", .x, "_Male_NoNEC.rds"))
)

# OCC2000
occ2000_mv = purrr::map(
    w_size_vec, ~ readRDS(paste0(data_path, "/MV", .x, "_NoNEC_OCC2000.rds"))
)

# reweighted by self-loops
rew_mv = purrr::map(
    w_size_vec, ~ readRDS(paste0(data_path, "/MV", .x, "_NoNEC_reweighted.rds"))
)

# Number of modules over time -------------------------------------------------

message("\nPlotting number of modules over time (Figure E3) ...")

n_mods_mv = purrr::map(
    nonec_mv, 
    ~ purrr::map_int(.x, length)
) %>%
    purrr::imap(
        ~ data.table(
            year = as.integer(gsub("midyear_", "", names(.x))),
            n_mods = .x,
            w_size = w_size_vec[.y]
        )
    ) %>%
    rbindlist


# plot (Figure_E3)
pdf(here("output", "figures", "Figure_E3.pdf"),
    width = 7.5, height = 5)
print(
    ggplot(
        n_mods_mv, 
        aes(x = year, y = n_mods, linetype = factor(w_size))
    ) + 
    geom_line() +
    labs(
        x = "\n(Mid-Year of Moving Window)", 
        y = "Number of Modules Detected\n"
    ) + 
    scale_linetype_manual(
        name = "Window Size",
        values = seq_along(w_size_vec)
    ) + 
    scale_x_continuous(
        breaks = seq(1989, 2015, 2)
    ) +
    fnb_theme
)
dev.off()

message(paste0("File Path : ", here("output", "figures", "Figure_E3.pdf")))

# save data
fwrite(n_mods_mv, here("output", "data", "Figure_E3.csv"))
message(paste0("Data Path : ", here("output", "data", "Figure_E3.csv")))
    



# Analyzing trends in modularity ----------------------------------------------

message("\nPlotting modularity over time (NoNEC, Figure E4) ...")

# create data.table out of modularity measures
nonec_df = purrr::map(
    nonec_mv,
    mv_to_modularity_dt,
    "both"
) %>%
    purrr::imap(~ .x[, w_size := w_size_vec[.y]]) %>%
    rbindlist

# plot for alternative measure of modularity (Figure E4)
pdf(here("output", "figures", "Figure_E4.pdf"),
    width = 7.5, height = 5)
print(
    ggplot(
        nonec_df,
        aes(
            x = mid_year, 
            y = w_modularity,
            linetype = factor(w_size)
        )
    ) + 
    geom_line() + 
    scale_x_continuous(
        breaks = seq(1989, 2015, 2)
    ) +
    fnb_mod_y_axis + 
    scale_linetype_manual(
        name = "Window Size",
        values = seq_along(w_size_vec)
    ) + 
    labs(
        y = "Weighted Modularity\n",
        x = "\nYear (Mid-Year of Moving Window)"
    ) +
    fnb_theme 
)
dev.off()

message(paste0("File Path : ", here("output", "figures", "Figure_E4.pdf")))

# save data
fwrite(nonec_df, here("output", "data", "Figure_E4.csv"))
message(paste0("Data Path : ", here("output", "data", "Figure_E4.csv")))

        
message("\nPlotting comparison with FullOccs(Figure E6) ...")
        
# create data.table out of modulularity measures for FullOccs
fulloccs_df = purrr::map(
    fulloccs_mv,
    mv_to_modularity_dt,
    "both"
) %>%
    purrr::imap(~ .x[, w_size := w_size_vec[.y]]) %>%
    rbindlist     

# combine dfs
comb_df = rbind(
    nonec_df[, d_spec := "Not-elsewhere-classified Occupations Excluded"],
    fulloccs_df[, d_spec := "All Occupations"]
)


# compare NoNEC with FullOccs (Figure E6)
pdf(here("output", "figures", "Figure_E6.pdf"), 
    width = 11.5, height = 5)
print(
    ggplot(
        comb_df, 
        aes(x = mid_year, 
            y = lr_modularity, 
            linetype = factor(w_size)
        )
    ) + 
    geom_line() +
    scale_linetype_manual(
        "Window Size", 
        values = seq_along(w_size_vec)) +
    facet_wrap(~ d_spec, ncol = 2)+
    labs(
        y = "LinkRank Modularity\n",
        x = "\nYear (Mid-Year of Moving Window)"
    )+
    scale_x_continuous(
        breaks = seq(1989, 2015, 2)
    ) +
    fnb_mod_y_axis + 
    fnb_theme +
    theme(
        legend.position=c(.995,.025)
    )
)
dev.off()

message(paste0("File Path : ", here("output", "figures", "Figure_E5.pdf")))

# save data
fwrite(comb_df, here("output", "data", "Figure_E6.csv"))
message(paste0("Data Path : ", here("output", "data", "Figure_E6.csv")))
        

        
# Analyzing Gender-specific trends in modularity ------------------------------


message("\nPlotting gender-specific trends in modularity (Figure C1) ...")

# make data.table out of results
g_df = rbind(
    mv_to_modularity_dt(
        female_mv[[1]], "lr"
    )[
        , d_spec := "Female"
    ],
    mv_to_modularity_dt(
        male_mv[[1]], "lr"
    )[
        , d_spec := "Male"
    ]
)

# plot results (Figure C1)
pdf(here("output", "figures", "Figure_C1.pdf"),
    width = 7.5, height = 5)
print(
    ggplot(
        g_df,
        aes(
            x = mid_year, 
            y = lr_modularity,
            linetype = factor(d_spec)
        )
    ) + 
    geom_line() + 
    scale_x_continuous(
        breaks = seq(1989, 2015, 2)
    ) +
    fnb_mod_y_axis + 
    scale_linetype_manual(
        name = "Gender",
        values = 1:2
    ) + 
    labs(
        y = "LinkRank Modularity\n",
        x = "\nYear (Mid-Year of Moving Window)"
    ) +
    fnb_theme 
)
dev.off()

message(paste0("File Path : ", here("output", "figures", "Figure_C1.pdf")))

# save data
fwrite(g_df, here("output", "data", "Figure_C1.csv"))
message(paste0("Data Path : ", here("output", "data", "Figure_C1.csv")))



# Analyzing modularity trends for OCC2000 occupational codes ------------------

message("\nPlotting modularity trends using OCC2000 codes (Figure E7) ...")

# create data.table out of modularity measures
occ2000_df = purrr::map(
    occ2000_mv,
    mv_to_modularity_dt,
    "both"
) %>%
    purrr::imap(~ .x[, w_size := w_size_vec[.y]]) %>%
    rbindlist

# plot Figure E7
pdf(here("output", "figures", "Figure_E7.pdf"),
    width = 7.5, height = 5)
print(
    ggplot(
        occ2000_df,
        aes(
            x = mid_year, 
            y = lr_modularity,
            linetype = factor(w_size)
        )
    ) + 
    geom_line() + 
    scale_x_continuous(
        breaks = seq(1989, 2015, 2)
    ) +
    fnb_mod_y_axis + 
    scale_linetype_manual(
        name = "Window Size",
        values = seq_along(w_size_vec)
    ) + 
    labs(
        y = "LinkRank Modularity\n",
        x = "\nYear (Mid-Year of Moving Window)"
    ) +
    fnb_theme 
)
dev.off()

message(paste0("File Path : ", here("output", "figures", "Figure_E7.pdf")))

# save data
fwrite(occ2000_df, here("output", "data", "Figure_E7.csv"))
message(paste0("Data Path : ", here("output", "data", "Figure_E7.csv")))



# Analyzing modularity trends for reweighted ties -----------------------------


message("\nPlotting modularity trends with reweighted ties (Figure E8) ...")

# create data.table out of modularity measures
rew_df = purrr::map(
    rew_mv,
    mv_to_modularity_dt,
    "both"
) %>%
    purrr::imap(~ .x[, w_size := w_size_vec[.y]]) %>%
    rbindlist

# plot Figure E8
pdf(here("output", "figures", "Figure_E8.pdf"),
    width = 7.5, height = 5)
print(
    ggplot(
        nonec_df,
        aes(
            x = mid_year, 
            y = lr_modularity,
            linetype = factor(w_size)
        )
    ) + 
    geom_line() + 
    scale_x_continuous(
        breaks = seq(1989, 2015, 2)
    ) +
    fnb_mod_y_axis + 
    scale_linetype_manual(
        name = "Window Size",
        values = seq_along(w_size_vec)
    ) + 
    labs(
        y = "LinkRank Modularity\n",
        x = "\nYear (Mid-Year of Moving Window)"
    ) +
    fnb_theme 
)
dev.off()

message(paste0("File Path : ", here("output", "figures", "Figure_E8.pdf")))

# save data
fwrite(rew_df, here("output", "data", "Figure_E8.csv"))
message(paste0("Data Path : ", here("output", "data", "Figure_E8.csv")))

message("\nDone!")

### END OF CODE ###