###############################################################################
##
## Summarizing Three-period Data 
##
## Creator     : Barum Park
## Last Update : 02/21/2020
##
## Notes:
## 1) Data are weighted by normalizing weights
## 2) NoNEC occupations are not included
## 3) Data generated for FullSamp specification
##
###############################################################################


message("\n\n\n***** Summarizing three-period data *****\n")

# Basic setup -----------------------------------------------------------------

if (!exists("p_vec"))
    stop("cannot find p_vec")

# labels of the periods (for ordering periods)
p_labs = gen_p_labs(p_vec)

message("Loading data ...")

p_dat = purrr::map(
    p_vec, 
    function(w) {
        x = fread(
            here(
                "data", 
                paste0("dat_", w, "_NoNEC_weighted_complete5.csv")
            )
        )
        x[, period := gen_p_labs(w)]
        return(x)
    }
) %>%
    do.call(rbind, .)



# Degree distributions --------------------------------------------------------

message("\nSummarizing in- and out-degrees ...\n")

# out-degrees        
out_df = p_dat[
    occ1 != occ2, .(degree = sum(N)), by = list(occ1, period)
][
    , type := "Out-degree"
] %>%
    setnames("occ1", "occ_name")

# in-degrees
in_df = p_dat[
    occ1 != occ2, .(degree = sum(N)), by = list(occ2, period)
][
    , type := "In-Degree"
] %>%
    setnames("occ2", "occ_name")

# combine
degree_df = rbind(out_df, in_df)[
    # transform period to factor
    , period := factor(period, p_labs)
]%>%
    # order by period and degree
    setorder(type, period, -degree)

# generate dataset of top 5 occupations
top_5_degree = degree_df[
    , head(.SD, 5L), by = list(type, period)
][
    # add labels
    , lab := paste0(occ_name, " (", round_to_char(degree, 1), ")")
][
    # trim labels that are too long
    , lab := gsub(
           "Customer service reps\\, investigators and adjusters\\, except insurance",
           "Customer service reps, except insurance",
           lab
    )
][
    # collapse labels into single character string
    , .(lab = paste0(lab, collapse = " \n")), by = list(type, period)
][
    # add whitespace to the whole string to keep text aligned
    , lab := paste0(lab, " ")
]

# file paths
f_path = here("output", "figures", "Figure_2.pdf")
d_path = here("output", "data", "Figure_2.csv")

message("Plotting histogram of in- and out-degrees ...")
message(paste0("File path : ", f_path))
message(paste0("Data path : ", d_path))

pdf(f_path, width = 10, height = 7)
print(
    ggplot(degree_df, aes(x = degree)) + 
        geom_histogram(col = "black", 
                      fill = "white",
                      bins = 75, 
                      alpha = 1) +
        facet_grid(period ~ type, scale='free_x') +
        labs(x='Weighted Degree', y='Frequency') +
        geom_hline(color = "grey",yintercept = 0) +
        geom_text(data=top_5_degree,
                  aes(x = Inf, y = Inf, label = lab),
                  hjust = 1,
                  vjust = 1.05, 
                  size = 3.5) +
        fnb_theme
)
dev.off()



# Distribution of transitions -------------------------------------------------

message("\nSummarizing transitions ...\n")

# generate data to plot
trans_df = p_dat[
    occ1 != occ2
][
    , period := factor(period, levels = p_labs)
]%>%
    setorder(period, -N)

# generate dataset of top 5 occupations
top_5_trans = trans_df[
    , head(.SD, 5L), by = period
][
    # add labels
    , lab := paste0(
        occ1,
        sprintf(" \u2192 "),
        occ2,
        " (", round_to_char(N, 1), ")")
][
    # collapse labels into single character string
    , .(lab = paste0(lab, collapse = " \n")), by = period
][
    # add whitespace to the whole string to keep text aligned
    , lab := paste0(lab, " ")
]


# generate plot

message("Plotting histogram of transition weights ...")

f_path = here("output", "figures", "Figure_E2.pdf")
d_path = here("output", "data", "Figure_E2.csv")

message(paste0("File Path : ", f_path))
message(paste0("Data Path : ", d_path))

# Note: The following warning messages are suppressed
#
#   1: Transformation introduced infinite values in continuous y-axis 
#   2: Removed X rows containing missing values (geom_bar)
#
# which result from the log-transformation of the y-axis. No data is lost.

pdf(f_path, width = 8, height = 7)
suppressWarnings(
    print(
        ggplot(trans_df, aes(x = N)) + 
            geom_hline(col = "grey50", yintercept = 1, alpha = .5) + 
            geom_histogram(col = "black", 
                            fill = "white",
                            bins = 50, 
                            alpha = 1) +
            theme_bw() +
            scale_y_continuous(
                trans = "log10",
                breaks = scales::trans_breaks('log10', function(x) 10^x),
                labels = scales::comma
            ) +
            facet_wrap(~ period, ncol = 1) +
            geom_text(data = top_5_trans,
                    aes(x = Inf, y = Inf, label = lab),
                    hjust = 1,
                    vjust = 1.05,
                    size = 3.5
            ) +
            labs(
                x = "Number of Transitions (Weighted)", 
                y = "Frequency (log scale)"
            ) +
            fnb_theme
    )
)
dev.off()

# save data
fwrite(trans_df, d_path)

message("\nDone!")
