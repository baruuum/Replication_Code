###############################################################################
##
## Summarizing FullSamps Data for AllOccs and NoNEC
##
## Creator     : Barum Park
## Last Update : 02/21/2020
##
###############################################################################

message("\n\n\n***** Summerizing FullSamp Data *****")

## Yearly data ----------------------------------------------------------------

message("\nLoading yearly data and weights ...")

# data
dat_full = fread(here("data", "dat_FullSamp_agg_FullOccs.csv"))
dat_nonec = fread(here("data", "dat_FullSamp_agg_NoNEC.csv"))

# weights for NoNEC
w_nonec = fread(here("data", "obs_weights_FullSamp_NoNEC.csv"))

# generating summarizing data
sum_dat = rbind(
    tie_summary(dat_full, w_nonec, verbose = FALSE),
    tie_summary(
        dat_nonec, 
        w_nonec, 
        nonec = T, 
        nec_regex = nec_regex,
        verbose = FALSE
    )
)

# transform nec to factor
sum_dat[, nec := factor(nec, levels = rev(unique(sum_dat$nec)))]

message("\nPlotting summary across all years ...")

f_path = here("output", "figures", "Figure_E1.pdf")
d_path = here("output", "data", "Figure_E1.rds")

message(paste0("File path: ", f_path))
message(paste0("Data path: ", d_path))

pdf(f_path, width = 10, height = 7)
print(
    ggplot(sum_dat, aes(x = year, y = value, col = nec, linetype = nec)) + 
        geom_line(size = 0.6) + 
        scale_x_continuous(breaks = 1989:2015)+
        scale_color_manual("", values = c("black", "grey50")) +
        scale_linetype_manual("", values = 1:2)+
        labs(x = "Year", y = NULL) +
        facet_wrap(~ variable, scales = "free_y") +
        fnb_theme + 
        theme(
            axis.text.x = element_text(angle = 45, vjust = .5),
            legend.direction = "vertical",
            legend.title = element_blank(),
            legend.background = element_rect(fill = "white", 
                                             colour = "black",
                                             size = .1),
            legend.position=c(.755,.01),
            legend.key.size = unit(2, "line"),
            legend.key.height = unit(1,"line")
        )
)
dev.off()

# save data
saveRDS(sum_dat, d_path)

message("\nDone!")

### END OF CODE ###