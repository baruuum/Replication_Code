## ------------------------------------------------------------------
## Utility functions for plotting
## ------------------------------------------------------------------

# ggplot theme
ggtheme = ggplot2::theme_bw() +
    ggplot2::theme(
        panel.grid          = ggplot2::element_blank(),
        title               = ggplot2::element_text(size = 14),
        axis.title          = ggplot2::element_text(size = 12),
        legend.title        = ggplot2::element_text(size = 11),
        legend.text         = ggplot2::element_text(size = 10),
        legend.key.size     = ggplot2::unit(2, "line"),
        legend.key.height   = ggplot2::unit(1, "line"),
        strip.text          = ggplot2::element_text(size = 12,
                                                    hjust = 0),
        strip.background    = ggplot2::element_rect(fill = "white",
                                                    color = "white")

    )



# function to draw blocks in plotting
gen_block_lines = function(class_map, rev_y = FALSE, draw_all = FALSE) {

    stopifnot(
        "occ_no" %in% names(class_map),
        "class" %in% names(class_map)
    )

    if (
        !isTRUE(
            all.equal(
                class_map,
                class_map[order(class, occ_no)],
                check.attributes = FALSE
            )
        )
    )
        stop("class_map has to be orderd by class and, within class, by occ_no")

    # make hard copy
    x = copy(class_map)

    # get new occ numbers
    x = x[, new_occ := seq_len(.N)]

    # get min and max occ no for each class
    pars = x[, list(min = min(new_occ), max = max(new_occ)), by = class]

    if (draw_all) {

        n_occs = nrow(x)

        segs = sort(unique(c(pars$min - .5,  pars$max + .5)))
        min_s = min(segs)
        max_s = max(segs)

        # generate block partition
        blocks = lapply(
            segs,
            function(w) {

                data.table(
                    x = c(min_s, w),
                    xend = c(max_s, w),
                    y = c(w, min_s),
                    yend = c(w, max_s)
                )

            }
        ) |> rbindlist()

        if (rev_y)
            blocks[, `:=`(y = max(y) - y + .5, yend = max(yend) - yend  + .5)]

        return(blocks)

    }

    # block partitions
    gen_blocks = function(a, b) {
        a = a - .5; b = b + .5
        rbind(
            c(a, a, a, b),
            c(a, b, a, a),
            c(a, b, b, b),
            c(b, b, a, b)
        )
    }

    blocks = do.call(
        `rbind`,
        lapply(
            1:nrow(pars),
            \(w) gen_blocks(pars[w]$min, pars[w]$max) # nolint
        )
    ) |> as.data.table()
    setnames(blocks, c("x", "xend", "y", "yend"))

    if (rev_y)
        blocks[, `:=`(y = max(y) - y + .5, yend = max(yend) - yend  + .5)]

    return(blocks)

}

# grab legend from ggplot
# https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
grab_legend = function(p){

    pdf(NULL) # to avoid generating Rplots.pdf

    pt = ggplot_gtable(ggplot_build(p))
    leg = which(sapply(pt$grobs, function(x) x$name) == "guide-box")
    legend = pt$grobs[[leg]]

    return(legend)

}

### EOF ###