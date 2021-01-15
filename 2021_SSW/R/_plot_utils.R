################################################################################
##
## Helper functions for plotting (default igraph plotting is too slow!)
##
################################################################################

#' Extract attributes from graph and combine with layout
#' 
#' @param g an \code{igraph} object
#' @param v_attr the vertex attributes to extract
#' @param e_attr the edge attributes to extract
#' @param what what to return
#' @return returns at most two \code{data.tables} in a list: one table has attributes of the nodes and the other attributes of edges
igraph_attr_extr = function(
    g, 
    v_attr = NULL, 
    e_attr = NULL, 
    what = c("both", "vertex", "edge")
    ) {
    
    library(igraph)
    library(data.table)
    
    # what to extract
    what = match.arg(what, several.ok = FALSE)
    
    # arg checks    
    stopifnot(
        class(g) == "igraph",
        !is.directed(g)
    )
    
    if (!is.null(v_attr))
        stopifnot(
            is.character(v_attr),
            all(v_attr %in% names(vertex_attr(g)))
        )
    
    if (!is.null(e_attr))
        stopifnot(
            is.character(e_attr),
            all(e_attr %in% names(edge_attr(g)))
        )
    
    # vertex attribiue
    v_attr = if ("name" %in% v_attr) {
        v_attr 
        } else {
            
            vertex_attr(g, "name") = seq_len(vcount(g))
            
            c("name", v_attr)
        }
    
    v = setDT(vertex_attr(g)[v_attr])
    
    e_attr_mat = if (!is.null(e_attr)) {
        do.call("cbind", edge_attr(g)[e_attr])
    } else NULL
    
    e = as.data.table(
        cbind(
            as_edgelist(g, names = TRUE), 
            e_attr_mat
        )
    ) %>%
        setnames(c("name1", "name2", e_attr))
    
    if (what == "both") {
        
        return(list(v_dat = v, e_dat = e))
        
    } else if (what == "vertex") {
        
        return(v)
    
    } else {
        
        return(e)
        
    }
    
}


#' Prepare extracted attributes from \code{igraph} object for plotting
#' 
#' @param ve_list a \code{list} of two elements: a \code{data.table} of vertex attributes and a \code{data.table} of an edge-list attributes
#' @param layout a two-dimensional \code{array} of node positions
#' @param v_color vertex colors
#' @param e_color edge colors
#' @param v_pch vertex shapes
#' @param v_cex vertex sizes
#' @param vf_cex_add how much to add (in percentages) to the vertex sizes when plotting frames
#' @param vf_color color of frames
#' @param vf_lwd line width of frames
#' @param e_lwd line width of edges
igraph_plot_prep = function(
    ve_list, 
    layout,
    v_frame = "white",
    v_fill = "blue",
    v_lwd  = .1, 
    v_pch = 21,
    v_cex = .6, 
    e_color = "grey",
    e_lwd = .3
){
    
    if (!("matrix" %in% class(layout)))
        stop("layout has to be a two-dimensional matrix")
    
    if (!all(c("v_dat", "e_dat") %in% names(ve_list)))
        stop("names of ve_list have to be v_dat and e_dat")
    
    if (!("name" %in% names(ve_list$v_dat)))
        stop("ve_list$v_dat must have a column named name")
    
    if (!("type" %in% names(ve_list$v_dat)))
        stop("ve_list$v_dat must have a column named type")
    
    if (!is.logical(ve_list$v_dat$type))
        stop("ve_list$v_dat$type must be logical")
    
    
    if (!all(c("name1", "name2") %in% names(ve_list$e_dat)))
        stop("ve_list$e_dat must have a columns name1 and name2")
    
    
    if (!all(c("v_dat", "e_dat") %in% names(ve_list)))
        stop("names of ve_list have to be v_dat and e_dat")
    
    if (NROW(ve_list$v_dat) != NROW(layout))
        stop("dimension mismatch between ve_list[[[1]]] and layout")
    
    
    v = data.table(name = ve_list$v_dat$name, type = ve_list$v_dat$type)
    e = data.table(name1 = ve_list$e_dat$name1, name2 = ve_list$e_dat$name2)

    # add layout for verticees
    v[, `:=`(dim1 = layout[, 1L], dim2 = layout[, 2L])]
    
    # vertex attributes    
    v[
        , `:=`(
            v_pch     = v_pch,
            v_frame   = v_frame,
            v_fill    = v_fill,
            v_lwd     = v_lwd,
            v_cex     = v_cex
        )
    ]
    
    # edge attributes
    e[
        , `:=`(
            e_color = e_color,
            e_lwd   = e_lwd
        )
    ]
    
    # layout for edges
    e = merge(
        e, 
        v[, .(name, dim1, dim2)],
        by.x = "name1",
        by.y = "name",
        all.x = T
    ) %>%
        setnames(
            c("dim1", "dim2"), c("dim1_1", "dim2_1")
        ) %>%
        merge(
            v[, .(name, dim1, dim2)],
            by.x = "name2",
            by.y = "name",
            all.x = T
        ) %>%
        setnames(
            c("dim1", "dim2"), c("dim1_2", "dim2_2")
        ) %>%
        setcolorder(c("name1", "name2"))

    return(list(v_dat = v, e_dat = e))
    
}


#' Function to plot network
#' 
#' @param ve_list list of data.table with elements \code{v_dat} and \code{e_dat}, where the former contains plotting/layout information of the vertices and the latter plotting/layout information of the edges
#' @param bg_col background color
#' @param plot_first what to plot first: courses or students
#' @param only_first whether only courses or students should be plotted
#' @param v_frame overwrite vertex frame color
#' @param v_lwd overwrite vertex frame thickness
#' @param v_fill overwrite vertex fill color
#' @param v_cex overwrite vertex size
#' @param v_pch overwrite vertex pch
#' @param e_color overwrite edge color
#' @param e_lwd overwrite edge linewidth
#' @param width width of the pdf file (in inches)
#' @param height height of the pdf file (in inches)
#' @param outfile name of the output file
plot_network = function(
    ve_list,
    bg_col     = "white",
    plot_first = c("none", "courses", "students"),
    only_first = FALSE,
    v_frame    = NULL,
    v_lwd      = NULL,
    v_fill     = NULL,
    v_cex      = NULL,
    v_pch      = NULL,
    e_color    = NULL,
    e_lwd      = NULL,
    width      = 7,
    height     = 7,
    outfile
) {
    
    v_dat = ve_list$v_dat
    e_dat = ve_list$e_dat
    
    plot_first = match.arg(plot_first, several.ok = FALSE)
    
    if (!is.null(v_frame))
        v_dat$v_frame = v_frame
    
    if (!is.null(v_fill))
        v_dat$v_fill = v_fill
    
    if (!is.null(v_lwd))
        v_dat$v_lwd = v_lwd
    
    if (!is.null(v_pch))
        v_dat$v_pch = v_pch
    
    if (!is.null(v_cex))
        v_cat$v_cex = v_cex
    
    if (!is.null(e_lwd))
        e_dat$e_lwd = e_lwd
    
    if (!is.null(e_color))
        e_dat$e_color = e_color
    
    pdf(outfile, width = width, height = height)
    par(oma = rep(0, 4), mar = c(0, 0, 0, 0))
    plot(
        v_dat$dim1, 
        v_dat$dim2, 
        type = "n",
        xlab = NA,
        ylab = NA,
        axes = F)
    rect(
        par("usr")[1],
        par("usr")[3],
        par("usr")[2],
        par("usr")[4],
        col = bg_col,
        border = NA
    )
    segments(
        x0 = e_dat$dim1_1, 
        y0 = e_dat$dim2_1,
        x = e_dat$dim1_2, 
        y = e_dat$dim2_2,
        col = e_dat$e_color,
        lwd = e_dat$e_lwd
    )
    
    
    if (plot_first %in% c("courses", "students")) {
        
        p_first = if (plot_first == "courses") FALSE else TRUE
        p_second = if (plot_first == "courses") TRUE else FALSE
        
        tmp_dat = v_dat[type == p_first]
        
        points(
            tmp_dat$dim1, 
            tmp_dat$dim2, 
            col = tmp_dat$v_frame,
            bg  = tmp_dat$v_fill,
            lwd = tmp_dat$v_lwd,
            cex = tmp_dat$v_cex, 
            pch = tmp_dat$v_pch
        )
        
        if (!only_first) {
            
            tmp_dat = v_dat[type == p_second]
            
            points(
                tmp_dat$dim1, 
                tmp_dat$dim2, 
                col = tmp_dat$v_frame,
                bg  = tmp_dat$v_fill,
                lwd = tmp_dat$v_lwd,
                cex = tmp_dat$v_cex, 
                pch = tmp_dat$v_pch
            )
            
        }
        
    } else {
        
        points(
            v_dat$dim1, 
            v_dat$dim2, 
            col = v_dat$v_frame,
            bg  = v_dat$v_fill,
            lwd = v_dat$v_lwd,
            cex = v_dat$v_cex, 
            pch = v_dat$v_pch
        )
        
    }
    
    dev.off()
    
}


#' Extract subset from plot-ready list
#' 
#' @param ve_list list of data.tables to subset (generated by igraph_plot_prep)
#' @param sub_var variables to subset on
#' @param sub_val value to subset on
#' @param keep_edges what edges to keep
subset_plot_df = function(
    ve_list, 
    sub_var, 
    sub_val, 
    keep_edges = c("all", "connected", "none")
) {
    
    keep = match.arg(keep_edges, several.ok = FALSE)
    
    stopifnot(
        length(sub_var) == 1L,
        length(ve_list) == 2L,
        is.character(sub_var),
        all(c("v_dat", "e_dat") %in% names(ve_list)),
        "name" %in% names(ve_list$v_dat),
        all(c("name1", "name2") %in% names(ve_list$e_dat))
    )
    
    # update v_dat
    ve_list$v_dat = ve_list$v_dat[get(sub_var) %in% sub_val]
    
    # update e_dat
    if (keep == "none") {
        
        ve_list$e_dat = ve_list$e_dat[0]
        
    } else if (keep == "all") {
        
        # get names
        v_names = ve_list$v_dat$name
        el = ve_list$e_dat[
            (name1 %in% v_names) | (name2 %in% v_names)
        ]
        
        ve_list$e_dat = el

    } else {
        
        # get names
        v_names = ve_list$v_dat$name
        el = ve_list$e_dat[
            (name1 %in% v_names) | (name2 %in% v_names)
        ]
        
        c_names = data.table(
            # all not-selected vertices
            en = c(
                el[!(name1 %in% v_names), name1],
                el[!(name2 %in% v_names), name2]
            )
        )[
            # count number
            , N := .N, by = "en"
        ][
            # keep only nodes that are connected to at least two of selected ones
            N > 1L
        ]
        
        # select only edges that connect nodes among selected nodes
        new_v_names = c(v_names, c_names$en)
        ve_list$e_dat = el[
            (name1 %in% new_v_names) & (name2 %in% new_v_names)
        ]

    }

    return(ve_list)
    
}
