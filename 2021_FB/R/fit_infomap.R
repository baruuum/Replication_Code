#' Fit the map equation
#' 
#' @param g an \code{igraph} object containing the mobility network
#' @param dat a \code{data.table} object containing three columns: the sender occupation, the receiver occupation, and the edge-weight
#' @param opts a character string containing additional options that should be feeded to the infomap algorithm
#' @param verbose integer; larger numbers produce more output
#' @param parallel boolean; if TRUE, indicates (not runs!) that the function is implemented in a parallel loop. This is important when generating output that is read into the R environment to summarize the results.
#' @param outer_loop_limit integer; number of times outer-most loop should be run to find optimal solution
#' @param core_loop_limit integer; number of times algorithm tries to allocate nodes into best possible module
#' @return returns a \code{igraph::communities} object
fit_infomap = function(
    g = NULL, 
    dat = NULL, 
    opts = NULL, 
    verbose = 0L, 
    parallel = FALSE,
    iterator = NULL,
    outer_loop_limit = 100,
    inner_loop_limit = 50,
    save_labels = FALSE,
    output_file = NULL) 
{
    
    # check that here package is installed
    suppressPackageStartupMessages(library(here))
    
    # check infomap folder exists
    if (!dir.exists(here("infomap")))
        stop(paste0("cannot find directory ", here("infomap")))
    
    # check that not both g and el are supplied
    if (!is.null(g) && !is.null(dat))
        stop("Specify either g or dat, but not both")
    
    if (!is.null(g) && !is.igraph(g))
        stop("g has to be an igraph object")
    
    if (!is.null(dat) && !is.data.table(dat))
        stop("dat has to be a data.table object")
    
    # check whether the "iterator" for parallel processing is present
    # note: this is needed not to overwrite the same file in a parallel loop
    if (parallel && is.null(iterator)) 
        stop("Parallel == TRUE but iterator == NULL")

    # check whether tree and clu are requested
    opts = paste0(
        opts, 
        "-w --tree --clu -2",
        " -N ",
        outer_loop_limit,
        " -M ", 
        inner_loop_limit)
    
    # check consistency between save_labels and output_file
    if (save_labels && is.null(output_file))
        stop("save_labels == TRUE but output_file == NULL")

    if (verbose < 2) 
        opts = paste0(opts, " --silent")

    # transform edgelist to igraph object
    if (!is.null(dat)) 
        g = igraph_from_df(dat, save_labels = save_labels, file = output_file)

    # check vertex sequence
    if (min(V(g)) != 1L)
        stop("vertex numbering does not start from 1")
        
    if (grepl("-z", opts))
        stop("vertex sequence starts from 1, don't specify -z option")

    if (vcount(g) != max(V(g)))
        stop("Vertex numbering is not sequential")
    
    # transform g to infomap format
    el = as_edgelist(g, names = FALSE) %>% 
            as.data.table %>%
        setnames(c("sender", "receiver"))
    
    el[, weight := edge_attr(g, "weight")]
    
    if (nrow(el[sender == receiver]) != 0L)
        stop("graph contains self-loops")
    
    if (sum(duplicated(el[, 1:2])) != 0)
        stop("edge-list contains duplicated rows")
    
    # summarize
    if (verbose > 0) {
        message("\n--- Model summary ---\n")
        message(paste0("Number of nodes : ", length(V(g))))
        message(paste0("Number of edges : ", NROW(el)))
        message(paste0("Total edge-weight : ", el[, round(sum(weight), 3)]))
        message(paste0("Infomap options : ", opts))
        message("\n---------------------\n")
        
    }

    # run infomap-stand alone (this is necessary since the infomap run from R
    # will return only the highest level of modules

    # change working directory to infomap folder
    # setwd(paste0(wd.base,"/infomap"))
    
    # create an "output" folder within the /infomap folder if nonexistent
    if (!dir.exists(here("infomap", "output"))) {
        
        message("creating infomap/output/ directory ...")
        dir.create(here("infomap", "output"))
        
    }

    if (!dir.exists(here("infomap", "tmp_data"))) {
        
        message("Creating infomap/tmp_data/ directory ...")
        dir.create(here("infomap", "tmp_data"))
        
    }
    
    # generate name of output files
    tmp_fn = if (parallel) paste0("el_temp_", iterator) else "el_temp"

    # write edge-list to "simple list" format
    fwrite(el, 
           here("infomap", "tmp_data", paste0(tmp_fn, ".txt")),
           sep = "\t", 
           row.names = FALSE, 
           col.names = FALSE)
    
    ## run infomap stand-alone
    if (verbose > 0)
        message("\nRunning Infomap ...")
    
    system(
        paste0(
            here("infomap"), 
            "/Infomap ", 
            here("infomap", "tmp_data"), 
            "/", 
            tmp_fn, 
            ".txt ",
            here("infomap", "output"), 
            "/ ",
            opts
        )
    )
    
    # create module list
    if (verbose > 0)
        message("Creating Module list...")

    # load results
    res = fread(
        paste0(here("infomap", "output"), "/", tmp_fn, ".tree"), 
        sep=" ", 
        skip = 2, 
        header = FALSE
    )[
        , .(cluster = V1, occ_id = as.integer(V4))
    ] %>%
        setorder(occ_id)
        
    # check that all the occupations are returned
    if (sum(diff(res$occ_id) != 1) != 0)
        stop("not all occupations are returned from infomap algorithm")
    

    # generate array of modules
    # output file is structured as
    # rows: node id
    # columns: 1st level, 2nd level, ...
    res[
        , memb := strsplit(res[[1]], ":") %>% 
                      purrr::map_chr(`[`, 1L) %>%
                      as.integer
    ][
        , cluster := NULL
    ]
        
    
    # get code length
    res_2 <- readLines(paste0(here("infomap", "output"), "/", tmp_fn,".tree"), n = 1L)
    code_len = gsub(".*to codelength\\s*|\\s*in.*","", res_2) %>%
        as.numeric

    # check levels
    if (as.numeric(gsub(".*to codelength.*in\\s*|\\s*levels.*","", res_2)) != 2)
        stop("infomap partition-level is not equal to two")

    # reorder modules in decreasing order based on number of nodes
    re_mod = reorder_labels(res$memb)
    
    # calculate weighted modularity
    # note: 1) this measure is known to be problematic for directed networks!
    suppress_all(w_modularity <- igraph::modularity(g, re_mod, weights = E(g)$weight))
    
    # create communities object
    comm = list(
        graph           = g, 
        membership      = re_mod, 
        algorithm       = "Infomap",
        codelength      = code_len,
        w_modularity    = w_modularity,
        lr_modularity   = lr_modularity(g, re_mod),
        names           = V(g)$name,
        unordered_membs = res$memb
    )
    class(comm) = "communities"
    
    if (verbose > 0) {
        
        message("\n--- Module summary ---\n")
        message(paste0("\nCode Length     : ", round(code_len(comm), 3)))
        message(paste0("No. of Clusters : ", length(comm)))
        message(paste0("Modularity      : ", round(comm$w_modularity, 3)))
        message(paste0("LR.Modularity   : ", round(comm$lr_modularity, 3)))
        message("\n----------------------\n")
        
    }

    return(comm)

}