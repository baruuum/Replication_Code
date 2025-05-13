#!/env/bin/Rscript

## Short Comparison Between R's glm Function and the loglin_block Function
## Author: Barum Park

DEBUG = FALSE

if (!DEBUG) {

    source(".Rprofile")

    log_file = file(snakemake@log[[1]], open = "wt")
    sink(log_file, type = "output")
    sink(log_file, type = "message")

}

library("sbmob")
import::from("src/R/utils.R", print_session)

logger::log_info("Start comparison between sbmob and glm ...")

set.seed(42)

n = 300
x = matrix(rpois(n^2, 3), n)
z = rep(1, n)
dat = mobmat_to_dat(x)

start_dense = Sys.time()
res_dense = loglin_blocks(dat, z, sparse = FALSE)
end_dense = Sys.time()

start_sparse = Sys.time()
res_sparse = loglin_blocks(dat, z, sparse = TRUE)
end_sparse = Sys.time()

time_dense = difftime(end_dense, start_dense, unit = "sec")
time_sparse = difftime(end_sparse, start_sparse, unit = "sec")

message("glm runtime: ", round(time_dense, 3), " seconds")
message("loglin_block runtime: ", round(time_sparse, 3), " seconds")

logger::log_info("Done!")

if (!DEBUG) {

    # add flag
    invisible(file.create(snakemake@output[["flag"]]))

    print_session()
    sink()

}

### EOF ###