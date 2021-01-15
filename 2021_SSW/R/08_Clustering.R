################################################################################
##
## Summary statistics to compare clustering by field
##
################################################################################

# Set up  ----------------------------------------------------------------------

# libraries
library("here")
library("data.table")
library("igraph")

# current time
message(
    paste0("\nStart running 08_Clustering.R on ", Sys.time())
)

# source functions
Rcpp::sourceCpp(here("src", "dist_by_partition.cpp"))

# current objects
obj_list = ls()


# Read Data --------------------------------------------------------------------

# largest component in both years
g19 = readRDS(here("data", "onemode19.rds"))
g20 = readRDS(here("data", "onemode20.rds"))

# data on students fields
major19 = rio::import(
    here("raw_data", "fall2019", "f2019_uni_field6_studentnbr.xls"),
    setclass = "data.table"
) %>% 
    setnames(c("id", "field"))
# renamd id to match graph
major19 = major19[
    , id := paste0("_", id)
][
    # reorder data to match graph
    match(id, vertex_attr(g19, "name"))
]


# data on students fields
major20 = rio::import(
    here("raw_data", "fall2020", "f2020_f2f_ith_field6_studentnbr.xls"),
    setclass = "data.table"
) %>% 
    setnames(c("id", "field"))
major20 = major20[
    , id := paste0("_", id)
][
    match(id, vertex_attr(g20, "name"))
]

# load distance matrices
dist19 = readRDS(here("data", "distances2019.rds"))
dist20 = readRDS(here("data", "distances2020.rds"))

#check
stopifnot(
    all(vertex_attr(g19, "name") == major19$id),
    all(vertex_attr(g20, "name") == major20$id),
    all(rownames(dist19) == major19$id),
    all(rownames(dist20) == major20$id),
    all(colnames(dist19) == major19$id),
    all(colnames(dist20) == major20$id)
)


# Calculate summary statistics -------------------------------------------------

p19 = major19$field
p20 = major20$field

modularity_vec = c(modularity(g19, p19), modularity(g20, p20))
names(modularity_vec) = c("Fall 2019", "Fall 2020")
assortativity_vec = c(assortativity_nominal(g19, p19), assortativity_nominal(g20, p20))
names(assortativity_vec) = c("Fall 2019", "Fall 2020")

med_dist_19 = med_dist_by_partition(dist19, p19)
med_dist_20 = med_dist_by_partition(dist20, p20)
med_dist_diff = med_dist_20 - med_dist_19
mean_dist_19 = mean_dist_by_partition(dist19, p19)
mean_dist_20 = mean_dist_by_partition(dist20, p20)
mean_dist_diff = mean_dist_20 - mean_dist_19

clust_res = list(
    modularity     = modularity_vec,
    assortativity  = assortativity_vec,
    med_dist_19    = med_dist_19,
    med_dist_20    = med_dist_20,
    med_dist_diff  = med_dist_diff,
    mean_dist_19   = mean_dist_19,
    mean_dist_20   = mean_dist_20,
    mean_dist_diff = mean_dist_diff
)

saveRDS(clust_res, here("data", "clust_res.rds"))



# Cleaning up ------------------------------------------------------------------

# clear objects (although package will remain attached)
rm(list = ls()[!(ls() %in% obj_list)])

message(
    paste0("Done running script on ", Sys.time(), "\n")
)


### END OF CODE ###