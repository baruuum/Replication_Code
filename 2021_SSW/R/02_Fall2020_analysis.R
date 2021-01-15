################################################################################
##
## Updating of Table2 in Weeden & Cornwell (2020) Soc.Sci
##
################################################################################

# Set up & read data -----------------------------------------------------------

# libraries
library("here")
library("igraph")

# current time
message(
    paste0("\nStart running 02_Fall20_analysis.R on ", Sys.time())
)

# current objects
obj_list = ls()

# create directory if non-existent
if (!dir.exists(here("data"))) {
    
    message("Creating directory to save results ...")
    dir.create(here("data"), recursive = T)
    
}

# read data from pajek into igraph
g = read_graph(
    here("raw_data", "fall2020", "p2_f2f_ith.net"), 
    format = "pajek"
)

# check edge-structure (is bipartite?)
bipart = bipartite_mapping(g)
stopifnot(bipart$res)

# Analyze as bipartite graph ---------------------------------------------------

# map graph into two types
V(g)$type = bipart$type

# get student nodes
students = V(g)[bipart$type]
courses  = V(g)[!bipart$type]

# get components
comps = components(g)

# largest component
comp1 = comps$membership[comps$membership == which.max(comps$csize)]

# get bicomponents & save results 
bicomps = biconnected_components(g)
saveRDS(bicomps, here("data", "bicomp2020.rds"))
# bicomps = readRDS(here("data", "bicomp2020.rds"))

# get bicomp. sizes
bicomp_sizes = sapply(bicomps$components, length)

# largest bicomponent
bicomp1 = names(bicomps$components[[which.max(bicomp_sizes)]])

# betweenness centralization
betw = centr_betw(g, directed = F)
saveRDS(betw, here("data", "betw2020.rds"))

# no of students and courses 
# no of students, courses & edges
n_students = sum(V(g)$type)
n_courses  = sum(!V(g)$type)
e_count = ecount(g)

# summary stats
two_mode = list(
    n_students  = n_students,
    n_courses   = n_courses,
    n_edges     = e_count,
    density     = e_count / (n_students * n_courses),
    p1_students = length(comp1[names(comp1) %in% names(students)]) / n_students,
    p1_courses  = length(comp1[names(comp1) %in% names(courses)]) / n_courses,
    b1_students = length(bicomp1[bicomp1 %in% names(students)]) / n_students,
    b1_courses  = length(bicomp1[bicomp1 %in% names(courses)]) / n_courses,
    btw_cntr    = betw$centralization
)



# One-mode projection & analysis -----------------------------------------------

# project to student-to-student 
g_one = bipartite_projection(g, multiplicity = FALSE, which = "true")
saveRDS(g_one, here("data", "onemode20.rds"))

# get geodesic distances
d_mat = distances(g_one)
saveRDS(d_mat, here("data", "distances2020.rds"))
# d_mat = readRDS(here("data, distances2020.rds"))

# keep only finite distances (reachable pairs)
finite_vec = d_mat[is.finite(d_mat)]

# vertex count
n_one = vcount(g_one)

# summary stats

one_mode = list(
    m1_edges   = ecount(g_one),
    m1_density = graph.density(g_one),
    m1_trans   = transitivity(g_one),
    m1_mdist   = mean(finite_vec),
    m1_diam    = max(finite_vec),
    m1_reach_2 = (sum(finite_vec <= 2) - n_one) / (n_one * (n_one - 1)),
    m1_reach_3 = (sum(finite_vec <= 3) - n_one) / (n_one * (n_one - 1)),
    m1_reach_4 = (sum(finite_vec <= 4) - n_one) / (n_one * (n_one - 1))
)



# Combine results & print session info -----------------------------------------

res2020 = list(
    two_mode = two_mode,
    one_mode = one_mode
)

saveRDS(res2020, here("data", "res2020.rds"))

# clear objects (although package will remain attached)
rm(list = ls()[!(ls() %in% obj_list)])

message(
    paste0("Done running script on ", Sys.time(), "\n")
)

### END OF CODE ###

