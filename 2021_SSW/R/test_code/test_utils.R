## Short unit tests for util functions

library("igraph")

con = FALSE
while (!con) {
    g = sample_gnp(10, .4)
    con = is.connected(g)
}

# update_denom_node ------------------------------------------------------------

old_n = vcount(g)
g2 = add_vertices(g, sample.int(4L, 1L) + 1L)
new_n = vcount(g2)

# arbitrary statistic involving nodes which is zero to isolates
deg1 = degree(g)
stat1 = mean(deg1 > 2)

deg2 = degree(g2)
stat2  = mean(deg2 > 2)

res1 = update_denom_node(stat1, old_n, new_n)
res2 = stat2

if (!isTRUE(all.equal(res1, res2))) {
    
    stop("error in update_denom_node function!")
    
} else {
    
    message("update_denom_node unit-test passed!")
    
}


# update_denom_edge ------------------------------------------------------------

# arbitrary statistic involving edges 
den1 = graph.density(g)
den2 = graph.density(g2)

# check 
res1 = update_denom_edge(den1, old_n, new_n, FALSE)
res2 = den2

if (!isTRUE(all.equal(res1, res2))) {
    
    stop("error in update_denom_edge function for simple graphs!")
    
} else {
    
    message("update_denom_edge unit-test passed for simple graphs!")
    
}


# create two-mode network
n = 10
k = 4

con = FALSE
while (!con) {
    X = matrix(
        sample(c(0, 1), n * k, replace = T, prob = c(.6, .4)), 
        nrow = n, 
        ncol = k
    )
    gtwo = graph_from_incidence_matrix(X)
    con = is.connected(gtwo)
}

# density
den1 = ecount(gtwo) / (sum(V(gtwo)$type) * sum(!V(gtwo)$type))

# update density
n_old = sum(!V(gtwo)$type)
n_add = sample.int(4, 1L) + 1L
n_new = n_old + n_add

gtwo2 = add_vertices(gtwo, n_add)
V(gtwo2)$type = c(V(gtwo)$type, rep(FALSE, n_add))

res1 = update_denom_edge(den1, c(n_old, k), c(n_new, k), TRUE)
res2 = ecount(gtwo2) / (sum(V(gtwo2)$type) * sum(!V(gtwo2)$type))


if (!isTRUE(all.equal(res1, res2))) {
    
    stop("error in update_denom_edge function for bipartite graphs!")
    
} else {
    
    message("update_denom_edge unit-test passed for bipartite graphs!")
    
}

# update_centr_betw ------------------------------------------------------------

# centralization
res = centr_betw(g, directed = FALSE)

# add two isolated vertices
g2 = add_vertices(g, 2)

# calculate centr.
res2 = centr_betw(g2, directed = FALSE)
c_igraph = res2$centralization

# use new function
c_fun = update_centr_betw(res, vcount(g2))

# check
if (!isTRUE(all.equal(c_igraph, c_fun))) {
    
    stop("error in update_centr_betw function!")
    
} else {
    
    message("update_centr_betw unit-test passed!")
    
}
    
