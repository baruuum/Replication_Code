# short unit-test for procrustes analysis

library("igraph")
library("here")

Rcpp::sourceCpp(here("src", "dist_by_partition.cpp"))

n = 20
k = 3
connected = TRUE

while(connected) {
    
    g = sample_gnp(n, .4)
    connected = is.connected(g)    
    
}

D = distances(g)
p = sample(1:k, n, TRUE)

med_res = mean_res = matrix(0.0, k, k)

for (j in seq_len(k)) {
    
    for (i in j:k) {
        
        tmp = D[p == i, p == j]
        
        if (i == j) {
            med_res[i, j] = median(tmp[lower.tri(tmp)])
        } else {
            tmp = D[p == i, p == j]
            med_res[i, j] = median(tmp)
        }
            
        
    }
    
}

for (j in seq_len(k)) {
    
    for (i in j:k) {

        tmp = D[p == i, p == j]
        
        if (i == j) {
            tmp = tmp[lower.tri(tmp)]
            mean_res[i, j] = mean(tmp[is.finite(tmp)])
        } else {
            mean_res[i, j] = mean(tmp[is.finite(tmp)])
        }
        
        
    }
    
}

# test
med_res_2 = med_dist_by_partition(D, p)
mean_res_2 = mean_dist_by_partition(D, p)

stopifnot(
    all(med_res_2 == med_res),
    all(mean_res == mean_res_2)
)

message("All checks for distance function passed!")
