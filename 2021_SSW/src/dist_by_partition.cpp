#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]

using namespace arma;


//' Median distance for partitions based on a distance matrix
//' 
//' @param x matrix of distances
//' @param p_ a partition of the graph (membership vector)
//' @return the median distance within and between partitions
//' @detail The armadillo library is able to handle infinite values; caution has to be taken, however, with missing (NA) values, as they will be treated as arma::datum::inf values.
//[[Rcpp::export]]
mat med_dist_by_partition(const mat &x, const ivec &p_) {
    
    ivec p(p_);
    
    if (min(p) == 1L)
        p -= 1L;
    
    ivec u = unique(p);
    uword K = u.n_elem;
    
    mat res(K, K, fill::zeros);
    
    uword i, j;
    for (j = 0; j < K; ++j) {
        for (i = j; i < K; ++i) {
            
            uvec uj = find(p == j);
            uvec ui = find(p == i);
            
            res(i, j) = median(nonzeros(x.submat(ui, uj)));
            
        }
    }
    
    return res;
    
}


// helper function to sort two (positive integer) values
inline uvec sort_two(uword x, uword y) {
    
    uvec res{x, y};
    
    if (x >= y)
        return res;
    
    return reverse(res);
    
}

//' Mean distance for partitions based on a distance matrix
//' 
//' @param x matrix of distances
//' @param p_ a partition of the graph (membership vector)
//' @return the mean distance within and between partitions
//' @detail The distance is only calculated for pairs of vertices that have finite distances
//[[Rcpp::export]]
mat mean_dist_by_partition(const mat &x, const ivec &p_) {
    
    ivec p(p_);

    if (min(p) == 1L)
        p -= 1L;
    
    ivec u = unique(p);
    
    uword N = x.n_rows;
    uword K = u.n_elem;
    
    mat res(K, K, fill::zeros);
    umat n_finite(K, K, fill::zeros);
    
    uword i, j;
    
    for (j = 0; j < N; ++j) {
        
        for (i = j + 1L; i < N; ++i) {
            
            if (x(i, j) < datum::inf) {
                
                uvec p_ij = sort_two(p(i), p(j));
                res(p_ij(0L), p_ij(1L)) += x(i, j);
                ++n_finite(p_ij(0L), p_ij(1L));
                    
            } 

        }
        
    }
    
    res /= conv_to<mat>::from(n_finite);
    
    // fill upper triangular values with zeros and return
   
    return trimatl(res);
    
}
