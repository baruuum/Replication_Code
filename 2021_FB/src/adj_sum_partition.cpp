#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]

using namespace arma;

//' Sum within-partition weights of adjacency matrix
//' 
//' @param x adjacency matrix (may be weighted)
//' @param p integer vector indicating the partition membership of each node
//' @return returns the sum of the within-partition elements
//[[Rcpp::export(.adj_sum_partition)]]
double adj_sum_partition(
    const mat &x,
    const uvec &p)
{

    double x_sum(0.0);
    
    for (uword j = 0; j < x.n_cols; ++j)
        for (uword i = 0; i < x.n_rows; ++i)
            if (p(i) == p(j))
                x_sum += x(i, j);
            
    return x_sum;
    
}
