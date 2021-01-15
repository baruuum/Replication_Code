#include <Rcpp.h>
#include "entropy.h"
#include "check_non_negative.h"

using namespace Rcpp;

//' Entropy
//' 
//' Calculates the entropy of a vector \code{x}
//' 
//' @param x numeric or integer object
//' @param na_rm boolean; if true, \code{NA} values are dropped 
//'     from the object before calculating the entropy. Otherwise,
//'     function throws an error when \code{x} contains any \code{NA}
//'     values
//' @return returns the entropy of \code{x}
//' @details the function will renormalize \code{x} to sum to one
//' @export
//[[Rcpp::export(.entropy)]]
double entropy_R(const NumericVector &x, bool na_rm = false) 
{
    
    if (na_rm) 
        return entropy(na_omit(x));
    
    return entropy(x);
    
}

//' Mutual information
//' 
//' Calculates the mutual information of the joint distribution between two
//' variables arranged in a cross table
//' 
//' @param x a numeric or integer matrix
//' @return returns the mutual information of rows and columns of \code{x}
//' @export
//[[Rcpp::export(.mutual_info)]]
double mutual_info(const IntegerMatrix &x)
{
    if (!check_non_negative(x))
        stop(".mutual_info: encountered negative elements");
    
    if (sum(is_na(x)) > 0)
        stop(".mutual_info: encountered missing values");
    
    const int I = x.rows();
    const int J = x.cols();
    
    const double N = sum(x);
    const NumericVector log_r = log(rowSums(x));
    const NumericVector log_c = log(colSums(x));
    
    double mi(0.0);
    
    for (int j = 0; j < J; ++j) 
        for (int i = 0; i < I; ++i) 
            if (x(i,j) > 0)
                mi += x(i,j) * (std::log(x(i,j)) - log_r(i) - log_c(j));
            
    return mi / N + std::log(N);
            
}

//' Expected mutual information for cross table
//' 
//' Calculates the expected value of the mutual information of the rows
//' and columns of a cross table under the hypergeometric model of randomness
//' 
//' @param x a integer matrix
//' @return returns the expected mutual information of the rows and columns
//' @references Vinh et al. 2010. "Information Theoretic Measures for Clusterings Comparison: Variants, Properties, Normalization and Correction for Chance," \emph{Journal of Machine Learning Research} 11: 2837-2854.
//' @export
//[[Rcpp::export(.emi_int)]]
double emi_int(const SEXP &x) 
{
    
    if (TYPEOF(x) != INTSXP)
        stop(".emi_int: input has to be of integer type");
    
    IntegerMatrix y = as<IntegerMatrix>(x);
    
    if (!check_non_negative(y))
        stop(".emi_int: encountered negative elements");
    
    if (sum(is_na(y)) > 0)
        stop(".emi_int: encountered missing values");
    
    const int N = sum(y);
    const double log_N = std::log(N);
    const IntegerVector A = rowSums(y);
    const IntegerVector B = colSums(y);
    const NumericVector log_A = log(A);
    const NumericVector log_B = log(B);
    
    double emi(0.0);
    
    for (int j = 0; j < y.cols(); j++) {        
        
        for (int i = 0; i < y.rows(); i++) {
            
            int a = A(i);
            int b = B(j);
            
            // note that the entropy would be zero if start=0
            int start = std::max(a + b - N, 1);
            int end = std::min(a, b);
            
            if (start <= end) {
                
                double log_a = log_A(i);
                double log_b = log_B(j);
                
                for (int n = start; n < end + 1; ++n) {
                    
                    double log_n = std::log(n);
                    
                    emi += (log_N + log_n - log_a - log_b) *
                        exp(log_n - log_N + R::dhyper(n, a, N - a, b, true));
                    
                }
                
            }
        }
    }
    
    return emi;
    
}