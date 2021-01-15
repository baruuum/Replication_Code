#ifndef ENTROPY_H
#define ENTROPY_H

#include "check_non_negative.h"

/*
 
 Entropy
 
 Calculates the entropy of a vector \code{x}
 
 @param x numeric or integer object
 @return returns the entropy of \code{x}
 @details the function will renormalize \code{x} to sum to one
 
 */


template <typename T>
inline double entropy(const T & x) {
    
    using std::log;
    
    if (!check_non_negative(x))
        throw std::range_error("input has negative elements");
    
    double ent(0.0);
    double N = std::accumulate(x.begin(), x.end(), 0.0);
    double log_N = log(N);
    
    typename T::const_iterator i;
    
    for (i = x.begin(); i != x.end(); ++i)
        if (*i > 0)
            ent -= *i * (log(*i) - log_N);
        
    return ent / N;
        
}

#endif