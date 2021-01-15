#ifndef CHECK_NON_NEGATIVE_H
#define CHECK_NON_NEGATIVE_H

/*
 * Checks whether all elements of T are non-negative
 */

template <typename T>
inline bool check_non_negative(const T &x)
{
    
    typename T::const_iterator i;
    
    for (i = x.begin(); i != x.end(); ++i) 
        if (*i < 0) 
            return false;
        
    return true;
        
}

#endif