
#include <Rcpp.h>
using namespace Rcpp;

// http://gallery.rcpp.org/articles/stl-transform-for-subsetting/

const double flagval = __DBL_MIN__; // works
//const double flagval = NA_REAL;   // does not

// simple double value 'flagging' function
inline double flag(double a, bool b) { return b ? a : flagval; }

// function to subset vector a based on a logical vector b
NumericVector subsetter(NumericVector a, LogicalVector b) {
    // We use the flag() function to mark values of 'a' 
    // for which 'b' is false with the 'flagval'
    // Need to make a clone of a, otherwise it will be modified
    NumericVector xx = clone(a);
    std::transform(xx.begin(), xx.end(), b.begin(), xx.begin(), flag);

    // We use sugar's sum to compute how many true values to expect
    NumericVector res = NumericVector(sum(b));

    // And then copy the ones different from flagval from a into
    // res using the remove_copy function from the STL
    std::remove_copy(xx.begin(), xx.end(), res.begin(), flagval);
    return res;    
}
