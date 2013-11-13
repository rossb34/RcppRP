
#include <Rcpp.h>
using namespace Rcpp;

NumericVector rp_unif(int n, double min, double max){
  // Set seed
  RNGScope scope;
  return runif(n, min, max);
}