// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// typedef must be declared here 
typedef double (*funcPtr)(const arma::colvec& weights, const Rcpp::List& args_list);
