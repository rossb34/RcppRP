#include <RcppArmadilloExtensions/sample.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
// This function is borrowed from the Rcpp Gallery
// http://gallery.rcpp.org/articles/using-the-Rcpp-based-sample-implementation/
// sample_num uses the sample() function from RcppArmadillo to replicate the
// behavior of R's sample() function
NumericVector sample_num(NumericVector x, int size, bool replace, NumericVector prob = NumericVector::create()) {
  RNGScope scope;
  NumericVector ret = RcppArmadillo::sample(x, size, replace, prob);
  return ret;
}