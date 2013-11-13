
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace arma;

// typedef also declared in RcppRP.h
typedef double (*funcPtr)(const arma::colvec& weights, const List& args_list);

double rcpp_port_return(const arma::colvec& weights, const List& args_list){
  // Function to compute portfolio variance
  arma::mat mu = args_list["mu"];
  arma::mat port_ret = trans(weights) * mu;
  double out = arma::as_scalar(port_ret);
  return out;
}

double rcpp_port_variance(const arma::colvec& weights, const List& args_list){
  // Function to compute portfolio variance
  arma::mat sigma = args_list["sigma"];
  arma::mat port_var = trans(weights) * sigma * weights;
  double out = arma::as_scalar(port_var);
  return out;
}

double rcpp_port_sd(const arma::colvec& weights, const List& args_list){
  // Function to compute portfolio standard deviation
  double out = sqrt(rcpp_port_variance(weights, args_list));
  return out;
}

double rcpp_port_qu(const arma::colvec& weights, const List& args_list){
  // Function to compute portfolio quadratic utility
  arma::colvec mu = args_list["mu"];
  arma::mat sigma = args_list["sigma"];
  double lambda = as<double>(args_list["lambda"]);
  double port_mean = arma::as_scalar(trans(weights) * mu);
  double port_var = arma::as_scalar(trans(weights) * sigma * weights);
  double out = port_mean - (lambda / 2.0) * port_var;
  return out;
}

double rcpp_port_sharpe(const arma::colvec& weights, const List& args_list){
  // Function to compute portfolio sharpe ratio
  arma::colvec mu = args_list["mu"];
  arma::mat sigma = args_list["sigma"];
  double port_mean = as_scalar(trans(weights) * mu);
  double port_sd = sqrt(as_scalar(trans(weights) * sigma * weights));
  double out = port_mean / port_sd;
  return out;
}

XPtr<funcPtr> putFunPtrInXPtr(std::string fstr) {
    if (fstr == "port_return")
        return(XPtr<funcPtr>(new funcPtr(&rcpp_port_return)));
    else if (fstr == "port_variance")
        return(XPtr<funcPtr>(new funcPtr(&rcpp_port_variance)));
    else if (fstr == "port_sd")
        return(XPtr<funcPtr>(new funcPtr(&rcpp_port_sd)));
    else if (fstr == "port_qu")
        return(XPtr<funcPtr>(new funcPtr(&rcpp_port_qu)));
    else if (fstr == "port_sharpe")
        return(XPtr<funcPtr>(new funcPtr(&rcpp_port_sharpe)));
    else
        return XPtr<funcPtr>(R_NilValue); // runtime error as NULL no XPtr
}

double callViaString(const arma::colvec& weights, const List& args_list, std::string funname) {
    XPtr<funcPtr> xpfun = putFunPtrInXPtr(funname);
    funcPtr fun = *xpfun;
    double y = fun(weights, args_list);
    return (y);
}
