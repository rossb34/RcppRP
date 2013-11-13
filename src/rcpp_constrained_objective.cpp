
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include "objective_fun.h"

using namespace Rcpp;

// [[Rcpp::export]]
double rcpp_constrained_objective(const arma::colvec& weights,
                                  const List& args_list,
                                  std::string objective,
                                  double penalty=10000
                                  ){
  double out, tmp;
  // List obj;
  out = 0;
  tmp = 0;
  
  // check min_sum and max_sum constraints
  // check box constraints
  
  // I don't think I need the pointer and callViaString
  // double out = callViaString(weights, args_list, fun_name);
  if(objective == "max_return"){
    // Rcout << "max_return" << std::endl;
    tmp = rcpp_port_return(weights, args_list);
    out = out + -1.0 * tmp;
    //obj = List::create(Rcpp::Named("mean") = tmp);
  } else if(objective == "min_variance"){
    // Rcout << "min_variance" << std::endl;
    tmp = rcpp_port_variance(weights, args_list);
    out = out + 1.0 * tmp;
    //obj = List::create(Rcpp::Named("var") = tmp);
  } else if(objective == "min_sd"){
    // Rcout << "min_sd" << std::endl;
    tmp = rcpp_port_sd(weights, args_list);
    out = out + -1.0 * tmp;
    //obj = List::create(Rcpp::Named("StdDev") = tmp);
  } else if(objective == "max_qu"){
    // Rcout << "max_qu" << std::endl;
    tmp = rcpp_port_qu(weights, args_list);
    out = out + -1.0 * tmp;
    //obj = List::create(Rcpp::Named("qu") = tmp);
  } else if(objective == "max_sharpe"){
    // Rcout << "max_sharpe" << std::endl;
    tmp = rcpp_port_sharpe(weights, args_list);
    out = out + -1.0 * tmp;
    //obj = List::create(Rcpp::Named("sharpe") = tmp);
  } else {
    // Try to catch this early in an R wrapper function
    Rcout << "objective not supported" << std::endl;
    return 0;
    // return R_NilValue;
  }
  return out;
  // return Rcpp::List::create(Rcpp::Named("out") = out,
  //                           Rcpp::Named("obj") = obj);
}

// [[Rcpp::export]]
arma::vec rcpp_rp_optimize(const arma::mat& rp, const List& args_list, std::string objective){
  
  // get the number of columns in rp
  int num_cols = rp.n_cols;
  
  // create a vector to store the results
  arma::vec res(num_cols);
  for(int i = 0; i < num_cols; i++){
    res[i] = rcpp_constrained_objective(rp.col(i), args_list, objective);
  }
  return res;
}

// [[Rcpp::export]]
std::vector<double> rcpp_constrained_objective_v2(const arma::colvec& weights,
                                        List& objectives_list,
                                        const List& args_list,
                                        double penalty=10000
                                        ){
  double out, tmp;
  // List tmp_measure;
  
  out = 0.0;
  tmp = 0.0;
  
  // check min_sum and max_sum constraints
  // check box constraints
  // check other constraints
  
  List obj_list = clone(objectives_list);
  int n = obj_list.size();
  std::vector<double> res;
  for(int i = 0; i < n; i++){
    List tmp_list = obj_list[i];
    // get the "name" element from the objectives list
    CharacterVector tmp_name = tmp_list["name"];
    std::string fun_name = as<std::string>(tmp_name);
    
    // get the "multiplier" element from the objectives list
    NumericVector tmp_mult = tmp_list["multiplier"];
    double mult = as<double>(tmp_mult);
    
    // check for "target" element
    // check for "arguments" element
    // These could be empty or NULL
    
    if(fun_name == "mean"){
      tmp = rcpp_port_return(weights, args_list);
      // Rcout << "mean: " << tmp << std::endl;
      out = out + mult * tmp;
      // tmp_measure["mean"] = tmp;
      res.push_back(tmp);
    } else if(fun_name == "var"){
      tmp = rcpp_port_variance(weights, args_list);
      // Rcout << "var: " << tmp << std::endl;
      out = out + mult * tmp;
      // tmp_measure["var"] = tmp;
      res.push_back(tmp);
    } else if(fun_name == "sd"){
      tmp = rcpp_port_sd(weights, args_list);
      // Rcout << "sd: " << tmp << std::endl;
      out = out + mult * tmp;
      // tmp_measure["sd"] = tmp;
      res.push_back(tmp);
    } else {
      // Try to catch this early in an R wrapper function
      Rcout << "objective not supported" << std::endl;
      // return 0;
      // return R_NilValue;
    }
  }
  //return Rcpp::List::create(Rcpp::Named("out") = out,
  //                          Rcpp::Named("obj") = tmp_measure);
  // Rcout << "out: " << out << std::endl;
  res.push_back(out);
  return res;
}

// [[Rcpp::export]]
arma::mat rcpp_rp_optimize_v2(const arma::mat& rp, 
                              List& objectives_list,
                              const List& args_list
                              ){
  
  // get the number of columns in rp
  int num_cols = rp.n_cols;
  int num_objs = objectives_list.size() + 1;
  // num_objs is the number of objectives + 1 for the out value
  
  // create a matrix to store the results for each iteration
  arma::mat res(num_objs, num_cols);
  
  // create a vector to temporarily store the results from rcpp_constrained_objective_v2
  std::vector<double> tmp;
  for(int i = 0; i < num_cols; i++){
    tmp = rcpp_constrained_objective_v2(rp.col(i), objectives_list, args_list);
    res.col(i) = arma::vec(tmp);
  }
  return res;
}
