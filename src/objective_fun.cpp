
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
void test_objectives_list(List& x){
  // Function to test extracting list elements
  // In R, a scalar is a vector of length 1 so we need to us as<double>(x)
  List x_list = clone(x);
  int n = x_list.size();
  Rcout << "list size: " << n << std::endl;
  for(int i = 0; i < n; i++){
    List tmp_list = x_list[i];
     Rcout << "sub_list size: " << tmp_list.size() << std::endl;
     CharacterVector fun_name = tmp_list["name"];
     Rcout << as<std::string>(fun_name) << std::endl;
     NumericVector mult = tmp_list["multiplier"];
     Rcout << as<double>(mult) << std::endl;
     //for(int j = 0; j < tmp_list.size(); j++){
    //   NumericVector y = tmp_list[j];
    //   Rcout << as<double>(y) << std::endl;
    //}
  }
}

//void test_list(List& list){
//  // Function to test extracting list elements
//  // In R, a scalar is a vector of length 1 so we need to us as<double>(x)
//  NumericVector x = list["x"];
//  Rcout << as<double>(x) << std::endl;
//  
//  NumericVector y = list["y"];
//  Rcout << as<double>(y) << std::endl;
//  
//  NumericVector z = list["z"];
//  Rcout << as<double>(z) << std::endl;
//}