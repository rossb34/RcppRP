
#include <RcppArmadillo.h>
#include "subsetter.h"
#include "sample_num.h"
#include "rp_unif.h"

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector rcpp_rp_sample(NumericVector init_weights,
                             NumericVector weight_seq,
                             double min_sum,
                             double max_sum,
                             NumericVector min_box,
                             NumericVector max_box,
                             int max_permutations
                             ) {
  // get the number of assets from the initial portfolio
  int nassets = init_weights.size();
  
  // Create a random index
  NumericVector random_index(nassets);
  for(int i=0; i < nassets; i++){
      random_index[i] = i;
  }
  // Use random_shuffle to shuffle the index
  std::random_shuffle(random_index.begin(), random_index.end());
  
  // Declare some variables
  NumericVector tmp_portfolio(nassets);
  int cur_index;
  double cur_val;
  double val;
  NumericVector sample_val(1);
  NumericVector tmp_seq;
  
  // Create a temporary portfolio 
  for(int i = 0; i < nassets; i++){
    cur_index = random_index[i];
    tmp_seq = subsetter(weight_seq, (weight_seq <= max_box[cur_index]) & (weight_seq >= min_box[cur_index]));
    // sample_val = sample_num(subsetter(weight_seq, (weight_seq <= max_box[cur_index]) & (weight_seq >= min_box[cur_index])), 1, FALSE);
    sample_val = sample_num(tmp_seq, 1, FALSE);
    val = sample_val[0];
    tmp_portfolio[cur_index] = val;
  }
  
  // initialize the permutations counter
  int permutations = 1;
  
  // outer while loop to check tmp_portfolio weights for violation of min_sum and max_sum
  while((sum(tmp_portfolio) < min_sum | sum(tmp_portfolio) > max_sum) & permutations <= max_permutations){
      permutations ++;
      
      int i = 0;
      // shuffle the random index
      std::random_shuffle(random_index.begin(), random_index.end());
      
      // loop while min_sum is violated
      while(sum(tmp_portfolio) <= min_sum & i < nassets){
        cur_index = random_index[i];
        cur_val = tmp_portfolio[cur_index];
        // subset weight_seq for elements that are greater than cur_val and less than max_box[cur_index]
        tmp_seq = subsetter(weight_seq, (weight_seq > cur_val) & (weight_seq <= max_box[cur_index]) );
        if(tmp_seq.size() > 1){
          sample_val = sample_num(tmp_seq, 1, FALSE);
        } else if(tmp_seq.size() == 1){
          sample_val = tmp_seq;
        }
        val = sample_val[0];
        tmp_portfolio[cur_index] = val;
        i++;
      } // end of loop while min_sum violated
      
      // reset i for max_sum loop
      i = 0;
      // loop while max_sum is violated
      while(sum(tmp_portfolio) >= max_sum & i < nassets){
        cur_index = random_index[i];
        cur_val = tmp_portfolio[cur_index];
        // subset weight_seq for elements that are less than cur_val and greater than min_box[cur_index]
        tmp_seq = subsetter(weight_seq, (weight_seq < cur_val) & (weight_seq >= min_box[cur_index]) );
        if(tmp_seq.size() > 1){
          sample_val = sample_num(tmp_seq, 1, FALSE);
        } else if(tmp_seq.size() == 1){
          sample_val = tmp_seq;
        }
        val = sample_val[0];
        tmp_portfolio[cur_index] = val;
        i++;
      } // end of loop while min_sum violated
  } // end outer while loop
  return tmp_portfolio;
}

// [[Rcpp::export]]
NumericVector rcpp_rp_unif(NumericVector init_weights,
                          double min_sum,
                          double max_sum,
                          NumericVector min_box,
                          NumericVector max_box,
                          int max_permutations
                          ) {
  // get the number of assets from the initial portfolio
  int nassets = init_weights.size();
  
  // Create a random index
  NumericVector random_index(nassets);
  for(int i=0; i < nassets; i++){
      random_index[i] = i;
  }
  // Use random_shuffle to shuffle the index
  std::random_shuffle(random_index.begin(), random_index.end());
  
  // Declare some variables
  NumericVector tmp_portfolio(nassets);
  int cur_index;
  double cur_val;
  
  // Create a temporary portfolio 
  for(int i = 0; i < nassets; i++){
    cur_index = random_index[i];
    // Uniform random draws for weights to create temporary portfolio
    tmp_portfolio[cur_index] = rp_unif(1, min_box[cur_index], max_box[cur_index])[0];
  }
  
  // initialize the permutations counter
  int permutations = 1;
  
  // outer while loop to check tmp_portfolio weights for violation of min_sum and max_sum
  while((sum(tmp_portfolio) < min_sum | sum(tmp_portfolio) > max_sum) & permutations <= max_permutations){
      permutations ++;
      
      int i = 0;
      // shuffle the random index
      std::random_shuffle(random_index.begin(), random_index.end());
      
      // loop while min_sum is violated
      while(sum(tmp_portfolio) <= min_sum & i < nassets){
        cur_index = random_index[i];
        cur_val = tmp_portfolio[cur_index];
        tmp_portfolio[cur_index] = rp_unif(1, cur_val, max_box[cur_index])[0];
        i++;
      } // end of loop while min_sum violated
      
      // reset i for max_sum loop
      i = 0;
      // loop while max_sum is violated
      while(sum(tmp_portfolio) >= max_sum & i < nassets){
        cur_index = random_index[i];
        cur_val = tmp_portfolio[cur_index];
        tmp_portfolio[cur_index] = rp_unif(1, min_box[cur_index], cur_val)[0];
        i++;
      } // end of loop while min_sum violated
  } // end outer while loop
  return tmp_portfolio;
}

// [[Rcpp::export]]
NumericMatrix rcpp_random_portfolios_sample(int n_portfolios, 
                                            const NumericVector& init_weights, 
                                            const NumericVector& weight_seq, 
                                            const double& min_sum, 
                                            const double& max_sum, 
                                            const NumericVector& min_box, 
                                            const NumericVector& max_box, 
                                            int max_permutations
                                            ){
  // function to repeatedly call rp_randomize and build a matrix of random portfolios
  // create a matrix with number of rows equal to nassets and number of columns equal to n_portfolios
  int nassets = min_box.size();
  NumericMatrix out(nassets, n_portfolios);
  for(int i = 0; i < n_portfolios; i++){
    out(_, i) = rcpp_rp_sample(init_weights, weight_seq, min_sum, max_sum, min_box, max_box, max_permutations);
  }
  return out;
}

// [[Rcpp::export]]
NumericMatrix rcpp_random_portfolios_unif(int n_portfolios, 
                                          const NumericVector& init_weights, 
                                          const double& min_sum, 
                                          const double& max_sum, 
                                          const NumericVector& min_box, 
                                          const NumericVector& max_box, 
                                          int max_permutations
                                          ){
  // function to repeatedly call rp_randomize and build a matrix of random portfolios
  // create a matrix with number of rows equal to nassets and number of columns equal to n_portfolios
  int nassets = min_box.size();
  NumericMatrix out(nassets, n_portfolios);
  for(int i = 0; i < n_portfolios; i++){
    out(_, i) = rcpp_rp_unif(init_weights, min_sum, max_sum, min_box, max_box, max_permutations);
  }
  return out;
}
