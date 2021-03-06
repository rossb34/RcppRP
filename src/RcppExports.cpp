// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>
#include "../inst/include/RcppRP.h"

using namespace Rcpp;

// test_objectives_list
void test_objectives_list(List& x);
RcppExport SEXP RcppRP_test_objectives_list(SEXP xSEXP) {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< List& >::type x(xSEXP );
        test_objectives_list(x);
    }
    return R_NilValue;
END_RCPP
}
// rcpp_constrained_objective
double rcpp_constrained_objective(const arma::colvec& weights, const List& args_list, std::string objective, double penalty = 10000);
RcppExport SEXP RcppRP_rcpp_constrained_objective(SEXP weightsSEXP, SEXP args_listSEXP, SEXP objectiveSEXP, SEXP penaltySEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const arma::colvec& >::type weights(weightsSEXP );
        Rcpp::traits::input_parameter< const List& >::type args_list(args_listSEXP );
        Rcpp::traits::input_parameter< std::string >::type objective(objectiveSEXP );
        Rcpp::traits::input_parameter< double >::type penalty(penaltySEXP );
        double __result = rcpp_constrained_objective(weights, args_list, objective, penalty);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// rcpp_rp_optimize
arma::vec rcpp_rp_optimize(const arma::mat& rp, const List& args_list, std::string objective);
RcppExport SEXP RcppRP_rcpp_rp_optimize(SEXP rpSEXP, SEXP args_listSEXP, SEXP objectiveSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const arma::mat& >::type rp(rpSEXP );
        Rcpp::traits::input_parameter< const List& >::type args_list(args_listSEXP );
        Rcpp::traits::input_parameter< std::string >::type objective(objectiveSEXP );
        arma::vec __result = rcpp_rp_optimize(rp, args_list, objective);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// rcpp_constrained_objective_v2
std::vector<double> rcpp_constrained_objective_v2(const arma::colvec& weights, List& objectives_list, const List& args_list, double penalty = 10000);
RcppExport SEXP RcppRP_rcpp_constrained_objective_v2(SEXP weightsSEXP, SEXP objectives_listSEXP, SEXP args_listSEXP, SEXP penaltySEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const arma::colvec& >::type weights(weightsSEXP );
        Rcpp::traits::input_parameter< List& >::type objectives_list(objectives_listSEXP );
        Rcpp::traits::input_parameter< const List& >::type args_list(args_listSEXP );
        Rcpp::traits::input_parameter< double >::type penalty(penaltySEXP );
        std::vector<double> __result = rcpp_constrained_objective_v2(weights, objectives_list, args_list, penalty);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// rcpp_rp_optimize_v2
arma::mat rcpp_rp_optimize_v2(const arma::mat& rp, List& objectives_list, const List& args_list);
RcppExport SEXP RcppRP_rcpp_rp_optimize_v2(SEXP rpSEXP, SEXP objectives_listSEXP, SEXP args_listSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const arma::mat& >::type rp(rpSEXP );
        Rcpp::traits::input_parameter< List& >::type objectives_list(objectives_listSEXP );
        Rcpp::traits::input_parameter< const List& >::type args_list(args_listSEXP );
        arma::mat __result = rcpp_rp_optimize_v2(rp, objectives_list, args_list);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// rcpp_hello_world
List rcpp_hello_world();
RcppExport SEXP RcppRP_rcpp_hello_world() {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        List __result = rcpp_hello_world();
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// rcpp_rp_sample
NumericVector rcpp_rp_sample(NumericVector init_weights, NumericVector weight_seq, double min_sum, double max_sum, NumericVector min_box, NumericVector max_box, int max_permutations);
RcppExport SEXP RcppRP_rcpp_rp_sample(SEXP init_weightsSEXP, SEXP weight_seqSEXP, SEXP min_sumSEXP, SEXP max_sumSEXP, SEXP min_boxSEXP, SEXP max_boxSEXP, SEXP max_permutationsSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< NumericVector >::type init_weights(init_weightsSEXP );
        Rcpp::traits::input_parameter< NumericVector >::type weight_seq(weight_seqSEXP );
        Rcpp::traits::input_parameter< double >::type min_sum(min_sumSEXP );
        Rcpp::traits::input_parameter< double >::type max_sum(max_sumSEXP );
        Rcpp::traits::input_parameter< NumericVector >::type min_box(min_boxSEXP );
        Rcpp::traits::input_parameter< NumericVector >::type max_box(max_boxSEXP );
        Rcpp::traits::input_parameter< int >::type max_permutations(max_permutationsSEXP );
        NumericVector __result = rcpp_rp_sample(init_weights, weight_seq, min_sum, max_sum, min_box, max_box, max_permutations);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// rcpp_rp_unif
NumericVector rcpp_rp_unif(NumericVector init_weights, double min_sum, double max_sum, NumericVector min_box, NumericVector max_box, int max_permutations);
RcppExport SEXP RcppRP_rcpp_rp_unif(SEXP init_weightsSEXP, SEXP min_sumSEXP, SEXP max_sumSEXP, SEXP min_boxSEXP, SEXP max_boxSEXP, SEXP max_permutationsSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< NumericVector >::type init_weights(init_weightsSEXP );
        Rcpp::traits::input_parameter< double >::type min_sum(min_sumSEXP );
        Rcpp::traits::input_parameter< double >::type max_sum(max_sumSEXP );
        Rcpp::traits::input_parameter< NumericVector >::type min_box(min_boxSEXP );
        Rcpp::traits::input_parameter< NumericVector >::type max_box(max_boxSEXP );
        Rcpp::traits::input_parameter< int >::type max_permutations(max_permutationsSEXP );
        NumericVector __result = rcpp_rp_unif(init_weights, min_sum, max_sum, min_box, max_box, max_permutations);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// rcpp_random_portfolios_sample
NumericMatrix rcpp_random_portfolios_sample(int n_portfolios, const NumericVector& init_weights, const NumericVector& weight_seq, const double& min_sum, const double& max_sum, const NumericVector& min_box, const NumericVector& max_box, int max_permutations);
RcppExport SEXP RcppRP_rcpp_random_portfolios_sample(SEXP n_portfoliosSEXP, SEXP init_weightsSEXP, SEXP weight_seqSEXP, SEXP min_sumSEXP, SEXP max_sumSEXP, SEXP min_boxSEXP, SEXP max_boxSEXP, SEXP max_permutationsSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< int >::type n_portfolios(n_portfoliosSEXP );
        Rcpp::traits::input_parameter< const NumericVector& >::type init_weights(init_weightsSEXP );
        Rcpp::traits::input_parameter< const NumericVector& >::type weight_seq(weight_seqSEXP );
        Rcpp::traits::input_parameter< const double& >::type min_sum(min_sumSEXP );
        Rcpp::traits::input_parameter< const double& >::type max_sum(max_sumSEXP );
        Rcpp::traits::input_parameter< const NumericVector& >::type min_box(min_boxSEXP );
        Rcpp::traits::input_parameter< const NumericVector& >::type max_box(max_boxSEXP );
        Rcpp::traits::input_parameter< int >::type max_permutations(max_permutationsSEXP );
        NumericMatrix __result = rcpp_random_portfolios_sample(n_portfolios, init_weights, weight_seq, min_sum, max_sum, min_box, max_box, max_permutations);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// rcpp_random_portfolios_unif
NumericMatrix rcpp_random_portfolios_unif(int n_portfolios, const NumericVector& init_weights, const double& min_sum, const double& max_sum, const NumericVector& min_box, const NumericVector& max_box, int max_permutations);
RcppExport SEXP RcppRP_rcpp_random_portfolios_unif(SEXP n_portfoliosSEXP, SEXP init_weightsSEXP, SEXP min_sumSEXP, SEXP max_sumSEXP, SEXP min_boxSEXP, SEXP max_boxSEXP, SEXP max_permutationsSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< int >::type n_portfolios(n_portfoliosSEXP );
        Rcpp::traits::input_parameter< const NumericVector& >::type init_weights(init_weightsSEXP );
        Rcpp::traits::input_parameter< const double& >::type min_sum(min_sumSEXP );
        Rcpp::traits::input_parameter< const double& >::type max_sum(max_sumSEXP );
        Rcpp::traits::input_parameter< const NumericVector& >::type min_box(min_boxSEXP );
        Rcpp::traits::input_parameter< const NumericVector& >::type max_box(max_boxSEXP );
        Rcpp::traits::input_parameter< int >::type max_permutations(max_permutationsSEXP );
        NumericMatrix __result = rcpp_random_portfolios_unif(n_portfolios, init_weights, min_sum, max_sum, min_box, max_box, max_permutations);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
