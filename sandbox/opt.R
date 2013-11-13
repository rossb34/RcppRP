

# random portfolio optimization
# This is what the R function could look like
# rp_optimize(R, portfolio, ...)

# calculate the moments
# set_moments

# generate the random portfolios
# rcpp_random_portfolios

# loop over random portfolios calling an objective function

# constrained_objective
# penalize constraints that are violated
# calculate an objective
# penalize the objective
# return out, weights, objective values

# constrained_objective(w, R, portfolio, ...)
# rp_objective_results <- foreach(ii=1:nrow(rp), .errorhandling='pass') %dopar% constrained_objective(w=rp[ii,], R, portfolio, trace=trace,...=dotargs, normalize=FALSE)


# simple way to

# The function argument for each rcpp objective function looks like
# const arma::colvec& weights, const List& args_list
# rcpp_port_return
# rcpp_port_variance
# rcpp_port_sd
# rcpp_port_qu
# rcpp_port_sharpe

# rcpp_rp_optimize(rp, objective, moments)

# How do I want to call this from R?

#' Random portfolio optimization
#' 
#' @param R xts or matrix of asset returns
#' @param constraints list of constraints used to generate random portfolios.
#' This could be a portfolio object from PortfolioAnalytics
#' @param objective string for which objective to minimize or maximize
#' @param \dots passthru parameters to the objective function
rp_optimize <- function(R, constraints, objective, ...){
  # R to calculate the moments
  moments <- set_moments(R)
  # The moments and any other arguments (e.g. lambda for qu, p for ETL, etc.)
  # need to be combined into a single flat list and passed as the args_list
  # argument to the rcpp function
}




