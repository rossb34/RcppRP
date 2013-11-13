
# Test the objective functions in C++ and R

# library(PortfolioAnalytics)

set.seed(123)
R <- matrix(rnorm(60), nrow=12, ncol=5)
colnames(R) <- c("A", "B", "C", "D", "E")
funds <- colnames(R)

# pspec <- portfolio.spec(assets=funds)
# pspec <- add.constraint(portfolio=pspec, type="weight_sum", min_sum=0.99, max_sum=1.01)
# pspec <- add.constraint(portfolio=pspec, type="box", min=0.05, max=0.75)
# pspec <- add.objective(portfolio=pspec, type="return", name="mean")
# pspec <- add.objective(portfolio=pspec, type="risk", name="StdDev")

weights=rep(1/5, 5)
lambda = 2

moments <- set_moments(R, momentargs=NULL)

args_list1 <- c(moments, lambda=2.0)

rcpp_constrained_objective(weights, args_list1, "max_return")

rcpp_constrained_objective(weights, args_list1, "min_variance")

rcpp_constrained_objective(weights, args_list1, "min_sd")

rcpp_constrained_objective(weights, args_list1, "max_qu")

rcpp_constrained_objective(weights, args_list1, "max_sharpe")

rcpp_constrained_objective(weights, args_list1, "blah")



# ##### Test portfolio return #####
# port_return <- function(weights, args_list){
#   mu <- args_list$mu
#   as.numeric(t(weights) %*% mu)
# }
# 
# # portfolio variance
# stopifnot(all.equal(rcpp_port_return(weights, args_list1), port_return(weights, args_list1)))
# 
# ##### Test portfoio variance #####
# port_variance <- function(weights, args_list){
#   sigma <- args_list$sigma
#   as.numeric(t(weights) %*% sigma %*% weights)
# }
# 
# # portfolio variance
# stopifnot(all.equal(rcpp_port_variance(weights, args_list1), port_variance(weights, args_list1)))
# 
# ##### Test portfolio quadratic utility #####
# port_qu <- function(weights, args_list, ...){
#   mu <- args_list$mu
#   sigma <- args_list$sigma
#   lambda <- args_list$lambda
#   port_mean <- as.numeric(t(weights) %*% mu)
#   port_var <- as.numeric(t(weights) %*% sigma %*% weights)
#   port_mean - (lambda / 2) * port_var
# }
# 
# # portfolio quadratic utility
# stopifnot(all.equal(rcpp_port_qu(weights, args_list2), port_qu(weights, args_list2)))
# 
# ##### Test portfolio sharpe ratio #####
# port_sharpe <- function(weights, args_list, ...){
#   mu <- args_list$mu
#   sigma <- args_list$sigma
#   port_mean <- as.numeric(t(weights) %*% mu)
#   port_sd <- sqrt(as.numeric(t(weights) %*% sigma %*% weights))
#   port_mean / port_sd
# }
# 
# # portfolio sharpe ratio
# stopifnot(all.equal(rcpp_port_sharpe(weights, args_list1), port_sharpe(weights, args_list1)))
# 
# 
