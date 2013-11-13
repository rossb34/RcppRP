
# test rcpp_rp_optimize

library(PortfolioAnalytics)

set.seed(123)
R <- matrix(rnorm(60), nrow=12, ncol=5)
colnames(R) <- c("A", "B", "C", "D", "E")
funds <- colnames(R)

moments <- set_moments(R, momentargs=NULL)
args_list1 <- c(moments, lambda=2.0)

pspec <- portfolio.spec(assets=funds)
pspec <- add.constraint(portfolio=pspec, type="weight_sum", min_sum=0.99, max_sum=1.01)
pspec <- add.constraint(portfolio=pspec, type="box", min=0.05, max=0.75)
pspec <- add.objective(portfolio=pspec, type="return", name="mean")
pspec <- add.objective(portfolio=pspec, type="risk", name="sd", multiplier=0)

# rp_weights <- rcpp_random_portfolios(pspec, method="sample", n_portfolios=10)

rp_weights <- cbind(c(0.200, 0.200, 0.200, 0.200, 0.200),
                    c(0.194, 0.070, 0.330, 0.342, 0.056),
                    c(0.398, 0.318, 0.058, 0.098, 0.128),
                    c(0.182, 0.382, 0.154, 0.202, 0.070),
                    c(0.248, 0.094, 0.196, 0.142, 0.320),
                    c(0.392, 0.052, 0.070, 0.296, 0.192),
                    c(0.260, 0.128, 0.320, 0.124, 0.166),
                    c(0.088, 0.248, 0.062, 0.076, 0.520),
                    c(0.354, 0.106, 0.054, 0.118, 0.372),
                    c(0.236, 0.090, 0.050, 0.064, 0.568))

rp_weights

# Version 1: Objective specified as a string
rcpp_rp_optimize(rp=rp_weights, args_list=args_list1, objective="min_variance")

rcpp_rp_optimize(rp=rp_weights, args_list=args_list1, objective="min_sd")

rcpp_rp_optimize(rp=rp_weights, args_list=args_list1, objective="max_qu")

rcpp_rp_optimize(rp=rp_weights, args_list=args_list1, objective="max_sharpe")

rcpp_rp_optimize(rp=rp_weights, args_list=args_list1, objective="max_return")

# Version 2: Objectives are grabbed from the portfolio$objectives list
x <- rcpp_rp_optimize_v2(rp=rp_weights, objectives_list=pspec$objectives, args_list=args_list1)
t(x)
x.t <- t(x)

colnames(x.t) <- c(unlist(lapply(pspec$objectives, function(x) x$name)), "out")
x.t

m_args <- list()
m_args$sigma=diag(5)

# rp_optimize_v1(R=R, portfolio=pspec, search_size=20, momentargs=m_args)
# rp_optimize_v1(R=R, portfolio=pspec, search_size=20, rp=rp_weights)

rp_optimize_v1(R=R, objective="max_return", portfolio=pspec, rp=rp_weights)

rp_optimize_v2(R=R, portfolio=pspec, rp=rp_weights)
