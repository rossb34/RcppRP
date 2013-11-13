
library(PortfolioAnalytics)

set.seed(123)
R <- matrix(rnorm(60), nrow=12, ncol=5)
colnames(R) <- c("A", "B", "C", "D", "E")
funds <- colnames(R)
n <- ncol(R)
weights <- rep(1/n, n)

moments <- set_moments(R, momentargs=NULL)
args_list1 <- c(moments, lambda=2.0)

pspec <- portfolio.spec(assets=funds)
pspec <- add.constraint(portfolio=pspec, type="weight_sum", min_sum=0.99, max_sum=1.01)
pspec <- add.constraint(portfolio=pspec, type="box", min=0.05, max=0.75)
pspec <- add.objective(portfolio=pspec, type="return", name="mean")
pspec <- add.objective(portfolio=pspec, type="risk", name="sd")
# pspec <- add.objective(portfolio=pspec, type="risk", name="sd", multiplier=0)

obj_list <- pspec$objectives

# actual objectives list from PortfolioAnalytics
rcpp_constrained_objective_v2(weights, obj_list, args_list1)

tmp_list <- list()
tmp_list[[1]] <- list(name="mean", multiplier=-1, bar="foo")
tmp_list[[2]] <- list(name="sd", multiplier=1)
test_objectives_list(tmp_list)

