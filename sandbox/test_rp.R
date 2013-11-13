library(PortfolioAnalytics)

data(edhec)
R <- edhec[, 1:4]
funds <- colnames(R)

weight_seq <- seq(from=0, to=1, by=0.01)

pspec <- portfolio.spec(assets=funds, weight_seq=weight_seq)
pspec <- add.constraint(portfolio=pspec, type="weight_sum", min_sum=0.99, max_sum=1.01)
pspec <- add.constraint(portfolio=pspec, type="box", min=0.05, max=0.75)

# pspec$assets
# pspec$weight_seq
# PortfolioAnalytics:::get_constraints(pspec)

rcpp_random_portfolios(portfolio=pspec, method="sample", n_portfolios=10)
random_portfolios(portfolio=pspec, permutations=10, rp_method="sample", eliminate=FALSE)