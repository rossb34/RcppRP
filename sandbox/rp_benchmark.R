
library(PortfolioAnalytics)

n_assets <- 150

n_portfolios <- 2000

weight_seq <- generatesequence(min=0, max=0.05, by=0.001)

init.portf <- portfolio.spec(assets=n_assets, weight_seq=weight_seq)
init.portf <- add.constraint(portfolio=init.portf, type="weight_sum", min_sum=0.99, max_sum=1.01)
init.portf <- add.constraint(portfolio=init.portf, type="box", min=0.001, max=0.05)

# rp_pa <- random_portfolios(portfolio=init.portf, permutations=n_portfolios, rp_method="sample")

# rp_rcpp <- rcpp_random_portfolios(portfolio=init.portf, method="sample", n_portfolios=n_portfolios, max_permutations=200)

library(rbenchmark)
benchmark(pa=random_portfolios(portfolio=init.portf, permutations=n_portfolios, rp_method="sample"),
          rcpp_s=rcpp_random_portfolios(portfolio=init.portf, method="sample", n_portfolios=n_portfolios, max_permutations=200),
          replications=1)[,1:4]
