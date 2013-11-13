
library(PortfolioAnalytics)
library(RcppRP)

# The multicore package, and therefore registerDoMC, should not be used in a
# GUI environment, because multiple processes then share the same GUI. Only use
# when running from the command line.
# require(doMC)
# registerDoMC(3)

data(edhec)
R <- edhec[,1:10]
funds <- colnames(R)

n_portfolios <- 5000

weight_seq <- generatesequence(min=0, max=0.45, by=0.001)

init.portf <- portfolio.spec(assets=funds, weight_seq=weight_seq)
init.portf <- add.constraint(portfolio=init.portf, type="weight_sum", min_sum=0.99, max_sum=1.01)
init.portf <- add.constraint(portfolio=init.portf, type="box", min=0.001, max=0.45)
init.portf <- add.objective(portfolio=init.portf, type="return", name="mean")
init.portf <- add.objective(portfolio=init.portf, type="risk", name="sd")

library(rbenchmark)
benchmark(pa=optimize.portfolio(R=R, portfolio=init.portf, optimize_method="random", search_size=n_portfolios),
          rcpp=rp_optimize_v2(R=R, portfolio=init.portf, search_size=n_portfolios),
          replications=1)[,1:4]

rp <- rcpp_random_portfolios(portfolio=init.portf, method="sample", n_portfolios=5000)

pa_opt=optimize.portfolio(R=R, portfolio=init.portf, optimize_method="random", rp=rp, trace=TRUE)
names(pa_opt)
xtract <- extractStats(pa_opt)
head(xtract)

rcpp_opt=rp_optimize_v2(R=R, portfolio=init.portf, rp=rp)
names(rcpp_opt)
head(rcpp_opt$rp_results)

all.equal(xtract, rcpp_opt$rp_results, check.attributes=FALSE)

benchmark(pa=optimize.portfolio(R=R, portfolio=init.portf, optimize_method="random", rp=rp),
          rcpp=rp_optimize_v2(R=R, portfolio=init.portf, rp=rp),
          replications=10)[,1:4]
