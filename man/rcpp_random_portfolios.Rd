\name{rcpp_random_portfolios}
\alias{rcpp_random_portfolios}
\title{Create random portfolios}
\usage{
  rcpp_random_portfolios(portfolio,
    method = c("sample", "unif"), n_portfolios = 10,
    max_permutations = 200)
}
\arguments{
  \item{portfolio}{portfolio object created with
  PortfolioAnalytics::portfolio.spec}

  \item{method}{method to generate random portfolios}

  \item{n_portfolios}{maximum number of portfolios to
  generate}

  \item{max_permutations}{maximum number of permutations to
  attempt}
}
\value{
  a matrix of random portfolios
}
\description{
  Create random portfolios using the "sample" or "unif"
  method using algorithms implemented in Rcpp
}
\author{
  Ross Bennett
}

