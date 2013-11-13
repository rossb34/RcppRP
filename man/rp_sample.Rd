\name{rp_sample}
\alias{rp_sample}
\title{Create random portfolio}
\usage{
  rp_sample(init_weights, weight_seq, min_sum, max_sum,
    min_box, max_box, max_permutations)
}
\arguments{
  \item{init_weights}{initial set of portfolio weights}

  \item{weight_seq}{sequence of weights to use for creating
  a random portfolio}

  \item{min_sum}{minimum sum of weights}

  \item{max_sum}{maximum sum of weights}

  \item{min_box}{minimum weights for box constraints}

  \item{max_box}{maximum weights for box constraints}

  \item{max_permutations}{maximum number of permutations to
  attempt}
}
\value{
  a vector of portfolio weights
}
\description{
  This function creates a random portfolio using the
  "sample" method
}
\author{
  Ross Bennett
}

