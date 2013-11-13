\name{set_moments}
\alias{set_moments}
\title{Set portfolio moments}
\usage{
  set_moments(R, momentargs = NULL, ...)
}
\arguments{
  \item{R}{an xts or matrix object of asset returns}
}
\value{
  a list with the first and second moments
}
\description{
  Set the portfolio moments so they only need to be
  calculated once
}

