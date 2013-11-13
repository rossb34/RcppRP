
#' Create random portfolio
#' 
#' This function creates a random portfolio using the "sample" method
#' 
#' @param init_weights initial set of portfolio weights
#' @param weight_seq sequence of weights to use for creating a random portfolio
#' @param min_sum minimum sum of weights
#' @param max_sum maximum sum of weights
#' @param min_box minimum weights for box constraints
#' @param max_box maximum weights for box constraints
#' @param max_permutations maximum number of permutations to attempt
#' @return a vector of portfolio weights
#' @author Ross Bennett
#' @export
rp_sample <- function(init_weights, 
                      weight_seq,
                      min_sum,
                      max_sum,
                      min_box, 
                      max_box,
                      max_permutations
){
  nassets <- length(init_weights)
  
  if(length(min_box) != nassets | length(max_box) != nassets){
    stop("length of box constraints vectors must be equal to number of assets")
  }
  
  rcpp_rp_sample(init_weights=init_weights, 
                 weight_seq=weight_seq, 
                 min_sum=min_sum, 
                 max_sum=max_sum, 
                 min_box=min_box, 
                 max_box=max_box, 
                 max_permutations=max_permutations)
}

#' Create random portfolio
#' 
#' This function creates a random portfolio using the "unif" method. The "unif"
#' method draws weights from a uniform distribution.
#' 
#' @param init_weights initial set of portfolio weights
#' @param min_sum minimum sum of weights
#' @param max_sum maximum sum of weights
#' @param min_box minimum weights for box constraints
#' @param max_box maximum weights for box constraints
#' @param max_permutations maximum number of permutations to attempt
#' @return a vector of portfolio weights
#' @author Ross Bennett
#' @export
rp_unif <- function(init_weights, 
                    min_sum,
                    max_sum,
                    min_box, 
                    max_box,
                    max_permutations
){
  nassets <- length(init_weights)
  
  if(length(min_box) != nassets | length(max_box) != nassets){
    stop("length of box constraints vectors must be equal to number of assets")
  }
  
  rcpp_rp_unif(init_weights=init_weights, 
               min_sum=min_sum, 
               max_sum=max_sum, 
               min_box=min_box, 
               max_box=max_box, 
               max_permutations=max_permutations)
}

#' Create random portfolios
#' 
#' Create random portfolios using the "sample" or "unif" method using
#' algorithms implemented in Rcpp
#' 
#' @param portfolio portfolio object created with PortfolioAnalytics::portfolio.spec
#' @param method method to generate random portfolios
#' @param n_portfolios maximum number of portfolios to generate
#' @param max_permutations maximum number of permutations to attempt
#' @return a matrix of random portfolios
#' @author Ross Bennett
#' @export
rcpp_random_portfolios <- function(portfolio, method=c("sample", "unif"), n_portfolios=10, max_permutations=200){
  # check for a portfolio objective from PortfolioAnalytics
  if(!inherits(x=portfolio, "portfolio")) stop("portfolio object passed in must be of class 'portfolio'")
  
  method <- method[1]
  
  # Get the constraints from the portfolio object
  stopifnot("package:PortfolioAnalytics" %in% search()  ||  require("PortfolioAnalytics", quietly = TRUE))
  constraints <- PortfolioAnalytics:::get_constraints(portfolio)
  min_sum <- constraints$min_sum
  max_sum <- constraints$max_sum
  min_box <- constraints$min
  max_box <- constraints$max
  
  # Get the initial weights from the portfolio object
  init_weights <- portfolio$assets
  weight_seq <- portfolio$weight_seq
  if(is.null(weight_seq)){
    if(method == "sample"){
      message("weight_seq not detected, generating weight_seq")
      weight_seq <- seq(from=min(min_box), to=max(max_box), by=0.002)
    }
  }
  
  if(method == "sample"){
    out <- rcpp_random_portfolios_sample(n_portfolios=n_portfolios,
                                         init_weights=init_weights,
                                         weight_seq=weight_seq,
                                         min_sum=min_sum,
                                         max_sum=max_sum,
                                         min_box=min_box,
                                         max_box=max_box, 
                                         max_permutations=max_permutations
    )
  } else if(method == "unif"){
    out <- rcpp_random_portfolios_unif(n_portfolios=n_portfolios,
                                       init_weights=init_weights,
                                       min_sum=min_sum,
                                       max_sum=max_sum,
                                       min_box=min_box,
                                       max_box=max_box, 
                                       max_permutations=max_permutations
    )
  }
  rownames(out) <- names(init_weights)
  return(t(out))
}