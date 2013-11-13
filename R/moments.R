#' Set portfolio moments
#' 
#' Set the portfolio moments so they only need to be calculated once
#' 
#' @param R an xts or matrix object of asset returns
#' @return a list with the first and second moments
#' @export
set_moments <- function(R, momentargs=NULL, ...){
  if(is.null(momentargs)) momentargs <- list()
  if(is.null(momentargs$mu)) momentargs$mu = matrix( as.vector(apply(R, 2, 'mean', na.rm=TRUE)), ncol=1);
  if(is.null(momentargs$sigma)) momentargs$sigma = cov(R, use='pairwise.complete.obs')
  return(momentargs)
}
