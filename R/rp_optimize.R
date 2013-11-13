
#' V1 Random portfolio optimization
#' 
#' TODO Random portfolio optimization description
#' 
#' @param R
#' @param portfolio
#' @param search_size
#' @param objective
#' @param rp
#' @param \dots
rp_optimize_v1 <- function(R, objective, search_size=2000, portfolio=NULL, rp=NULL, momentFUN="set_moments", ...){
  if(!inherits(x=portfolio, "portfolio")) stop("portfolio object passed in must be of class 'portfolio'")
  
  call <- match.call()
  
  # R <- checkData(R)
  N <- length(portfolio$assets)
  if (ncol(R) > N) {
    R <- R[, names(portfolio$assets)]
  }
  
  if(!is.null(rp)){
    # The user has passed in a matrix for rp, we do not need to generate
    if(dim(rp)[2] == ncol(R)){
      rp <- t(rp)
    } else if(dim(rp)[1] != ncol(R)){
      stop("rp must be an n x m matrix where n is equal to the number of assets 
           and m is equal to the number of portfolios to test.")
    }
  } else {
    if(hasArg(method)) method=match.call(expand.dots=TRUE)$method else method="sample"
    if(hasArg(max_permutations)) max_permutations=match.call(expand.dots=TRUE)$max_permutations else max_permutations=200
    rp <- rcpp_random_portfolios(portfolio=portfolio, method="sample", n_portfolios=search_size, max_permutations=max_permutations)
    rp <- t(rp)
  }
  rownames(rp) <- paste("w", names(portfolio$assets), sep=".")
  # print(rp)
  
  dotargs <- list(...)
  
  # set portfolio moments only once
  if(!is.function(momentFUN)){
    momentFUN <- match.fun(momentFUN)
  }
  # TODO FIXME should match formals later
  .mformals <- dotargs
  .formals <- formals(momentFUN)
  onames <- names(.formals)
  if (length(.mformals)) {
    dargs <- .mformals
    pm <- pmatch(names(dargs), onames, nomatch = 0L)
    names(dargs[pm > 0L]) <- onames[pm]
    .formals[pm] <- dargs[pm > 0L]
  }
  .formals$R <- R
  # .formals$portfolio <- portfolio
  .formals$... <- NULL
  
  # If no dotargs are passed in, .formals was a pairlist and do.call was failing
  if(!inherits(.formals, "list")) .formals <- as.list(.formals)
  
  mout <- try((do.call(momentFUN, .formals)) ,silent=TRUE)
  if(inherits(mout,"try-error")) { 
    stop(paste("portfolio moment function failed with message",mout))
  }
  # print(mout)
  
  valid_objectives = c("max_return", "min_variance", "min_sd", "max_qu", "max_sharpe")
  if(!(objective %in% valid_objectives)) stop("Invalid objective")
  if(objective == "max_qu"){
    if(!hasArg(risk_aversion)) risk_aversion=match.call(expand.dots=TRUE)$risk_aversion else risk_aversion=2
  }
  opt <- rcpp_rp_optimize(rp=rp, args_list=mout, objective=objective)
  colnames(opt) <- "out"
  rp_results <- cbind(opt, t(rp))
  # print(rp_results)
  rp_results[which.min(rp_results[,"out"]), ]
}

#' V2 Random portfolio optimization
#' 
#' TODO Random portfolio optimization description
#' 
#' @param R
#' @param portfolio
#' @param search_size
#' @param objective
#' @param rp
#' @param \dots
rp_optimize_v2 <- function(R, objective=NULL, portfolio=NULL, search_size=2000, rp=NULL, momentFUN="set_moments", ...){
  if(!inherits(x=portfolio, "portfolio")) stop("portfolio object passed in must be of class 'portfolio'")
  
  call <- match.call()
  
  # R <- checkData(R)
  N <- length(portfolio$assets)
  if (ncol(R) > N) {
    R <- R[, names(portfolio$assets)]
  }
  
  if(!is.null(rp)){
    # The user has passed in a matrix for rp, we do not need to generate
    if(dim(rp)[2] == ncol(R)){
      rp <- t(rp)
    } else if(dim(rp)[1] != ncol(R)){
      stop("rp must be an n x m matrix where n is equal to the number of assets 
           and m is equal to the number of portfolios to test.")
    }
    } else {
      if(hasArg(method)) method=match.call(expand.dots=TRUE)$method else method="sample"
      if(hasArg(max_permutations)) max_permutations=match.call(expand.dots=TRUE)$max_permutations else max_permutations=200
      rp <- rcpp_random_portfolios(portfolio=portfolio, method="sample", n_portfolios=search_size, max_permutations=max_permutations)
      rp <- t(rp)
  }
  rownames(rp) <- paste("w", names(portfolio$assets), sep=".")
  # print(rp)
  
  dotargs <- list(...)
  
  # set portfolio moments only once
  if(!is.function(momentFUN)){
    momentFUN <- match.fun(momentFUN)
  }
  # TODO FIXME should match formals later
  .mformals <- dotargs
  .formals <- formals(momentFUN)
  onames <- names(.formals)
  if (length(.mformals)) {
    dargs <- .mformals
    pm <- pmatch(names(dargs), onames, nomatch = 0L)
    names(dargs[pm > 0L]) <- onames[pm]
    .formals[pm] <- dargs[pm > 0L]
  }
  .formals$R <- R
  # .formals$portfolio <- portfolio
  .formals$... <- NULL
  
  # If no dotargs are passed in, .formals was a pairlist and do.call was failing
  if(!inherits(.formals, "list")) .formals <- as.list(.formals)
  
  mout <- try((do.call(momentFUN, .formals)) ,silent=TRUE)
  if(inherits(mout,"try-error")) { 
    stop(paste("portfolio moment function failed with message",mout))
  }
  # print(mout)
  
  valid_names = c("mean", "sd", "var")
  obj_names <- unlist(lapply(portfolio$objectives, function(x) x$name))
  if(!all(obj_names %in% valid_names)) stop("Invalid objective name")
  
  opt2 <- rcpp_rp_optimize_v2(rp=rp, objectives_list=portfolio$objectives, args_list=mout)
  rownames(opt2) <- c(obj_names, "out")
  rp_results <- t(rbind(opt2, rp))
  # print(rp_results)
  optimal <- rp_results[which.min(rp_results[,"out"]), ]
  list(rp_results=rp_results, optimal=optimal)
}
