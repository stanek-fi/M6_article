construct_regressors <- function(y, k, horizon=1){
  obs <- length(y) - (k + max(horizon) - 1)
  if(obs>0){
    is <- rev(c(1:k,(k+horizon)) - (k + max(horizon)))
    temp <- do.call(cbind,lapply(is,function(i)
      y[(length(y) - (obs - 1) + i):(length(y) + i)]
    ))
    Y <- temp[,1:length(horizon),drop = F]
    X <- temp[,-(1:length(horizon)),drop = F]
  }else{
    Y <- NULL
    X <- NULL
  }

  out <- list(
    Y = Y,
    X = X
  )
  return(out)
}
