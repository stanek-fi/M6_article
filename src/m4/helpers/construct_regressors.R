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

# construct_regressors <- function(y, k, horizon=1, test=0, validation=0, drop_NA = "all"){
#   obs <- length(y) - (k + max(horizon) - 1)
#   if(obs>0){
#     nva <- (validation - max(horizon) + 1)
#     if(nva>obs | nva<0){
#       stop("not enough observation for validation")
#     }
#     ntr <- min(max(ceiling((obs - nva) / 2), (obs - nva - test)), obs - nva)
#     nte <- obs - nva - ntr
#     if(obs != (nva + ntr + nte)){
#       stop("faulty split allocation")
#     }
#     split <- c(
#       rep("train",ntr),
#       rep("test",nte),
#       rep("validation",nva)
#     )
#     observed_one_step <- c(
#       rep(T, obs - (validation - max(horizon) + 1)),
#       rep(F, (validation - max(horizon) + 1))
#     )

#     is <- rev(c(1:k,(k+horizon)) - (k + max(horizon)))
#     temp <- do.call(cbind,lapply(is,function(i)
#       y[(length(y) - (obs - 1) + i):(length(y) + i)]
#     ))
#     Y <- temp[,1:length(horizon),drop = F]
#     X <- temp[,-(1:length(horizon)),drop = F]

#     for(i in 1:obs){
#       if(split[i]=="train"){
#         temp <- i - 1 + k + horizon - (length(y) - nte - validation)
#         Y[i,rev(temp>0)]=NA
#       }else if (split[i]=="test"){
#         temp <- i - 1 + k + horizon - (length(y) - validation)
#         Y[i,rev(temp>0)]=NA
#       }
#     }

#     if(drop_NA == "any"){
#       validrows <- !apply(is.na(Y), 1, function(x) any(x))
#     }else if (drop_NA == "all"){
#       validrows <- !apply(is.na(Y), 1, function(x) all(x))
#     }
#     split <- split[validrows]
#     Y <- Y[validrows,,drop=F]
#     X <- X[validrows,,drop=F]
#     observed_one_step <- observed_one_step[validrows]
#   }else{
#     split <- NULL
#     Y <- NULL
#     X <- NULL
#     observed_one_step <- NULL
#   }

#   out <- list(
#     split = split,
#     Y = Y,
#     X = X,
#     observed_one_step = observed_one_step
#   )
#   return(out)
# }

# y <- 1:20
# k <- 2
# horizon <- 1
# test <- 4
# validation <- 4
# drop_NA <- "all"
# construct_tensors(y,k,horizon,test,validation,drop_NA)
