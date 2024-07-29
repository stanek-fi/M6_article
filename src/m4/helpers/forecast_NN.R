forecast_NN <- function(y, model, h, k, backPadding = 100, task = NULL){
  if(!is.null(task)){
    task <- torch_tensor(as.matrix(task))
  }
  y <- c(rep(0,backPadding),y)
  yforecast <- c(y,rep(NA,h))
  for(ih in 1:h){
    if(is.null(task)){
      yforecast[length(y) + ih] <- as.matrix(model(torch_tensor(t(yforecast[((ih - 1) + length(y):(length(y)-(k-1)))]))))
    }else{
      yforecast[length(y) + ih] <- as.matrix(model(torch_tensor(t(yforecast[((ih - 1) + length(y):(length(y)-(k-1)))])), task))
    }
  }
  yforecast[-(1:(length(y)))]
}
