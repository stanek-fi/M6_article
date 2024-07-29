local_generic <- function(xs, h, fun, parallel = F, libraries = NULL, ...){
  if(!parallel){
    lapply(xs, function(y) {
      model <- fun(y, ...)
      list(
        xxs_hat = as.vector(forecast(model, h)$mean)
      )
    })
  }else{
    numCores <- min(detectCores() - 1, 7)
    cl <- makeCluster(numCores)
    registerDoParallel(cl)
    for(lib in libraries){
        clusterExport(cl, c("lib"), envir=environment())
        clusterEvalQ(cl, library(lib, character.only=TRUE))
    }
    Ids <- seq_along(xs)
    IdsChunks <- split(Ids,ceiling(seq_along(Ids) / length(Ids) * min(100,ceiling(length(Ids)/numCores) )))
    res <- lapply(seq_along(IdsChunks), function(x) NULL)
    for(Chunk in seq_along(IdsChunks)){
        xsSubset <- xs[IdsChunks[[Chunk]]]
        clusterExport(cl, c("xsSubset"), envir=environment())

        res[[Chunk]] <- foreach(i = seq_along(IdsChunks[[Chunk]])) %dopar% {
        y <- xsSubset[[i]]
        model <- fun(y, ...)
        as.vector(forecast(model, h)$mean)
        }
        print(paste0("Period:",Chunk/length(IdsChunks) * 100,"% time:",Sys.time()))
    }
    stopCluster(cl)
    list(
      xxs_hat = do.call(c,res)
    )
  }
}
