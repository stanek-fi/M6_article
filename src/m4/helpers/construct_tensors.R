construct_tensors <- function(ys, k, horizon = 1) {
    data_raw <- lapply(seq_along(ys), function(i) {
        y <- ys[[i]]
        temp <- construct_regressors(y = y, k = k, horizon = horizon)
        names(temp)
        temp$task <- matrix(i, nrow(temp$Y))
        return(temp)
    })

    train <- list(
        y = torch_tensor(do.call(rbind, lapply(data_raw, function(obj) obj$Y))),
        x = torch_tensor(do.call(rbind, lapply(data_raw, function(obj) obj$X))),
        task = torch_tensor(do.call(rbind, lapply(data_raw, function(obj) obj$task)))
    )

    list(
        train = train,
        test = NA,
        validation = NA
    )
}
