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








# construct_tensors <- function(ys, k, horizon = 1, test = 0, validation = 0, drop_NA = "all") {
#     data_raw <- lapply(seq_along(ys), function(i) {
#         y <- ys[[i]]
#         temp <- construct_regressors(y = y, k = k, horizon = horizon, test = test, validation = validation, drop_NA = drop_NA)
#         names(temp)
#         temp$task <- matrix(i, nrow(temp$Y))
#         return(temp)
#     })

#     train <- list(
#         y = torch_tensor(do.call(rbind, lapply(data_raw, function(obj) obj$Y[obj$split == "train", , drop = F]))),
#         x = torch_tensor(do.call(rbind, lapply(data_raw, function(obj) obj$X[obj$split == "train", , drop = F]))),
#         task = torch_tensor(do.call(rbind, lapply(data_raw, function(obj) obj$task[obj$split == "train", , drop = F])))
#     )

#     test <- list(
#         y = torch_tensor(do.call(rbind, lapply(data_raw, function(obj) obj$Y[obj$split == "test", , drop = F]))),
#         x = torch_tensor(do.call(rbind, lapply(data_raw, function(obj) obj$X[obj$split == "test", , drop = F]))),
#         task = torch_tensor(do.call(rbind, lapply(data_raw, function(obj) obj$task[obj$split == "test", , drop = F])))
#     )

#     validation <- list(
#         y = torch_tensor(do.call(rbind, lapply(data_raw, function(obj) obj$Y[obj$split == "validation", , drop = F]))),
#         x = torch_tensor(do.call(rbind, lapply(data_raw, function(obj) obj$X[obj$split == "validation", , drop = F]))),
#         task = torch_tensor(do.call(rbind, lapply(data_raw, function(obj) obj$task[obj$split == "validation", , drop = F])))
#     )

#     list(
#         train = train,
#         test = test,
#         validation = validation
#     )
# }
