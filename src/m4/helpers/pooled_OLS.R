pooled_OLS <- function(xs, k, num_groups = 1, clustering = "random", ...) {
  if(clustering == "random" || num_groups == 1) {
      group <- sample(seq_len(num_groups), length(xs), replace = T)
  }else if (clustering == "sequential") {
      group <- rep(1:num_groups, each = ceiling(length(xs) / num_groups))[seq_along(xs)]
  }else if (clustering == "default_tsclust") {
      temp <- tsclust(series = xs, k = num_groups,...)
      group <- temp@cluster
  }else if (clustering == "PG2015"){
      temp <- tsclust(series = xs, k = num_groups, type = "partitional", preproc = zscore, distance = "sbd", centroid = "shape", ...)
      group <- temp@cluster
  }else if (clustering == "tsfeatures") {
      features <- tsfeatures(xs, ...)
      clusters <- kmeans(features, num_groups, iter.max = 1000)
      group <- clusters$cluster
  }
  group_indices <- split(seq_along(group), group)

  betas <- lapply(group_indices, function(indices) {
    temp <- lapply(xs[indices], function(y) {
      construct_regressors(y, k)
    })
    XOLS <- do.call(rbind, lapply(temp, function(obj) obj$X))
    yOLS <- do.call(rbind, lapply(temp, function(obj) obj$Y))
    solve(t(XOLS) %*% XOLS) %*% t(XOLS) %*% yOLS
  })

  predict <- function(fit, X, horizon = 1) {
    betas <- fit$betas
    group_indices <- fit$group_indices
    K <- ncol(X)
    Xf <- X

    for (hi in 1:max(horizon)) {
      X <- Xf[, 1:K, drop = F]
      y_hat <- matrix(NA, nrow(X), 1)
      for (m in seq_along(group_indices)) {
        Xsub <- X[group_indices[[m]], , drop = F]
        y_hat[group_indices[[m]]] <- Xsub %*% betas[[m]]
      }
      Xf <- cbind(y_hat, Xf)
    }
    Xf[, (max(horizon) + 1 - rev(horizon)), drop = F]
  }

  list(
    group_indices = group_indices,
    betas = betas,
    predict = predict
  )
}