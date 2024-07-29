#' MtMs_analytic
#'
#' This function performs (MtMs) for simple linear regression and L2 loss. The mesa and meta parameters are derived iteratively using analytical FOCs.
#'
#' @param y A numeric vector of response variables.
#' @param X A numeric matrix of predictor variables.
#' @param group A factor or character vector indicating the group membership of each observation.
#' @param S An integer specifying the number of mesa parameters. Default is 2 (i.e. one constant and one variable parameter).
#' @param constant A logical value indicating whether the first mesa parameter is constant across tasks. Default is TRUE.
#' @param lambda A numeric value or vector specifying the regularization parameter(s). Default is 0.
#' @param tol A numeric value specifying the tolerance for convergence. Default is 1e-5.
#' @param max_iterations An integer specifying the maximum number of iterations. Default is 100.
#' @param theta_sd A numeric value specifying the standard deviation for initializing Theta. Default is 1.
#' @param omega_sd A numeric value specifying the standard deviation for initializing Omega. Default is 0.
#'
#' @return A list containing the following components:
#' \describe{
#'   \item{Theta}{A matrix of estimated group-specific coefficients.}
#'   \item{Omega}{A matrix of estimated scale-specific coefficients.}
#'   \item{predict}{A function to make predictions based on the fitted model.}
#' }
#'
#' @examples
#' \dontrun{
#' y <- rnorm(100)
#' X <- matrix(rnorm(100 * 10), 100, 10)
#' group <- sample(letters[1:5], 100, replace = TRUE)
#' fit <- MtMs_analytic(y, X, group)
#' }
#'
#' @export

MtMs_analytic <- function(y, X, group, S = 2, constant = T, lambda = 0, tol = 1e-5, max_iterations = 100, theta_sd = 1, omega_sd = 0) {
    K <- dim(X)[2]
    group_names_all <- sort(unique(group))
    group_indices_all <- split(seq_along(group), group)
    group_names_all <- as.character(group_names_all)
    valid_groups <- sapply(group_indices_all, function(x) length(x) >= S)
    group_indices <- group_indices_all[valid_groups]
    group_names <- group_names_all[valid_groups]

    M <- length(group_indices)
    X_list <- lapply(1:M, function(m) X[group_indices[[m]], , drop = F])
    y_list <- lapply(1:M, function(m) y[group_indices[[m]], drop = F])
    XX_list <- lapply(X_list, function(x) t(x) %*% x)
    Xy_list <- lapply(seq_along(X_list), function(m) t(X_list[[m]]) %*% y_list[[m]])
    if (!(length(lambda) == 1 || length(lambda) == S)) {
        stop("length of lambda must be equal to either 1 or S")
    } else if (length(lambda) == 1) {
        lambdas <- rep(lambda, S)
    } else {
        lambdas <- lambda
    }

    Theta <- matrix(rnorm(M * S, 0, theta_sd), nrow = M, ncol = S)
    if (constant) {
        Theta[, 1] <- 1
    }
    Omega <- matrix(rnorm(K * S, 0, omega_sd), nrow = K, ncol = S)


    for (iter in seq_len(max_iterations)) {
        Theta_old <- Theta
        Omega_old <- Omega

        QQ <- matrix(0, nrow = K * S, ncol = K * S)
        for (s1 in seq_len(S)) {
            for (s2 in seq_len(s1)) {
                temp <- Reduce("+", lapply(seq_len(M), function(m) Theta[m, s1] * Theta[m, s2] * XX_list[[m]]))
                QQ[(s1 - 1) * K + 1:K, (s2 - 1) * K + 1:K] <- temp
                QQ[(s2 - 1) * K + 1:K, (s1 - 1) * K + 1:K] <- temp
            }
        }
        Qy <- do.call(rbind, lapply(seq_len(S), function(s) Reduce("+", lapply(seq_len(M), function(m) Theta[m, s] * Xy_list[[m]]))))
        vecOmega <- solve(QQ + diag(rep(lambdas * nrow(X), each = K), K * S, K * S)) %*% Qy
        Omega <- matrix(vecOmega, nrow = K, ncol = S)

        Theta <- do.call(rbind, lapply(seq_along(X_list), function(m) {
            if (!constant) {
                T <- X_list[[m]] %*% Omega
                theta <- solve(t(T) %*% T + diag(lambdas * nrow(X) / M, S, S)) %*% (t(T) %*% y_list[[m]])
            } else {
                T <- X_list[[m]] %*% Omega[, -1, drop = F]
                if (S > 1) {
                    theta <- rbind(1, solve(t(T) %*% T + diag(lambdas[-1] * nrow(X) / M, S - 1, S - 1)) %*% (t(T) %*% (y_list[[m]] - X_list[[m]] %*% Omega[, 1, drop = F])))
                } else {
                    theta <- matrix(1)
                }
            }
            t(theta)
        }))

        Theta_change <- mean(abs(Theta - Theta_old))
        Omega_change <- mean(abs(Omega - Omega_old))
        if (Theta_change < tol & Omega_change < tol) {
            break()
        }
    }

    if (iter == max_iterations) {
        warning("maximum number of iterations reached")
    }

    rownames(Omega) <- colnames(X)
    colnames(Omega) <- paste0("s", seq_len(S))
    Theta_all <- matrix(apply(Theta, 2, function(x) mean(x)), nrow = length(group_names_all), ncol = S, dimnames = list(group_names_all, paste0("s", seq_len(S))), byrow = T)
    Theta_all[group_names, ] <- Theta
    Theta <- Theta_all

    predict <- function(fit, X, group, horizon = 1) {
        Omega <- fit$Omega
        Theta <- fit$Theta
        group_names <- rownames(Theta)
        K <- nrow(Omega)
        if (K != ncol(X)) {
            stop("incompatible number of columns of X")
        }
        if (!all(group %in% group_names)) {
            stop("one or more unseen group")
        }

        group_names <- sort(unique(group))
        group_indices <- split(seq_along(group), group)
        group_names <- as.character(group_names)

        Xf <- X
        for (hi in 1:max(horizon)) {
            X <- Xf[, 1:K, drop = F]
            y_hat <- matrix(NA, nrow(X), 1)
            for (m in seq_along(group_indices)) {
                Xsub <- X[group_indices[[m]], , drop = F]
                beta <- (Omega %*% t(Theta[group_names[m], , drop = F]))
                y_hat[group_indices[[m]]] <- Xsub %*% beta
            }
            Xf <- cbind(y_hat, Xf)
        }
        Xf[, (max(horizon) + 1 - rev(horizon)), drop = F]
    }

    return(
        list(
            Theta = Theta,
            Omega = Omega,
            predict = predict
        )
    )
}
