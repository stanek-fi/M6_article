rm(list = ls())
library(yaml)
library(data.table)
library(stringr)
library(torch)
library(M4comp2018)
library(forecast)
library(doParallel)
library(tsfeatures)

par <- yaml.load_file(file.path("par.yaml"))
par <- setNames(lapply(par[["m4"]], function(x) x[[1]]), sapply(par[["m4"]], function(x) names(x)))

sourcePath <- file.path("src", "MtMs")
sapply(file.path(sourcePath, list.files(sourcePath)), source)
sourcePath <- file.path("src", "m4", "helpers")
sapply(file.path(sourcePath, list.files(sourcePath)), source)
temp_file_path <- file.path("models", "m4", "temp")
dir.create(temp_file_path, showWarnings = FALSE)

set.seed(par$seed)
torch_manual_seed(par$seed)

periods <- sapply(sapply(M4, "[", "period"), "[", 1)
selected_periods <- c("Yearly", "Quarterly", "Monthly", "Weekly")
# selected_periods <- c("Weekly")
max_ks <- sapply(selected_periods, function(x) min(sapply(which(periods == x), function(y) length(M4[[y]]$x))))
validations <- sapply(selected_periods, function(x) min(sapply(which(periods == x), function(y) length(M4[[y]]$xx))))

contrast_function <- function(y_pred, y) {
    abs(y_pred - y)
}

wrappers <- list(
    "OLS" = function(num_groups = 1, clustering = "random") {
        fit <- pooled_OLS(xs, k, num_groups = num_groups, clustering = clustering)
        X_hat <- do.call(rbind,lapply(xs, function(x) rev(as.vector(tail(x, k)))))
        xxs_hat <- fit$predict(fit, X_hat, horizon = 1:validation)
        xxs_hat <- lapply(seq_len(nrow(xxs_hat)), function(i) rev(xxs_hat[i,]))
        list(
            fit = fit,
            validation = mean(evaluate_OOS(xxs, xxs_hat, contrast_function))
        )
    },
    "local_ets" = function() {
        fit <- local_generic(xs, h = validation, fun = ets, parallel = T, libraries = "forecast", model = "ZZZ")
        list(
            validation = mean(evaluate_OOS(xxs, fit$xxs_hat, contrast_function))
        )
    },
    "local_auto_arima" = function() {
        fit <- local_generic(xs, h = validation, fun = auto.arima, parallel = T, libraries = "forecast", approximation = T, max.P = 1, max.Q = 1)
        list(
            validation = mean(evaluate_OOS(xxs, fit$xxs_hat, contrast_function))
        )
    },
    "MtMs" = function(init="MtMs_analytic", mesa_parameter_size = 1, minibatch = 10, meta_bias=T, meta_dropout_p=0, epochs = 10, lr= 0.001, weight_decay=0, patience = Inf) {
        data <- construct_tensors(xs, k, horizon = 1)
        if (init == "MtMs_analytic"){

            lambda <- c(0, rep(0.1, mesa_parameter_size))
            y <- as.matrix(data$train$y)
            X <- as.matrix(data$train$x)
            group <- as.vector(as.matrix(data$train$task))
            fit <- MtMs_analytic(
                y = y,
                X = X,
                group = group,
                S=mesa_parameter_size + 1, 
                constant = meta_bias, 
                lambda = lambda,
                max_iterations = 1000
            )
            Theta <- fit$Theta
            Omega <- fit$Omega

            if(!(meta_bias & (mesa_parameter_size == (ncol(Theta) - 1)) & all(Theta[,1] == 1))) {
                stop("invalid setup for MtMs_analytic init")
            }

            layer_transforms <- c(lapply(seq_len(length(1) - 1), function(x) nnf_relu), list(function(x) x))
            base_model <- FFNN(k, 1, layer_transforms, layer_biases = F)
            state <- base_model$state_dict()
            init_weights <- t(Omega[,1,drop=F])
            state[["layer_1.weight"]] <- torch_tensor(init_weights, requires_grad = T)
            base_model$load_state_dict(state)

            mesa_parameters_init = torch_tensor(t(Theta[,-1,drop=F]))
            meta_weight_init = torch_tensor(Omega[, -1, drop = F])
        }

        mtms <- MtMs(
            base_model = base_model,
            num_tasks = as.array(max(data$train$task)),
            mesa_parameter_size = mesa_parameter_size,
            meta_bias = meta_bias,
            meta_dropout_p = meta_dropout_p,
            mesa_parameters_init = mesa_parameters_init,
            meta_weight_init = meta_weight_init
        )
        
        fit <- train_model(
            model = mtms,
            criterion = criterion,
            train = data$train,
            test = NULL,
            epochs = epochs,
            minibatch = minibatch_generator(minibatch, task = data$train[[3]]),
            lr = lr,
            temp_file_path = temp_file_path,
            weight_decay = weight_decay,
            patience = patience
        )

        xxs_hat <- lapply(seq_along(xs), function(i) forecast_NN(xs[[i]], fit$model, h = validation, k, task = i))
        list(
            fit = lapply(fit$model$state_dict(), as.matrix),
            validation = mean(evaluate_OOS(xxs, xxs_hat, contrast_function))
        )
    }
)

combinations <- list(
    list(model = "local_ets"),
    list(model = "local_auto_arima"),
    list(model = "OLS"),
    list(model = "OLS", num_groups = 2, clustering = "tsfeatures"),
    list(model = "OLS", num_groups = 4, clustering = "tsfeatures"),
    list(model = "OLS", num_groups = 8, clustering = "tsfeatures"),
    list(model = "OLS", num_groups = 16, clustering = "tsfeatures"),
    list(model = "OLS", num_groups = 32, clustering = "tsfeatures"),
    list(model = "OLS", num_groups = 64, clustering = "tsfeatures"),
    list(model = "OLS", num_groups = 128, clustering = "tsfeatures"),
    list(model = "OLS", num_groups = 256, clustering = "tsfeatures"),
    list(model = "OLS", num_groups = 512, clustering = "tsfeatures"),
    list(model = "OLS", num_groups = 1024, clustering = "tsfeatures"),
    list(model = "MtMs", init = "MtMs_analytic", mesa_parameter_size = 2, epochs = par$epochs, minibatch = par$minibatch, lr = par$lr, patience = par$patience)
)

j <- 1
for(j in seq_along(selected_periods)) {
    
    selected_period <- selected_periods[j]
    k <- max_ks[j] - 1
    validation <- validations[j]
    # horizon <- 1
    ids <- which(periods == selected_period)
    ids <- ids[seq_len(min(par$series_included, length(ids)))]

    ys <- lapply(ids, function(id) {
        yObs <- M4[[id]]$x
        yOut <- M4[[id]]$xx
        scale <- mase_scale(yObs)
        location <- 0
        yObs <- (yObs - location) / scale
        yOut <- (yOut - location) / scale
        list(
            x = yObs,
            xx = yOut
        )
    })
    xs <- lapply(ys, function(obj) obj$x)
    xxs <- lapply(ys, function(obj) obj$xx)

    names(combinations) <- make.unique(str_c(
        sapply(combinations, function(x) x[[1]]),
        suppressWarnings(sapply(combinations, function(x) str_c("(", str_c(str_c(names(x[-1]), rep("=", length(x[-1])), x[-1]), collapse = ", "), ")")))
    ))

    outputs <- sapply(names(combinations), function(x) NULL)

    for (i in seq_along(combinations)) {
        try({
            gc()
            set.seed(1)
            combination <- combinations[[i]]
            outputs[[i]] <- do.call(wrappers[[combination[[1]]]], args = combination[-1])
            print(str_c(names(combinations)[i], ", validation = ", round(mean(outputs[[i]]$validation), 4)))
            print(str_c("-----------------------------  ", Sys.time(), "  -----------------------------"))
        })
    }

    print(selected_period)
    print(t(t(sapply(outputs, function(x) x$validation))))

    path <- file.path("models", "m4")
    dir.create(path, showWarnings = FALSE)
    saveRDS(outputs, file = file.path(path, paste0(selected_period, ".rds")))
}