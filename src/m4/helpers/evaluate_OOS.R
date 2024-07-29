evaluate_OOS <- function(xxs, xxs_hat, contrast_function){
  sapply(seq_along(xxs), function(n){
    mean(contrast_function(xxs[[n]], xxs_hat[[n]]))
  })
}
