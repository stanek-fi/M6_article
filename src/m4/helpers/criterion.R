criterion <- function(y_pred, y, tensor=T) {
    if(tensor) {
        mean(torch_nansum(contrast_function(y_pred, y), 2) / y$size(2))
    }else{
        rowSums(contrast_function(y_pred, y), na.rm = T) / ncol(y_pred)
    }
}
