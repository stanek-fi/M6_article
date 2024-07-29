position_baseline <- function(returns_num, participants_num, num_shorted = 0, num_zeros = 0, scale = 1) {
    if (returns_num - num_zeros < max(0, num_shorted)) {
        stop("invalid combination of inputs")
    }
    x <- do.call(rbind, lapply(seq_len(participants_num), function(r) {
        temp <- sample(rep(c(-1, 0, 1), c(num_shorted, num_zeros, returns_num - num_zeros - num_shorted)))
        temp / sum(abs(temp)) * scale
    }))
    return(x)
}
