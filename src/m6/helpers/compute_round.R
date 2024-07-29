compute_round <- function(returns, positions, states = NULL) {
    if (is.null(states)) {
        states <- rep(0, nrow(positions))
    }
    ret <- log(1 + returns %*% t(positions))
    sret <- apply(ret, 2, sum)
    sdp <- apply(ret, 2, sd)
    states <- states + sret / sdp
    return(states)
}
