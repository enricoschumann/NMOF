gbm <- function(npaths, timesteps, mean, sd, tau, S0) {
    if (missing(S0))
        s0 <- 0 else s0 <- log(S0)
    dt <- tau/timesteps
    ans <- numeric((timesteps + 1L) * npaths)
    dim(ans) <- c(timesteps + 1L, npaths)
    ans[-1L,] <- rnorm(timesteps * npaths,
                      mean = (mean - 0.5 * sd * sd)*dt,
                      sd = sqrt(dt) * sd)
    if (timesteps > 1L) {
        ans[1L,] <- s0
        for (j in seq_len(npaths))
            ans[ ,j] <- cumsum(ans[ ,j])
    } else
        ans <- ans + s0
    exp(ans)
}
gbb <- function(npaths, timesteps, S0, ST, sd, tau) {
        gbm <- t(gbm(npaths, timesteps, 0, sd, tau, S0 = S0))[ ,-1L]
    w <- seq_len(timesteps)/timesteps
    ans <- gbm - as.matrix((gbm[,timesteps]-S0)) %*% w
    rbind(S0, t(ans + as.matrix(rep(ST - S0, npaths)) %*% w), deparse.level=0)
}
mc <- function(paths, payoff, ...) {
    payoff(paths, ...)
}
