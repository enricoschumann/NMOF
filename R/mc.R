gbm <- function(npaths, timesteps, r, v, tau, S0, exp.result = TRUE,
                antithetic = FALSE) {
    s0 <-  if (missing(S0))
               0
           else
               log(S0)
    dt <- tau/timesteps
    ans <- numeric((timesteps + 1L) * npaths)
    dim(ans) <- c(timesteps + 1L, npaths)
    if (!antithetic) {
        ans[-1L, ] <- rnorm(timesteps * npaths,
                            mean = (r - 0.5 * v)*dt,
                            sd = sqrt(dt * v))
    } else {
        tmp <- rnorm(timesteps * npaths/2,
                     mean = 0,
                     sd = sqrt(dt * v))
        ans[-1L, ] <- cbind(tmp, -tmp) + (r - 0.5 * v)*dt
    }
    if (timesteps > 1L) {
        ans[1L, ] <- s0
        for (j in seq_len(npaths))
            ans[ ,j] <- cumsum(ans[ ,j])
    } else
        ans <- ans + s0
    if (exp.result)
        exp(ans) else ans
}

gbb <- function(npaths, timesteps, S0, ST, v, tau, log = FALSE,
                exp.result = TRUE) {
    gbm <- t(gbm(npaths, timesteps, r = 0, v = v, tau = tau,
                 S0 = S0, exp = !log))[ , -1L, drop = FALSE]
    w <- seq_len(timesteps)/timesteps
    if (log) {
        ST <- log(ST)
        S0 <- log(S0)
    }
    ans <- gbm + as.matrix(-gbm[, timesteps] + ST) %*% w
    ans <- t(cbind(S0, ans))

    if (log && exp.result)
        exp(ans)
    else
        ans
}

mc <- function(paths, payoff, ...)
    payoff(paths, ...)
