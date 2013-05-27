gBrownianMotion <- function(npaths, timesteps, mean, sd, tau, S0) {
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
gBrownianBridge <- function(npaths, timesteps, S0, ST, sd, tau) {
        gbm <- t(gBrownianMotion(npaths, timesteps, 0, sd, tau, S0 = S0))[ ,-1L]
    w <- seq_len(timesteps)/timesteps
    ans <- gbm - as.matrix((gbm[,timesteps]-S0)) %*% w
    rbind(S0, t(ans + as.matrix(rep(ST - S0, npaths)) %*% w), deparse.level=0)
}
MCpricing <- function(paths, payoff, ...) {
    ## ... goes into payoff

}
npaths <- 10000000
timesteps <- 1
sd <- 0.5
S0 <- 100
ST <- 100
tau <- 1
mean <- 0.01
sd <- 0.25

system.time(x <- gBrownianMotion(npaths, timesteps, mean, sd, tau, S0=100))




## x <- gBrownianBridge(npaths, timesteps, S0, ST, sd, tau)[seq(1, timesteps, by=timesteps %/% 5),]
## mean(apply(x[-1,]/x[-5,]-1, 2,sd))
## sqrt(tau/5)*sd
