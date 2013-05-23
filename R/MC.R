gBrownianMotion <- function(npaths, timesteps, mean, sd, tau, S0) {
    if (missing(S0))
        s0 <- 0 else s0 <- log(S0)
    dt <- tau/timesteps
    tmp <- s0 + rnorm(timesteps * npaths,
                      mean = (mean - 0.5 * sd * sd)*dt,
                      sd = sqrt(dt) * sd)
    dim(tmp) <- c(timesteps, npaths) 
    if (timesteps > 1L)
        for (j in seq_len(npaths))
            tmp[ ,j] <- cumsum(tmp[ ,j])
    exp(tmp)
}


gBrownianBridge <- function(npaths, timesteps, S0, ST, sd, tau) {

    
    
    
}
MCpricing <- function(paths, payoff, ...) {
    ## ... goes into payoff

}
