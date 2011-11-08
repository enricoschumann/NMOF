callCF <- function(cf, S, X, tau, r, q = 0, tauD = 0, D = 0, ..., 
    implVol = FALSE) {
    
    ## TODO: correct tauD, D
    
    P1 <- function(om, S, X, tau, r, q, ...) {
        p <- Re(exp(-1i * log(X) * om) * cf(om - 1i, S, tau, r, q, ...) / 
                (1i * om * S * exp((r-q) * tau)))
        p
    }
    P2 <- function(om,S,X,tau,r,q, ...) {
        p <- Re(exp(-1i * log(X) * om) * cf(om, S, tau, r, q, ...) / 
                (1i * om))
        p
    }
    vP1 <- 0.5 + 1/pi * integrate(P1, lower = 0, upper = 200,
        S, X, tau, r, q, ...)$value
    vP2 <- 0.5 + 1/pi * integrate(P2, lower = 0, upper = 200,
        S, X, tau, r, q, ...)$value
    result <- exp(-q * tau) * S * vP1 - exp(-r * tau) * X * vP2;
    
    # implied BSM vol
    if (implVol) {
        diffPrice <- function(vol, call, S, X, tau, r, q) {
            d1 <- (log(S/X) + (r - q + vol^2/2)*tau)/(vol*sqrt(tau))
            d2 <- d1 - vol*sqrt(tau)
            callBSM <- S * exp(-q * tau) * pnorm(d1) - 
                X * exp(-r * tau) * pnorm(d2)
            call - callBSM
        }
        impliedVol <- uniroot(diffPrice, interval = c(0,2), 
            call = result, S = S, X = X, 
            tau = tau, r = r, q = q)[[1]]
        result <- list(callPrice = result, impliedVol = impliedVol)
    }
    result
}

cfHeston <- function(om, S, tau, r, q, v0, vT, rho, k, sigma) {
    d <- sqrt((rho * sigma * 1i * om - k)^2 + sigma^2 * 
            (1i * om + om ^ 2))
    g <- (k - rho * sigma * 1i * om - d) / 
        (k - rho * sigma * 1i * om + d)
    cf1 <- 1i * om * (log(S) + (r - q) * tau)
    cf2 <- vT*k/(sigma^2)*((k - rho * sigma * 1i * om - d) * 
            tau - 2 * log((1 - g * exp(-d * tau)) / (1 - g)))
    cf3 <- v0 / sigma^2 * (k - rho * sigma * 1i * om - d) * 
        (1 - exp(-d * tau)) / (1 - g * exp(-d * tau))
    cf  <- exp(cf1 + cf2 + cf3)
    cf
}

cfBSM <- function(om, S, tau, r, q, v) {
    exp(1i * om * log(S) + 1i * tau * (r - q) * om - 
            0.5 * tau * v * (1i * om + om ^ 2))
}

cfBates <- function(om, S, tau, r, q, 
    v0, vT, rho, k, sigma, lambda, muJ, vJ) {
    d <- sqrt( (rho*sigma*1i*om - k)^2 + sigma^2 * (1i*om + om^2) )
    g <- (k - rho*sigma*1i*om - d) / (k - rho*sigma*1i*om + d)
    cf1 <- 1i*om * (log(S) + (r - q) * tau)
    cf2 <- vT*k / (sigma^2) * ((k - rho*sigma*1i*om - d) * tau - 
            2 * log((1 - g * exp(-d * tau)) / (1 - g)))
    cf3 <- v0/sigma^2*(k-rho*sigma*1i*om-d)*(1-exp(-d*tau)) / 
        (1-g*exp(-d*tau))
    cf4 <- -lambda*muJ*1i*tau*om + lambda*tau* 
        ((1+muJ)^(1i*om) * exp( vJ*(1i*om/2) * (1i*om-1) )-1)
    cf  <- exp(cf1 + cf2 + cf3 + cf4)
    cf
}

cfMerton <- function(om, S, tau, r, q, v, lambda, muJ, vJ) {
    cf1 <- 1i*om*log(S) + 1i*om*tau*(r-q-0.5*v-lambda*muJ) - 
        0.5*(om^2)*v*tau
    cf2 <- lambda*tau*(exp(1i*om*log(1+muJ) - 
                    0.5*1i*om*vJ-0.5*vJ*om^2) - 1)
    cf <- exp(cf1 + cf2)
    cf
}

cfVG <- function(om, S, tau, r, q, nu, theta, sigma) {
    om1i <- om*1i; sigma2 <- sigma^2
    w <- log(1 - theta*nu - 0.5*nu*sigma2)/nu
    temp <- om1i*log(S) + om1i*(r-q+w)*tau
    temp <- exp(temp)
    cf <- temp / ((1 - om1i*theta*nu + 0.5*sigma2*nu*om^2)^(tau/nu))
    cf
}
