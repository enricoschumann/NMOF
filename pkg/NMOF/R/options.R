vanillaOptionEuropean <- function(S, X, tau, r, q = 0, v,
                                  tauD = 0, D = 0, type = "call",
                                  greeks = TRUE, model = NULL, ...) {
    if (any(q != 0) && any(D != 0))
        stop("dividend rate and dividend amount supplied")
    if (any(D != 0))
        stopifnot(length(D) == length(tauD))
    if (any(D != 0)) {
        D2keep <-  tauD <= tau & tauD > 0
        D <- D[D2keep]
        tauD <- tauD[D2keep]
    }
    if (any(tau <= 0))
        stop(sQuote("tau"), " must be greater than 0")
    if (any(v <= 0))
        stop(sQuote("v"), " must be greater than 0")

    if (is.null(model) || model == "bsm") {
        S <- S - sum(exp(-r*tauD)*D)
        exq <- exp(-q * tau)
        exr <- exp(-r * tau)
        Sexq <- S * exq
        Xexr <- X * exr
        d1 <- (log(S/X) + (r - q + v / 2) * tau)/(sqrt(v * tau))
        d2 <- d1 - sqrt(v * tau)
        I <- switch(type, "call" = 1, "put" = -1)
        N1 <- pnorm(I * d1)
        N2 <- pnorm(I * d2)
        value <- I*(Sexq * N1 - Xexr * N2)        
        if (greeks) {
            delta <- I * exq * N1
            theta <-  - Sexq*dnorm(d1)*sqrt(v)/ (2*sqrt(tau)) -
                I*(-q*Sexq*N1 + r*Xexr*N2)
            rho <- I * tau * Xexr * N2
            rhoDiv <- - I * tau * Sexq * N1
            n1 <- dnorm(I*d1)
            gamma <- exq * n1 / (S * sqrt(v*tau))
            vega <- Sexq * n1 * sqrt(tau)
        }
        if (!greeks)
            value else list(value = value, delta = delta, gamma = gamma,
                            theta = theta, vega = vega,
                            rho = rho, rhoDiv = rhoDiv)
    }
}
vanillaOptionAmerican <- function(S, X, tau, r, q, v,
                                  tauD = 0, D = 0, type = "call",
                                  greeks = TRUE, M = 101) {
    if (any(q != 0) && any(D != 0))
        stop("dividend rate and dividend amount supplied")
    if(any(D != 0))
        stopifnot(length(D) == length(tauD))
    if(any(D != 0)) {
        D2keep <-  tauD <= tau & tauD > 0
        D <- D[D2keep]
        tauD <- tauD[D2keep]
    }
    if (tau < 0)
        stop(sQuote("tau"), " must be > 0")
    pmax2 <- function(y1,y2)
        ((y1 + y2) + abs(y1 - y2)) / 2

    S <- S - sum(exp(-r*tauD)*D)
    dt <- tau/M
    u <- exp(sqrt(v*dt))
    d <- 1/u
    p <- (exp((r-q)*dt)-d)/(u-d)
    dM <- d^(M:0)
    uM <- u^(0:M)
    v1 <- p*exp(-r*dt)
    v2 <- (1-p)*exp(-r*dt)
    if (type == "call")
        m <- 1 else m <- -1
    W <- pmax2(m*(S * dM * uM - X),0)
    for (i in M:1) {
        t <- (i-1)*dt
        PV <- sum(D * (t < tauD) * exp(-r * (tauD - t)))
        Si <- S * dM[(M-i+2):(M+1)] * uM[1:i]
        W <- pmax2(m*((Si + PV) - X), v1 * W[2:(i+1)] + v2 * W[1:i])
        W <- (W + abs(W))/2
        ## greeks
        if (greeks) {
            if (i == 2L) {
                deltaE <- (W[2L] - W[1L]) / (Si[2L] - Si[1L])
            } else if (i == 3L) {
                gammaE <- ((W[3L] - W[2L]) / (Si[3L] - Si[2L]) -
                           (W[2L] - W[1L]) / (Si[2L] - Si[1L])) /
                               (0.5*(Si[3] - Si[1L]))
                thetaE <- W[2L]
            } else if (i == 1L)
                thetaE <- (thetaE - W[1L]) / (2 * dt)
        } ## end greeks
    }
    ##theta2E <- r * W[1] - r * Si[1] * deltaE - 0.5 * v * Si[1]^2 * gammaE
    if (!greeks)
        W else list(value = W, delta = deltaE, gamma = gammaE,
                    theta = thetaE, vega = NA,
                    rho = NA, rhoDiv = NA)
}
vanillaOptionImpliedVol <- function(exercise = "european",
                                    price, S, X, tau, r, q = 0,
                                    tauD = 0, D = 0, type = "call",
                                    M = 101, uniroot.control = list(),
                                    uniroot.info = FALSE) {

    ucon <- list(interval = c(1e-05, 2), tol = .Machine$double.eps^0.25,
                 maxiter = 1000)
    ucon[names(uniroot.control)] <- uniroot.control
    
    createF <- function() {
        if (exercise == "european") {
            f <- function(vv) {
                price - vanillaOptionEuropean(S = S, X = X,
                                              tau = tau, r = r, q = q, v = vv^2,
                                              tauD = tauD, D = D,
                                              type = type, greeks = FALSE)
            }
        } else {
            f <- function(vv) {
                price - vanillaOptionAmerican(S = S, X = X,
                                              tau = tau, r = r, q = q, v = vv^2,
                                              tauD = tauD, D = D,
                                              type = type, greeks = FALSE, M = M)
            }
        }
        f
    }
    f <- createF()
    res <- uniroot(f, interval = ucon$interval,
                   tol = ucon$tol, maxiter = ucon$maxiter)
    if (!uniroot.info)
        res <- res$root
    res
}
## putCallParity <- function(call, put, S, X, tau, r, q = 0, tauD = 0, D = 0,
##                           value = NULL) {
##     if (missing(call) && !missing(put) && !missing(S) && && !missing(X)) {
        

##     }
    


## }
