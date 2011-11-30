## CF PRICING
test.callCF <- function() {

    ## taken from the PM package
    vanillaOptionEuropean <- function (S, K, tau, r, q = 0, v,
                                       tauD = 0, D = 0, type = "call",
                                       greeks = TRUE) {
        if (q != 0 & any(D != 0))
            stop("dividend rate and dividend amount supplied")
        if (any(D != 0))
            stopifnot(length(D) == length(tauD))
        if (any(D != 0)) {
            delD <- tauD <= tau & tauD > 0
            D <- D[delD]
            tauD <- tauD[delD]
        }
        if (tau <= 0)
            stop("'tau' must be greater than 0")
        if (v <= 0)
            stop("'v' must be greater than 0")
        S <- S - sum(exp(-r * tauD) * D)
        exq <- exp(-q * tau)
        exr <- exp(-r * tau)
        Sexq <- S * exq
        Kexr <- K * exr
        d1 <- (log(S/K) + (r - q + v/2) * tau)/(sqrt(v * tau))
        d2 <- d1 - sqrt(v * tau)
        I <- switch(type, call = 1, put = -1)
        N1 <- pnorm(I * d1)
        N2 <- pnorm(I * d2)
        price <- I * (Sexq * N1 - Kexr * N2)
        if (greeks == TRUE) {
            delta <- I * exq * N1
            theta <- -Sexq * dnorm(d1) * sqrt(v)/(2 * sqrt(tau)) -
                I * (-q * Sexq * N1 + r * Kexr * N2)
            rho <- I * tau * Kexr * N2
            rhoDiv <- -I * tau * Sexq * N1
            n1 <- dnorm(I * d1)
            gamma <- exq * n1/(S * sqrt(v * tau))
            vega <- Sexq * n1 * sqrt(tau)
        }
        if (greeks == FALSE)
            price
        else list(price = price, delta = delta, gamma = gamma, theta = theta/365,
                  vega = vega/100, rho = rho/100, rhoDiv = rhoDiv/100)
    }


    ## HESTON
    S <- 100; X <- 100; tau <- 1; r <- 0.02; q <- 0.08;
    v0 <- 0.2^2; vT <- 0.2^2            ## variance, not volatility
    rho <- -0.3; k <- 0.2; sigma <- 0.3 ## stoch. vol: Heston/Bates
    lambda <- 0.1; muJ <- -0.2; vJ <- 0.1^2 ## jumps: Merton/Bates/Heston
    ##
    S <- 100; X <- 100; tau <- 1; r <- 0.02; q <- 0.08;
    v0 <- 0.2^2; vT <- 0.2^2            # variance, not volatility
    rho <- -0.3; k <- 0.2; sigma <- 0.3 # stoch. vol: Heston/Bates
    temp <- callCF(cf = cfHeston, S=S, X=X, tau=tau, r=r, q = q,
                   v0 = v0, vT = vT, rho = rho, k = k, sigma = sigma, implVol = FALSE)
    checkEquals(round(temp,3), 4.268)

    S <- 100; X <- 100; tau <- 1; r <- 0.00; q <- 0.08;
    v0 <- 0.2^2; vT <- 0.2^2            # variance, not volatility
    rho <- -0.3; k <- 0.2; sigma <- 0.3 # stoch. vol: Heston/Bates
    temp <- callCF(cf = cfHeston, S=S, X=X, tau=tau, r=r, q = q,
                   v0 = v0, vT = vT, rho = rho, k = k, sigma = sigma, implVol = FALSE)
    checkEquals(round(temp,3), 3.621)

    S <- 100; X <- 100; tau <- 1; r <- 0.05; q <- 0.00;
    v0 <- 0.2^2; vT <- 0.2^2            ## variance, not volatility
    rho <- -0.3; k <- 0.2; sigma <- 0.3 ## stoch. vol: Heston/Bates
    temp <- callCF(cf = cfHeston, S=S, X=X, tau=tau, r=r, q = q,
                   v0 = v0, vT = vT, rho = rho, k = k, sigma = sigma, implVol = FALSE)
    checkEquals(round(temp,3), 10.055)

    S <- 100; X <- 90; tau <- 0.1; r <- 0.05; q <- 0.00;
    v0 <- 0.2^2; vT <- 0.2^2            # variance, not volatility
    rho <- -0.3; k <- 0.2; sigma <- 0.3 # stoch. vol: Heston/Bates
    temp <- callCF(cf = cfHeston, S=S, X=X, tau=tau, r=r, q = q,
                   v0 = v0, vT = vT, rho = rho, k = k, sigma = sigma, implVol = FALSE)
    checkEquals(round(temp,3), 10.586)


    ## BSM
    S <- 100; X <- 100; tau <- 1; r <- 0.02; q <- 0.08; v <- 0.2^2
    temp1 <- callCF(cf = cfBSM, S = S, X = X, tau = tau, r = r, q = q,
                    v = v, implVol = FALSE)
    temp2 <- vanillaOptionEuropean(S=S, K=X, tau=tau, r=r, q=q, v=v, greeks = FALSE)
    checkEquals(round(temp1,4), round(temp2,4))

    S <- 100; X <- 100; tau <- 1; r <- 0.00; q <- 0.08; v <- 0.2^2
    temp1 <- callCF(cf = cfBSM, S = S, X = X, tau = tau, r = r, q = q,
                    v = v, implVol = FALSE)
    temp2 <- vanillaOptionEuropean(S=S, K=X, tau=tau, r=r, q=q, v=v, greeks = FALSE)
    checkEquals(round(temp1,4), round(temp2,4))

    S <- 100; X <- 100; tau <- 1; r <- 0.02; q <- 0.00; v <- 0.2^2
    temp1 <- callCF(cf = cfBSM, S = S, X = X, tau = tau, r = r, q = q,
                    v = v, implVol = FALSE)
    temp2 <- vanillaOptionEuropean(S=S, K=X, tau=tau, r=r, q=q, v=v, greeks = FALSE)
    checkEquals(round(temp1,4), round(temp2,4))

    S <- 100; X <- 100; tau <- 1; r <- 0.00; q <- 0.00; v <- 0.2^2
    temp1 <- callCF(cf = cfBSM, S = S, X = X, tau = tau, r = r, q = q,
                    v = v, implVol = FALSE)
    temp2 <- vanillaOptionEuropean(S=S, K=X, tau=tau, r=r, q=q, v=v, greeks = FALSE)
    checkEquals(round(temp1,4), round(temp2,4))

    S <- 100; X <- 90; tau <- 1/12; r <- 0.00; q <- 0.00; v <- 0.2^2
    temp1 <- callCF(cf = cfBSM, S = S, X = X, tau = tau, r = r, q = q,
                    v = v, implVol = FALSE)
    temp2 <- vanillaOptionEuropean(S=S, K=X, tau=tau, r=r, q=q, v=v, greeks = FALSE)
    checkEquals(round(temp1,4), round(temp2,4))

    S <- 100; X <- 90; tau <- 1/12; r <- 0.00; q <- 0.00; v <- 0.2^2
    temp1 <- callCF(cf = cfBSM, S = S, X = X, tau = tau, r = r, q = q,
                    v = v, implVol = TRUE)
    checkEquals(round(temp1$impliedVol^2,4), v)


                                        # BATES

    temp <- callCF(cf = cfBates, S = S, X = X, tau = tau,
                   r = r, q = q,
                   v0 = v0, vT = vT, rho = rho, k = k, sigma = sigma,
                   lambda = lambda, muJ = muJ, vJ = vJ, implVol = FALSE)


                                        # MERTON

    temp <- callCF(cf = cfMerton, S = S, X = X, tau = tau,
                   r = r, q = q,
                   v = v, lambda = lambda, muJ = muJ, vJ = vJ, implVol = FALSE)

    S <- 100; X <- 100; tau <- 1; r <- 0.1; q <- 0.0
    nu <- 0.2; theta <- -0.14; sigma <- 0.12


                                        # VARIANCE GAMMA

    temp <- callCF(cf = cfVG, S = S, X = X, tau = tau,
                   r = r, q = q,
                   nu = nu, theta=theta, sigma = sigma, implVol = FALSE)
    checkEquals(round(temp,2), 11.37)

}
