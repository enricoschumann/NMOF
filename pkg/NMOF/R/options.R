vanillaOptionEuropean <- function(S, X, tau, r, q = 0, v,
    tauD = 0, D = 0, type = "call", greeks = TRUE) {
    ##type <- match.arg(tolower(type), c("call","put"))  ## too slow!
    if( q != 0 & any(D != 0) )
        stop("dividend rate and dividend amount supplied")
    if(any(D != 0))
        stopifnot(length(D) == length(tauD))
    if(any(D != 0)) {
        delD <-  tauD <= tau & tauD > 0
        D <- D[delD]
        tauD <- tauD[delD]
    }
    if (any(tau <= 0))
        stop("'tau' must be greater than 0")
    if (any(v <= 0))
        stop("'v' must be greater than 0")
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
    # price
    price <- I*(Sexq * N1 - Xexr * N2)
    if (greeks == TRUE){
        ## delta
        delta <- I * exq * N1
        ## theta
            theta <-  - Sexq*dnorm(d1)*sqrt(v)/ (2*sqrt(tau)) -
                I*(-q*Sexq*N1 + r*Xexr*N2)
        ## rho
        rho <- I * tau * Xexr * N2
        ## rhoDiv
        rhoDiv <- - I * tau * Sexq * N1
        ## gamma: same for call and put
        n1 <- dnorm(I*d1)
        gamma <- exq * n1 / (S * sqrt(v*tau))
        ## vega: same for call and put
        vega <- Sexq * n1 * sqrt(tau)
    }
    if (greeks == FALSE)
        price else list(price = price, delta = delta, gamma = gamma,
            theta = theta, vega = vega,
            rho = rho, rhoDiv = rhoDiv)

}
vanillaOptionAmerican <- function(S, X, tau, r, q, v,
    tauD = 0, D = 0, type = "call", greeks = TRUE, M = 101) {
    if( q != 0 & any(D != 0) )
        stop("dividend rate and dividend amount supplied")
    if(any(D != 0))
        stopifnot(length(D) == length(tauD))
    if(any(D != 0)) {
        delD <-  tauD <= tau & tauD > 0
        D <- D[delD]
        tauD <- tauD[delD]
    }
    if (tau < 0)
        stop("'tau' must be > 0")
    pmax2 <- function(y1,y2) ((y1 + y2) + abs(y1 - y2)) / 2

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
        # greeks
        if (greeks) {
            if (i == 2)
                deltaE <- (W[2] - W[1]) / (Si[2] - Si[1])
            if (i == 3) {
                # gamma
                gammaE <- ((W[3]-W[2]) / (Si[3]-Si[2]) -
                           (W[2]-W[1]) / (Si[2]-Si[1])) /
                               (0.5*(Si[3] - Si[1]))
                ## theta (prelim)
                thetaE <- W[2]
            }
            if (i == 1) thetaE <- (thetaE - W[1]) / (2 * dt)
        } # end greeks
    }
    ##theta2E <- r * W[1] - r * Si[1] * deltaE - 0.5 * v * Si[1]^2 * gammaE
    price <- W
    if (!greeks)
        price else list(price = W, delta = deltaE, gamma = gammaE,
                        theta = thetaE, vega = NA,
                        rho = NA, rhoDiv = NA)
}


vanillaOptionImpliedVol <- function(exercise = "european",
    price, S, X, tau, r, q = 0, tauD = 0, D = 0, type = "call",
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
                    tauD = tauD, D = D, type = type, greeks = FALSE)
            }
        } else {
            f <- function(vv) {
                price - vanillaOptionAmerican(S = S, X = X,
                    tau = tau, r = r, q = q, v = vv^2,
                    tauD = tauD, D = D, type = type, greeks = FALSE, M = M)
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
