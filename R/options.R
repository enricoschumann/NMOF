vanillaOptionEuropean <- function(S, X, tau, r, q = 0, v,
                                  tauD = 0, D = 0, type = "call",
                                  greeks = TRUE, model = NULL, ...) {

    tmp <- due(D, tauD, tau, q)
    tauD <- tmp$tauD
    D <- tmp$D

    if (any(tau <= 0))
        stop(sQuote("tau"), " must be a positive number")

    I <- switch(type, "call" = 1, "put" = -1)
    S <- S - sum(exp(-r*tauD)*D)  ## forward
    exq <- exp(-q * tau)
    exr <- exp(-r * tau)
    Sexq <- S * exq
    Xexr <- X * exr

    if (is.null(model) || tolower(model) == "bsm") {
        dots <- list(...)
        if (missing(v) && !is.null(vol <- dots[["vol"]]))
            v <- vol^2
        if (any(v <= 0))
            stop(sQuote("v"), " must be greater than 0")
        d1 <- (log(S/X) + (r - q + v / 2) * tau)/(sqrt(v * tau))
        d2 <- d1 - sqrt(v * tau)
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
            DvegaDspot <- vega/S * (1 - d1/sqrt(v * tau))
            DvegaDvol <- vega * d1 * d2 / sqrt(v)
        }
        if (!greeks)
            value else list(value = value, delta = delta, gamma = gamma,
                            theta = theta, vega = vega,
                            rho = rho, rhoDiv = rhoDiv,
                            DvegaDspot = DvegaDspot,
                            DvegaDvol = DvegaDvol)
    } else if (tolower(model) == "heston") {
        cf <- cfHeston
        P1 <- function(om, S, X, tau, r, q, ...)
            Re(exp(-(0+1i) * log(X) * om) *
               cf(om - (0+1i), S, tau, r, q, ...)/
               ((0+1i) * om * S * exp((r - q) * tau)))
        P2 <- function(om, S, X, tau, r, q, ...)
            Re(exp(-(0+1i) * log(X) * om) *
               cf(om, S, tau, r, q, ...)/((0+1i) * om))
        vP1 <- 0.5 + 1/pi * integrate(P1, lower = 1e-08, upper = Inf,
                                      S, X, tau, r, q, ...)$value
        vP2 <- 0.5 + 1/pi * integrate(P2, lower = 1e-08, upper = Inf,
                                      S, X, tau, r, q, ...)$value
        value <- Sexq * vP1 - Xexr * vP2
        if (I < 0)
            value <- putCallParity(what = "put", call = value,
                                   put = NULL, S, X, tau, r, q, tauD, D)
        if (!greeks)
            value else list(value = value,
                            delta = exq * vP1 - exq *(I < 0))
    }
}

vanillaOptionAmerican <- function(S, X, tau, r, q, v,
                                  tauD = 0, D = 0, type = "call",
                                  greeks = TRUE, M = 101) {

    tmp <- due(D, tauD, tau, q)
    tauD <- tmp$tauD
    D <- tmp$D

    if (any(tau <= 0))
        stop(sQuote("tau"), " must be a positive number")

    pmax2 <- function(y1, y2) (y1 + y2 + abs(y1 - y2)) / 2

    S <- S - sum(exp(-r*tauD)*D) ## forward
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
    W <- pmax2(m*(S * dM * uM - X), 0)

    if (greeks) {
        deltaE <- NA_real_
        thetaE <- NA_real_
        gammaE <- NA_real_
    }
    for (i in M:1) {
        t <- (i-1)*dt
        PV <- sum(D * (t < tauD) * exp(-r * (tauD - t)))
        Si <- S * dM[(M-i+2L):(M+1)] * uM[1:i]
        W <- pmax2(m*((Si + PV) - X), v1 * W[2:(i+1)] + v2 * W[1:i])
        W <- (W + abs(W))/2
        ## greeks
        if (greeks) {
            if (i == 2L) {
                deltaE <- (W[2L] - W[1L]) / (Si[2L] - Si[1L])
            } else if (i == 3L) {
                gammaE <- ((W[3L] - W[2L]) / (Si[3L] - Si[2L]) -
                           (W[2L] - W[1L]) / (Si[2L] - Si[1L])) /
                          (0.5*(Si[3L] - Si[1L]))
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
                                    M = 101L, uniroot.control = list(),
                                    uniroot.info = FALSE) {

    ucon <- list(interval = c(1e-05, 2),
                 tol = .Machine$double.eps^0.25,
                 maxiter = 1000L)
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
putCallParity <- function(what, call, put, S, X, tau, r, q = 0, tauD = 0, D = 0) {

    tmp <- due(D, tauD, tau, q)
    tauD <- tmp$tauD
    D <- tmp$D
    D <- sum(exp(-r*tauD)*D)

    if (any(tau <= 0))
        stop(sQuote("tau"), " must be a positive number")

    ## call + X * exp(-r*tau) = put + S * exp(-q * tau) - D

    switch(what,
           call = put + S * exp(-q * tau) - D - X * exp(-r*tau),
           put  = call + X * exp(-r*tau) - S * exp(-q * tau) + D,
           stop(sQuote(what), " is not supported (yet)"))
}
if (FALSE) {
    ## TODO compare with/move to NMOF manual
    ## test cases for implied vol
    impliedVol <- function(price, S, X, tau, r, q, vol0 = 0.15, I = 1,
                           tol = 1e-4, maxit = 200) {

        for (i in seq_len(maxit)) {
            tmp <- bsm(S, X, tau, r, q, vol0, I)
            step <- (tmp$value - price)/tmp$vega
            vol0 <- vol0 - step
            if (all(abs(step) < tol))
                break
        }
        vol0
    }
    bsm <- function(S, X, tau, r, q, vol, I = 1) {
        d1 <- (log(S/X) + (r - q + vol^2/2) * tau)/(vol * sqrt(tau))
        d2 <- d1 - vol * sqrt(tau)
        list(value = I * (S * exp(-q * tau) * pnorm(I * d1) -
                          X * exp(-r * tau) * pnorm(I * d2)),
             vega  = S * exp(-q*tau) * dnorm(d1 * I) * sqrt(tau))
    }
    cases <- 100000
    S <- sample(50:150, cases, replace = TRUE)
    X <- sample(50:150, cases, replace = TRUE)
    r <- sample(seq(0.0,0.1,by=0.001), cases, replace = TRUE)
    q <- sample(seq(0.0,0.1,by=0.001), cases, replace = TRUE)
    tau <- sample(seq(0.1,5,by=0.1), cases, replace = TRUE)
    vol <- sample(seq(0.2,0.7,by=0.01), cases, replace = TRUE)
    prices <- vanillaOptionEuropean(S,X,tau,r,q,v=vol^2, greeks=FALSE)

    v0 <- numeric(cases)
    for (i in seq_len(cases)) {
        tmp <- try(vanillaOptionImpliedVol("european", price = prices[i],
                                           S = S[i], X = X[i], tau = tau[i],
                                           r = r[i], q=q[i]))
        if (inherits(tmp, "try-error"))
            message("error in i ", i)
        else
            v0[i] <- tmp
    }

    ci <- 1:cases
    iv <- impliedVol(prices[ci],S[ci],X[ci],tau[ci],r[ci],q[ci],
                     vol0 = pmax(sqrt(abs(log(S[ci]/X[ci])+(r[ci]-q[ci])*tau[ci])*2/tau[ci]),0.1))
    ##data.frame(iv=iv, true=vol[ci], diff = round(iv-vol[ci],4))

    summary(abs(v0-vol))
    summary(abs(iv - vol[ci]))
    boxplot(list(abs(v0-vol), abs(iv - vol[ci])))
}


barrierOptionEuropean <- function(S, X, H, tau, r, q = 0, v,
                                  tauD = 0, D = 0, type = "call",
                                  barrier.type = "downin",
                                  rebate = 0,
                                  greeks = FALSE, model = NULL, ...) {

    if (D != 0)
        stop("not supported: use q as an approximation")

    if (type == "call") {
        if (barrier.type %in% c("downin", "downout")) {
            nu  <- 1
            phi <- 1
        } else if (barrier.type %in% c("upin", "upout")) {
            nu <- -1
            phi <- 1
        }
    } else if (type == "put") {
        if (barrier.type %in% c("downin", "downout")) {
            nu <- 1
            phi <- -1
        } else if (barrier.type %in% c("upin", "upout")) {
            nu <- -1
            phi <- -1
        }
    } else
        stop("unknown ", sQuote(type))

    K <- rebate
    mu <- (r - q - v/2)/v
    lambda <- sqrt(mu*mu + 2*r/v)
    z <- log(H/S)/sqrt(v*tau) + lambda*sqrt(v*tau)

    x1 <- log(S/X)/(sqrt(v*tau)) + (1+mu)*sqrt(v*tau)
    x2 <- log(S/H)/(sqrt(v*tau)) + (1+mu)*sqrt(v*tau)

    y1 <- log(H^2/(S*X))/sqrt(v*tau) + (1+mu)*sqrt(v*tau)
    y2 <- log(H/S)/sqrt(v*tau) + (1+mu)*sqrt(v*tau)

    A <- phi*S*exp(-q*tau) * pnorm(phi*x1) - phi*X*exp(-r*tau)*pnorm(phi*x1 - phi*sqrt(v*tau))
    B <- phi*S*exp(-q*tau) * pnorm(phi*x2) - phi*X*exp(-r*tau)*pnorm(phi*x2 - phi*sqrt(v*tau))

    C <- phi*S*exp(-q*tau)*(H/S)^(2*(mu+1))*pnorm(nu*y1) - phi*X*exp(-r*tau)*(H/S)^(2*mu)*pnorm(nu*y1 - nu*sqrt(v*tau))
    D <- phi*S*exp(-q*tau)*(H/S)^(2*(mu+1))*pnorm(nu*y2) - phi*X*exp(-r*tau)*(H/S)^(2*mu)*pnorm(nu*y2 - nu*sqrt(v*tau))

    E <- K*exp(-r*tau)*(pnorm(nu*x2 - nu*sqrt(v*tau)) - (H/S)^(2*mu)*pnorm(nu*y2 - nu*sqrt(v*tau)))
    F <- K*((H/S)^(mu + lambda)*pnorm(nu*z) + (H/S)^(mu - lambda)*pnorm(nu*z - 2*nu*lambda*sqrt(v*tau)))

    if (type == "call") {
        if (barrier.type == "downin") {
            if (X > H)
                value <- C + E
            else
                value <- A - B + D +E
        } else if (barrier.type == "upin") {
            if (X > H)
                value <- A + E
            else
                value <- B - C + D +E
        } else if (barrier.type == "downout") {
            if (X > H)
                value <- A - C + F
            else
                value <- B - D + F
        } else if (barrier.type == "upout") {
            if (X > H)
                value <- F
            else
                value <- A - B + C - D + F
        }
    } else if (type == "put") {
        if (barrier.type == "downin") {
            if (X > H)
                value <- B  - C +D+E
            else
                value <- A+E
        } else if (barrier.type == "upin") {
            if (X > H)
                value <- A - B +D+E
            else
                value <- C+E
        } else if (barrier.type == "downout") {
            if (X > H)
                value <- A-B+C-D+F
            else
                value <- F

        } else if (barrier.type == "upout") {
            if (X > H)
                value <- B - D +F
            else
                value <- A -C +F
        }
    }
    if (!greeks)
        value else
                  list(value = value, delta = NA)
}
