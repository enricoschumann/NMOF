if (FALSE) {
    require("NMOF")
    S <- 90
    X <- 100
    tau <- 1
    r <- 0.02; q <- 0.08
    v0 <- 0.2^2  ## variance, not volatility
    vT <- 0.2^2  ## variance, not volatility
    v <- vT
    rho <- -0.0; k <- .2
    sigma <- 0.3

    ## jump parameters (Merton and Bates)
    lambda <- 0.1
    muJ <- -0.2
    vJ <- 0.1^2

    ## get Heston price and BSM implied volatility
    callCF(cf = cfHeston, S=S, X=X, tau=tau, r=r, q = q,
           v0 = v0, vT = vT, rho = rho, k = k, sigma = sigma, implVol = FALSE)

    delta <- function (cf, S, X, tau, r, q = 0, ..., implVol = FALSE,
                       uniroot.control = list(), 
                       uniroot.info = FALSE) {
        ucon <- list(interval = c(1e-05, 2), tol = .Machine$double.eps^0.25, 
                     maxiter = 1000)
        ucon[names(uniroot.control)] <- uniroot.control
        P1 <- function(om, S, X, tau, r, q, ...)
            Re(exp(-(0+1i) * log(X) * om) *
               cf(om - (0+1i), S, tau, r, q, ...)/
               ((0+1i) * om * S * exp((r - q) * tau)))
        P2 <- function(om, S, X, tau, r, q, ...)
            Re(exp(-(0+1i) * log(X) * om) *
               cf(om, S, tau, r, q, ...)/((0+1i) * om))
        vP1 <- 0.5 + 1/pi * integrate(P1, lower = 1e-08, upper = 200, 
                                      S, X, tau, r, q, ...)$value
        vP2 <- 0.5 + 1/pi * integrate(P2, lower = 1e-08, upper = 200, 
                                      S, X, tau, r, q, ...)$value
        exp(-q * tau) * vP1
    }

    ro <- function (cf, S, X, tau, r, q = 0, ..., implVol = FALSE,
                    uniroot.control = list(), 
                    uniroot.info = FALSE) {
        ucon <- list(interval = c(1e-05, 2), tol = .Machine$double.eps^0.25, 
                     maxiter = 1000)
        ucon[names(uniroot.control)] <- uniroot.control
        P1 <- function(om, S, X, tau, r, q, ...)
            Re(exp(-(0+1i) * 
                   log(X) * om) * cf(om - (0+1i), S, tau, r, q, ...)/
               ((0+1i) * om * S * exp((r - q) * tau)))
        P2 <- function(om, S, X, tau, r, q, ...)
            Re(exp(-(0+1i) * 
                   log(X) * om) * cf(om, S, tau, r, q, ...)/((0+1i) * om))
        vP1 <- 0.5 + 1/pi * integrate(P1, lower = 1e-08, upper = 200, 
                                      S, X, tau, r, q, ...)$value
        vP2 <- 0.5 + 1/pi * integrate(P2, lower = 1e-08, upper = 200, 
                                      S, X, tau, r, q, ...)$value
        X * exp(-r*tau)*tau * vP2
    }

    for (X in 60:140)
        for (S in 60:140)
            for (tau in seq(0.1, 3, by = 0.25)) {
                D1 <- delta(cf = cfHeston, S=S, X=X, tau=tau, r=r, q = q,
                            v0 = v0, vT = vT, rho = rho, k = k,
                            sigma = sigma, implVol = FALSE)            
                D2 <- fd(cf = cfHeston, S=S, X=X, tau=tau, r=r, q = q,
                         v0 = v0, vT = vT, rho = rho, k = k,
                         sigma = sigma, implVol = FALSE)
                if (abs(D1-D2) > 1e-3)
                    stop("ERROR for S = ",S, " and X = ", X)
            }

    for (X in seq(60, 140, by = 5))
        for (S in seq(60, 140, by = 5))
            for (tau in seq(0.1, 3, by = 0.25)) {
                D1 <- ro(cf = cfHeston, S=S, X=X, tau=tau, r=r, q = q,
                         v0 = v0, vT = vT, rho = rho, k = k,
                         sigma = sigma, implVol = FALSE)            
                D2 <- fd(cf = cfHeston, S=S, X=X, tau=tau, r=r, q = q,
                         v0 = v0, vT = vT, rho = rho, k = k,
                         sigma = sigma, implVol = FALSE, what = "r")
                if (abs(D1-D2) > 1e-3 && D1/D2-1 > 1e-4)
                    stop("ERROR for S = ",S, " and X = ", X)
            }



    ## this code uses the functions from 'NMOF' to return values that are required
    ## to calculate the number of shares in the minimum variance portfolio

    require('NMOF') ## make sure NMOF is included; this makes
    ## available the function cfHeston which is the
    ## \Phi function in our empirical paper and Gilli
    P1integrand <- function(om,S,X,tau,r,q,v0,vT,rho,k,sigma) {
        i <- 1i
        p <- Re(exp(-i*log(X)*om) *
                cfHeston(om-i,S,tau,r,q,v0,vT,rho,k,sigma) /
                (i * om * S * exp((r-q) * tau)))
        return(p)
    }

    P2integrand <- function(om,S,X,tau,r,q,v0,vT,rho,k,sigma) {
        i <- 1i
        p <- Re(exp(-i*log(X)*om) *
                cfHeston(om,S,tau,r,q,v0,vT,rho,k,sigma) / (i * om))
        return(p)
    }

                                        # partial of exponential wrt v
    dexpdv <- function(om,S,tau,r,q,v0,vT,rho,k,sigma) {
        d <- sqrt((rho * sigma * 1i * om - k)^2 + sigma^2 * 
                  (1i * om + om ^ 2))
        g2 <- (k - rho * sigma * 1i * om - d) / 
            (k - rho * sigma * 1i * om + d)
        dcf3 <- 1 / sigma^2 * (k - rho * sigma * 1i * om - d) * 
            (1 - exp(-d * tau)) / (1 - g2 * exp(-d * tau))
        return(dcf3)
    }


    dP1integrand <- function(om,S,X,tau,r,q,v0,vT,rho,k,sigma) {
        i <- 1i
        dcf3dv = dexpdv(om,S,tau,r,q,v0,vT,rho,k,sigma) 
        p <- Re(exp(-i*log(X)*om) * 
                dcf3dv*cfHeston(om-i,S,tau,r,q,v0,vT,rho,k,sigma) / 
                (i * om * S * exp((r-q) * tau)))
        return(p)
    }

    dP2integrand <- function(om,S,X,tau,r,q,v0,vT,rho,k,sigma) {
        i <- 1i
        dcf3dv = dexpdv(om,S,tau,r,q,v0,vT,rho,k,sigma) 
        p <- Re(exp(-i*log(X)*om) * 
                dcf3dv*cfHeston(om,S,tau,r,q,v0,vT,rho,k,sigma) / (i * om))
        return(p)
    }

                                        # calculating number of shares minimum variance hedge ratio
                                        # N(t): number of units of underlying to be used in hedging with a call option
                                        # \Delta_s + (\rho \sigma \Delta_{v0})/S
                                        # \Delta_{v0}  = S* d P_1/d v - K*exp(-r*T)*dP_2/dv
                                        # taken from Bakshi1997 

    numsharesSV <- function(S,X,tau,r,q,v0,vT,rho,k,sigma){
        P1val = 0.5 + 1/pi * integrate(P1integrand,lower = 0,upper = 200,
        S,X,tau,r,q,v0,vT,rho,k,sigma)$value
        P2val = 0.5 + 1/pi * integrate(P2integrand,lower = 0,upper = 200,
        S,X,tau,r,q,v0,vT,rho,k,sigma)$value
                                        # calculate dP1dval
        dP1valdv = 1/pi * integrate(dP1integrand,lower = 0,upper = 200,
        S,X,tau,r,q,v0,vT,rho,k,sigma)$value
        dP2valdv = 1/pi * integrate(dP2integrand,lower = 0,upper = 200,
        S,X,tau,r,q,v0,vT,rho,k,sigma)$value

                                        # calculate dP2dval

                                        # calculate dcdv; requires dP1valdv and dP2valdv
        dcdv = S*dP1valdv - X*exp(-r*T)*dP2valdv # correct 
        dcds = P1val

        Nshares = dcds + (rho*sigma*dcdv)/S
        return(Nshares)
    }
}
