## -*- truncate-lines: t; -*-

## - MA
## - option pricing
## - testFunctions
## - restartOpt

## MA
test.MA <- function() {
    x <- rnorm(100L); myMA <- numeric(length(x)); order <- 5L
    for (i in order:length(x))
        myMA[i] <- sum(x[(i - order + 1):i])/order

    checkEquals(MA(x, order = order)[-(1:(order-1))],
                myMA[-(1:(order-1))])

    x <- rnorm(100L); myMA <- numeric(length(x)); order <- 1L
    checkEquals(x, MA(x, order = order))
    checkEquals(x, MA(x, order = order, pad = NA))
}


## HESTON
test.callHestoncf <- function() {
    S <- 100; X <- 100; tau <- 1; r <- 0.02; q <- 0.01;
    v0 <- 0.2^2; vT <- 0.2^2
    rho <- -0.5; k <- 0.5; sigma <- 0.5
    result <- callHestoncf(S = S, X = X, tau = tau, r = r, q = q,
                           v0 = v0, vT = vT, rho = rho, k = k,
                           sigma = sigma)
    checkEquals(round(result[[1L]], 3), 7.119)

    S <- 100; X <- 100; tau <- 1; r <- 0.02; q <- 0.01; v0 <- 0.2^2; vT <- 0.2^2
    rho <- -0.5; k <- 0.5; sigma <- 0.01
    result <- callHestoncf(S = S, X = X, tau = tau, r = r, q = q,
                           v0 = v0, vT = vT, rho = rho, k = k, sigma = sigma)
    checkEquals(round(result[[1L]], 3), 8.347)

    S <- 100; X <- 90; tau <- 1; r <- 0.02; q <- 0.01; v0 <- 0.2^2; vT <- 0.2^2
    rho <- -0.5; k <- 0.5; sigma <- 1
    result <- callHestoncf(S = S, X = X, tau = tau, r = r, q = q,
                           v0 = v0, vT = vT, rho = rho, k = k, sigma = sigma)
    checkEquals(round(result[[1L]], 3), 13.362)
}



## EUROPEAN
test.EuropeanCall <- function() {
    S0 <- 10; X <- 10; r <- 0.02; tau <- 1; sigma <- 0.20; M = 101
    res <- EuropeanCall(S0 = S0, X = X, r = r, tau = tau, sigma = sigma, M = M)
    checkEquals(round(res,2), 0.89)

    S0 <- 10; X <- 6; r <- 0.02; tau <- 1; sigma <- 0.20; M = 101
    res <- EuropeanCall(S0 = S0, X = X, r = r, tau = tau, sigma = sigma, M = M)
    checkEquals(round(res,2), 4.12)

    S0 <- 10; X <- 10; r <- 0.00; tau <- 1; sigma <- 0.20; M = 101
    res <- EuropeanCall(S0 = S0, X = X, r = r, tau = tau, sigma = sigma, M = M)
    checkEquals(round(res,2), 0.80)

    S0 <- 10; X <- 10; r <- 0.02; tau <- 1/12; sigma <- 0.20; M = 101
    res <- EuropeanCall(S0 = S0, X = X, r = r, tau = tau, sigma = sigma, M = M)
    checkEquals(round(res,2), 0.24)

    S0 <- 10; X <- 10; r <- 0.02; tau <- 1/12; sigma <- 0.80; M = 101
    res <- EuropeanCall(S0 = S0, X = X, r = r, tau = tau, sigma = sigma, M = M)
    checkEquals(round(res,2), 0.93)

    S0 <- 10; X <- 10; r <- 0.02; tau <- 1/12; sigma <- 0.02; M = 101
    res <- EuropeanCall(S0 = S0, X = X, r = r, tau = tau, sigma = sigma, M = M)
    checkEquals(round(res,2), 0.03)

}
test.EuropeanCallBE <- function() {
    ## EuropeanCall and EuropeanCallBE should give the same results
    S0 <- 10; X <- 10; r <- 0.02; tau <- 1; sigma <- 0.20; M = 101
    res <- EuropeanCall(S0 = S0, X = X, r = r, tau = tau, sigma = sigma, M = M)
    res2 <- EuropeanCallBE(S0 = S0, X = X, r = r, tau = tau, sigma = sigma, M = M)
    checkEquals(res,res2)

    S0 <- 10; X <- 6; r <- 0.02; tau <- 1; sigma <- 0.20; M = 101
    res <- EuropeanCall(   S0 = S0, X = X, r = r, tau = tau, sigma = sigma, M = M)
    res2 <- EuropeanCallBE(S0 = S0, X = X, r = r, tau = tau, sigma = sigma, M = M)
    checkEquals(res,res2)

    S0 <- 10; X <- 10; r <- 0.00; tau <- 1; sigma <- 0.20; M = 101
    res <- EuropeanCall(S0 = S0, X = X, r = r, tau = tau, sigma = sigma, M = M)
    res2 <- EuropeanCallBE(S0 = S0, X = X, r = r, tau = tau, sigma = sigma, M = M)
    checkEquals(res,res2)

    S0 <- 10; X <- 10; r <- 0.02; tau <- 1/12; sigma <- 0.20; M = 101
    res <- EuropeanCall(S0 = S0, X = X,
                        r = r, tau = tau, sigma = sigma, M = M)
    res2 <- EuropeanCallBE(S0 = S0, X = X,
                           r = r, tau = tau, sigma = sigma, M = M)
    checkEquals(res,res2)

    S0 <- 10; X <- 10; r <- 0.02; tau <- 1/12; sigma <- 0.80; M = 101
    res <- EuropeanCall(S0 = S0, X = X,
                        r = r, tau = tau, sigma = sigma, M = M)
    res2 <- EuropeanCallBE(S0 = S0, X = X,
                           r = r, tau = tau, sigma = sigma, M = M)
    checkEquals(res,res2)

    S0 <- 10; X <- 10; r <- 0.02; tau <- 1/12; sigma <- 0.02; M = 101
    res <- EuropeanCall(S0 = S0, X = X,
                        r = r, tau = tau, sigma = sigma, M = M)
    res2 <- EuropeanCallBE(S0 = S0, X = X,
                           r = r, tau = tau, sigma = sigma, M = M)
    checkEquals(res,res2)
}

## TESTfunctions
test.testFunctions <- function() {
    x <- rep(0,10L)
    checkEqualsNumeric(tfAckley(x), 0)
    checkEqualsNumeric(tfGriewank(x), 0)
    checkEqualsNumeric(tfRastrigin(x), 0)

    x <- rep(1,10L)
    checkEqualsNumeric(tfRosenbrock(x), 0)

    x <- rep(420.9687, 10)
    checkEqualsNumeric(tfSchwefel(x), -418.9829*10,
                       tolerance = 1e-4)

    x <- c(-0.0244, 0.2106)
    checkEqualsNumeric(tfTrefethen(x), -3.306868,
                       tolerance = 1e-4)
}

## restartOpt
test.restartOpt <- function() {
    testParallel <- FALSE

    xTRUE <- runif(5L)
    data <- list(xTRUE = xTRUE, step = 0.02)
    OF <- function(x, data)
        max(abs(x - data$xTRUE))
    neighbour <- function(x, data)
        x + runif(length(data$xTRUE))*data$step - data$step/2

    x0 <- runif(5L)
    algo <- list(q = 0.05, nS = 5L, nT = 5L,
                 neighbour = neighbour, x0 = x0,
                 printBar = FALSE, printDetail = FALSE)

        sols <- restartOpt(fun = TAopt, n = 5L,
                           OF = OF, algo = algo, data = data)
    checkEquals(length(sols), 5L)

    ## tests for snow/multicore: slow!
    if (testParallel) {
        OF <- function(x, data) {
            Sys.sleep(1e-3)
            max(abs(x - data$xTRUE))
        }
        if (require("snow", quietly = TRUE)){
            system.time({
                sols <- restartOpt(fun = TAopt, n = 10L,
                                   OF = OF, algo = algo, data = data,
                                   method = "snow", cl = 2)
            })
            checkEquals(length(sols), 10L)

            ## up top version 0.23-1, an argument passed with '...'
            ## could not be called 'X': led to an error
            X <- list(xTRUE = runif(5L), step = 0.02)
            OF <- function(x, X)
                max(abs(x - X$xTRUE))
            neighbour <- function(x, X)
                x + runif(length(X$xTRUE))*X$step - X$step/2
            algo <- list(q = 0.05, nS = 10L, nT = 5L,
                         neighbour = neighbour, x0 = runif(5),
                         printBar = FALSE, printDetail = FALSE)
            sols <- restartOpt(fun = TAopt, n = 4L,
                               OF = OF, algo = algo, X = X)
            sols <- restartOpt(fun = TAopt, n = 4L,
                               OF = OF, algo = algo, X = X,
                               method = "snow", cl = 2L)
        }
        if (suppressWarnings(require("multicore", quietly = TRUE))) {
            ## up top version 0.23-1, an argument passed with '...'
            ## could not be called 'X': led to an error
            X <- list(xTRUE = runif(5L), step = 0.02)
            OF <- function(x, X)
                max(abs(x - X$xTRUE))
            neighbour <- function(x, X)
                x + runif(length(X$xTRUE))*X$step - X$step/2
            algo <- list(q = 0.05, nS = 10L, nT = 5L,
                         neighbour = neighbour, x0 = runif(5),
                         printBar = FALSE, printDetail = FALSE)
            sols <- restartOpt(fun = TAopt, n = 4L,
                               OF = OF, algo = algo, X = X,
                               method = "multicore")
        }
    }

}


## EUROPEAN BSM
test.vanillaOptionEuropean <- function() {
    # PRICES
    S <- 100; X <- 100; tau <- 1; r <- 0.02; q <- 0.00; vol <- 0.3
    x <- vanillaOptionEuropean(S, X, tau, r, q, vol^2, type = "call")$value
    checkEquals(round(x,3), 12.822)

    S <- 100; X <- 100; tau <- 1; r <- 0.00; q <- 0.00; vol <- 0.3
    x <- vanillaOptionEuropean(S, X, tau, r, q, vol^2, type = "call")$value
    y <- vanillaOptionEuropean(S, X, tau, r, q, vol^2, type = "put")$value
    checkEquals(x, y)

    S <- 100; X <- 95; tau <- 0.5; r <- 0.03; q <- 0.06; vol <- 0.1
    x <- vanillaOptionEuropean(S, X, tau, r, q, vol^2, type = "put")$value
    checkEquals(round(x,3), 1.305)

    S <- 30; X <- 32; tau <- 0.5; r <- 0.03; q <- 0.00; vol <- 0.2;
    tauD <- c(0.1,0.2,0.3); D <- c(1,2,3)
    x <- vanillaOptionEuropean(S, X, tau, r, q, vol^2,
        tauD = tauD, D = D, type = "put")$value
    checkEquals(round(x,3), 7.523)

    S <- 30; X <- 32; tau <- 0.5; r <- 0.03; q <- 0.00; vol <- 0.2;
    tauD <- c(0.1,0.2,0.3,1); D <- c(1,2,3,5)
    x <- vanillaOptionEuropean(S, X, tau, r, q, vol^2,
        tauD = tauD, D = D, type = "put")$value
    checkEquals(round(x,3), 7.523)

    # ERRORS IN INPUTS
    # ... q and D specified
    S <- 30; X <- 30; tau <- 0.5; r <- 0.03; q <- 0.06; vol <- 0.1;
    tauD <- c(0.1,0.2,0.3); D <- c(1,2,3)
    checkException(vanillaOptionEuropean(S, X, tau, r, q, vol^2,
            tauD = tauD, D = D, type = "put")$value, silent = TRUE)

    # GREEXS
    ## delta
    S <- 30; X <- 30; tau <- 0.5; r <- 0.03; q <- 0.06; vol <- 0.1;
    x <- vanillaOptionEuropean(S, X, tau, r, q, vol^2, type = "put")
    all.equal(round(x$delta,6), round(-0.55330738389122,6))

    # TODO -- add tests for greeks
}
##test.vanillaOptionEuropean()


## AMERICAN BSM
test.vanillaOptionAmerican <- function() {
    # PRICES
    S <- 100; X <- 100; tau <- 1; r <- 0.02; q <- 0.00; vol <- 0.3
    x <- vanillaOptionAmerican(S, X, tau, r, q, vol^2,
        type = "call", M = 101)$value
    checkEquals(round(x,3), 12.850)

    S <- 100; X <- 100; tau <- 1; r <- 0.00; q <- 0.00; vol <- 0.3
    x <- vanillaOptionAmerican(S, X, tau, r, q, vol^2,
        type = "call", M = 101)$value
    y <- vanillaOptionAmerican(S, X, tau, r, q, vol^2,
        type = "put", M = 101)$value
    checkEquals(x, y)

    S <- 100; X <- 95; tau <- 0.5; r <- 0.03; q <- 0.06; vol <- 0.1
    x <- vanillaOptionAmerican(S, X, tau, r, q, vol^2,
        type = "put", M = 101)$value
    checkEquals(round(x,3), 1.303)

    S <- 30; X <- 32; tau <- 0.5; r <- 0.03; q <- 0.00; vol <- 0.2;
    tauD <- c(0.1,0.2,0.3); D <- c(1,2,3)
    x <- vanillaOptionAmerican(S, X, tau, r, q, vol^2,
        tauD = tauD, D = D, type = "call")$value
    checkEquals(round(x,3), 0.153)

    S <- 30; X <- 32; tau <- 0.5; r <- 0.03; q <- 0.00; vol <- 0.2;
    tauD <- c(0.1,0.2,0.3,1); D <- c(1,2,3,5)
    x <- vanillaOptionAmerican(S, X, tau, r, q, vol^2,
        tauD = tauD, D = D, type = "call")$value
    checkEquals(round(x,3), 0.153)

    ## ERRORS IN INPUTS
    ## ... q and D specified
    S <- 30; X <- 30; tau <- 0.5; r <- 0.03; q <- 0.06; vol <- 0.1;
    tauD <- c(0.1,0.2,0.3); D <- c(1,2,3)
    checkException(vanillaOptionAmerican(S, X, tau, r, q, vol^2,
            tauD = tauD, D = D, type = "put")$value, silent = TRUE)

    # GREEKS
    # TODO...
}


test.vanillaOptionImpliedVol <- function() {
    # EUROPEAN
    S <- 100; X <- 100; tau <- 1; r <- 0.02; q <- 0.00; vol <- 0.3
    p <- vanillaOptionEuropean(S, X, tau, r, q, vol^2, type = "call")$value
    ivol <- vanillaOptionImpliedVol(exercise = "european", price = p,
        S = S, X = X, tau = tau, r = r, q = q, tauD = 0, D = 0,
        type = "call")
    checkEquals(round(ivol,4), vol)

    S <- 100; X <- 100; tau <- 1; r <- 0.00; q <- 0.00; vol <- 0.3
    p <- vanillaOptionEuropean(S, X, tau, r, q, vol^2, type = "call")$value
    ivol <- vanillaOptionImpliedVol(exercise = "european", price = p,
        S = S, X = X, tau = tau, r = r, q = q, tauD = 0, D = 0,
        type = "call")
    checkEquals(round(ivol,4), vol)

    S <- 100; X <- 100; tau <- 1; r <- 0.00; q <- 0.00; vol <- 0.3
    p <- vanillaOptionEuropean(S, X, tau, r, q, vol^2, type = "put")$value
    ivol <- vanillaOptionImpliedVol(exercise = "european", price = p,
        S = S, X = X, tau = tau, r = r, q = q, tauD = 0, D = 0,
        type = "put")
    checkEquals(round(ivol,4), vol)

    S <- 100; X <- 95; tau <- 0.5; r <- 0.03; q <- 0.06; vol <- 0.1
    p <- vanillaOptionEuropean(S, X, tau, r, q, vol^2, type = "put")$value
    ivol <- vanillaOptionImpliedVol(exercise = "european", price = p,
        S = S, X = X, tau = tau, r = r, q = q, tauD = 0, D = 0,
        type = "put")
    checkEquals(round(ivol,4), vol)

    S <- 30; X <- 32; tau <- 0.5; r <- 0.03; q <- 0.00; vol <- 0.2;
    tauD <- c(0.1,0.2,0.3); D <- c(1,2,3)
    p <- vanillaOptionEuropean(S, X, tau, r, q, vol^2,
        tauD = tauD, D = D, type = "put")$value
    ivol <- vanillaOptionImpliedVol(exercise = "european", price = p,
        S = S, X = X, tau = tau, r = r, q = q, tauD = tauD, D = D,
        type = "put")
    checkEquals(round(ivol,4), vol)

    S <- 30; X <- 32; tau <- 0.5; r <- 0.03; q <- 0.00; vol <- 0.2;
    tauD <- c(0.1,0.2,0.3,1); D <- c(1,2,3,5)
    p <- vanillaOptionEuropean(S, X, tau, r, q, vol^2,
        tauD = tauD, D = D, type = "put")$value
    ivol <- vanillaOptionImpliedVol(exercise = "european", price = p,
        S = S, X = X, tau = tau, r = r, q = q, tauD = tauD, D = D,
        type = "put")
    checkEquals(round(ivol,4), vol)

    # AMERICAN
    S <- 100; X <- 100; tau <- 1; r <- 0.02; q <- 0.00; vol <- 0.3
    p <- vanillaOptionAmerican(S, X, tau, r, q, vol^2,
        type = "call", M = 101)$value
    ivol <- vanillaOptionImpliedVol(exercise = "american", price = p,
        S = S, X = X, tau = tau, r = r, q = q, tauD = 0, D = 0,
        type = "call", uniroot.control = list(interval = c(0.01, 0.8)))
    checkEquals(round(ivol,4), vol)

    S <- 100; X <- 100; tau <- 1; r <- 0.00; q <- 0.00; vol <- 0.3
    p <- vanillaOptionAmerican(S, X, tau, r, q, vol^2,
        type = "call", M = 101)$value
    ivol <- vanillaOptionImpliedVol(exercise = "american", price = p,
        S = S, X = X, tau = tau, r = r, q = q, tauD = 0, D = 0,
        type = "call")
    checkEquals(round(ivol,4), vol)

    S <- 100; X <- 100; tau <- 1; r <- 0.00; q <- 0.00; vol <- 0.3
    p <- vanillaOptionAmerican(S, X, tau, r, q, vol^2,
        type = "put", M = 101)$value
    ivol <- vanillaOptionImpliedVol(exercise = "american", price = p,
        S = S, X = X, tau = tau, r = r, q = q, tauD = 0, D = 0,
        type = "put",uniroot.control = list(interval = c(0.01, 0.5)))
    checkEquals(round(ivol,4), vol)

    S <- 100; X <- 95; tau <- 0.5; r <- 0.03; q <- 0.06; vol <- 0.1
    p <- vanillaOptionAmerican(S, X, tau, r, q, vol^2,
        type = "put", M = 101)$value
    ivol <- vanillaOptionImpliedVol(exercise = "american", price = p,
        S = S, X = X, tau = tau, r = r, q = q, tauD = 0, D = 0,
        type = "put")
    checkEquals(round(ivol,4), vol)

    S <- 30; X <- 32; tau <- 0.5; r <- 0.03; q <- 0.00; vol <- 0.2;
    tauD <- c(0.1,0.2,0.3); D <- c(1,2,3)
    p <- vanillaOptionAmerican(S, X, tau, r, q, vol^2,
        tauD = tauD, D = D, type = "call")$value
    ivol <- vanillaOptionImpliedVol(exercise = "american", price = p,
        S = S, X = X, tau = tau, r = r, q = q, tauD = tauD, D = D,
        type = "call")
    checkEquals(round(ivol,4), vol)

    S <- 30; X <- 32; tau <- 0.5; r <- 0.03; q <- 0.00; vol <- 0.2;
    tauD <- c(0.1,0.2,0.3,1); D <- c(1,2,3,5)
    p <- vanillaOptionAmerican(S, X, tau, r, q, vol^2,
        tauD = tauD, D = D, type = "call")$value
    ivol <- vanillaOptionImpliedVol(exercise = "american", price = p,
        S = S, X = X, tau = tau, r = r, q = q, tauD = tauD, D = D,
        type = "call")
    checkEquals(round(ivol,4), vol)
}
