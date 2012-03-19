## -*- truncate-lines: t; -*-

require("NMOF")

## optimisation functions, MA, testFunctions, option pricing

## MA
test.MA <- function() {
    x <- rnorm(100L); myMA <- numeric(length(x)); order <- 5L
    for (i in order:length(x))
        myMA[i] <- sum(x[(i - order + 1):i])/order

    checkEquals(MA(x,order = order)[-(1:(order-1))],
                myMA[-(1:(order-1))])

    x <- rnorm(100L); myMA <- numeric(length(x)); order <- 1L
    checkEquals(x, MA(x, order = order))
    checkEquals(x, MA(x, order = order, pad = NA))
}


## HESTON
test.callHestoncf <- function() {
    S <- 100; X <- 100; tau <- 1; r <- 0.02; q <- 0.01; v0 <- 0.2^2; vT <- 0.2^2
    rho <- -0.5; k <- 0.5; sigma <- 0.5
    result <- callHestoncf(S = S, X = X, tau = tau, r = r, q = q,
                           v0 = v0, vT = vT, rho = rho, k = k, sigma = sigma)
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


## PSopt
test.PSopt <- function() {
    ## test function: PS should find minimum
    OF <- tfRosenbrock
    size <- 3L ## define dimension
    algo <- list(printBar = FALSE,
                 printDetail = FALSE,
                 nP = 50L, nG = 1000L,
                 c1 = 0.0, c2 = 1.5,
                 iner = 0.8, initV = 0.50, maxV = 50,
                 min = rep(-50, size),
                 max = rep( 50, size))

    sol <- PSopt(OF = OF, algo = algo)
    checkEquals(sol$OFvalue, 0)

    ## exception: wrong size of initP
    algo$initP <- array(0, dim = c(20L,20L))
    checkException(res <- PSopt(OF = OF, algo), silent = TRUE)
    algo$initP <- function()
        array(0, dim = c(5,20))
    checkException(res <- PSopt(OF = OF, algo), silent = TRUE)
    algo$initP <- NULL


    ## check if Fmat/xlist are returned
    ## ...if FALSE
    algo <- list(printBar = FALSE,
                 printDetail = FALSE,
                 nP = 50L, nG = 100L,
                 c1 = 0.0, c2 = 1.5,
                 iner = 0.8, initV = 0.50, maxV = 50,
                 min = rep(-50, size),
                 max = rep( 50, size),
                 storeF = FALSE,
                 storeSolutions = FALSE)
    sol <- PSopt(OF = OF, algo = algo)
    checkTrue(is.na(sol$Fmat))
    checkTrue(is.na(sol$xlist))
    checkEquals(length(sol$Fmat), 1L)
    checkEquals(length(sol$xlist), 1L)

    ## ...if TRUE
    algo$storeF <- TRUE
    algo$storeSolutions <- TRUE
    sol <- PSopt(OF = OF, algo = algo)
    checkEquals(names(sol$xlist), c("P","Pbest"))
    checkEquals(dim(sol$xlist[[c(1L, algo$nG)]]),
                c(length(algo$min),algo$nP))
    checkEquals(dim(sol$xlist[[c(2L, algo$nG)]]),
                c(length(algo$min),algo$nP))
    ## xlist has only two elements
    checkException(sol$xlist[[c(3L, algo$nG)]], silent = TRUE)
    ## xlist[[i]] stores only algo$nG elements
    checkException(sol$xlist[[c(2L, algo$nG + 1L)]], silent = TRUE)
}


## TAopt
test.TAopt <- function() {

    ## TA should come close to the minimum
    size <- 5L
    x0 <- runif(size)
    xTRUE <- runif(size)
    data <- list(xTRUE = xTRUE,
                 step = 0.02)
    OF <- function(x, data)
        max(abs(x - data$xTRUE))
    neighbour <- function(x, data)
        x + runif(length(data$xTRUE))*data$step - data$step/2

    algo <- list(q = 0.05, nS = 1000L, nT = 15L,
                 neighbour = neighbour, x0 = x0,
                 printBar = FALSE,
                 printDetail = FALSE,
                 storeSolutions = TRUE,
                 storeF = TRUE)
    res <- TAopt(OF, algo = algo, data = data)
    checkTrue(res$OFvalue < 0.005)

    ## ---------
    ## check 'xlist'
    checkTrue(length(res$xlist[[1L]])==length(res$xlist[[2L]]))
    checkEquals(dim(do.call(rbind, res$xlist[[1L]])),
                dim(do.call(rbind, res$xlist[[2L]])))
    checkEquals(dim(res$Fmat), c(algo$nS * algo$nT, 2L))

    ## check 'Fmat': xn and xc must not all be the same
    checkTrue(!isTRUE(all.equal(res$Fmat[ ,1L],res$Fmat[ ,2L])))

    tmp <- res$Fmat[ ,1L]==res$Fmat[ ,2L]
    checkEquals(res$Fmat[tmp ,1L],
                apply(do.call(rbind, res$xlist[[1L]])[tmp, ], 1,OF, data))
    ## -------

    ## length(returned thresholds) == specified length(thresholds)
    checkTrue(length(res$vT) == algo$nT)

    ## specified thresholds are used
    algo$vT <- c(0.1,0.05,0)
    algo$nS <- 1000L
    res <- TAopt(OF, algo = algo, data = data)
    checkEqualsNumeric(res$vT,algo$vT)

    ## stepUp is used
    algo$stepUp <- 2L
    res <- TAopt(OF, algo = algo, data = data)
    checkEqualsNumeric(res$vT, rep(algo$vT, 3L))

    ## scale is used
    algo$stepUp <- 0L
    algo$scale <- 1.5
    res <- TAopt(OF, algo = algo, data = data)
    checkEqualsNumeric(res$vT, algo$scale*c(0.1,0.05,0))

    ## q is zero
    algo <- list(q = 0, nS = 100L, nT = 15L,
                 neighbour = neighbour, x0 = x0,
                 printBar = FALSE,
                 printDetail = FALSE)
    res <- TAopt(OF, algo = algo, data = data)
    checkEqualsNumeric(res$vT, numeric(algo$nT))
    checkEquals(length(res$vT), algo$nT)

    ## check printDetail
    algo <- list(q = 0, nS = 100L, nT = 5L,
                 neighbour = neighbour, x0 = x0,
                 printBar = TRUE,
                 printDetail = 50)
    res <- capture.output(ignore <- TAopt(OF, algo = algo, data = data))
    checkEquals(sum(grepl("Best solution", res)), 11L)
}


## LSopt
test.LSopt <- function() {
    xTRUE <- runif(5)
    data <- list(xTRUE = xTRUE, step = 0.02)
    OF <- function(x, data) max(abs(x - data$xTRUE))
    neighbour <- function(x, data)
        x + runif(length(data$xTRUE))*data$step - data$step/2

    x0 <- runif(5)
    algo <- list(nS = 10000L,
                 neighbour = neighbour, x0 = x0,
                 printBar = FALSE, printDetail = FALSE,
                 storeF = TRUE, storeSolutions = TRUE)
    res <- LSopt(OF, algo = algo, data = data)
    checkTrue(res$OFvalue < 0.005)

    ## ---------
    ## check 'xlist'
    checkTrue(length(res$xlist[[1L]])==length(res$xlist[[2L]]))
    checkEquals(dim(do.call(rbind, res$xlist[[1L]])),
                dim(do.call(rbind, res$xlist[[2L]])))
    checkEquals(dim(res$Fmat), c(algo$nS, 2L))

    ## check 'Fmat': xn and xc must not all be the same
    checkTrue(!isTRUE(all.equal(res$Fmat[ ,1L],res$Fmat[ ,2L])))

    tmp <- res$Fmat[ ,1L]==res$Fmat[ ,2L]
    checkEquals(res$Fmat[tmp ,1L],
                apply(do.call(rbind, res$xlist[[1L]])[tmp, ], 1,OF, data))
    ## -------

    ## check printDetail
    algo <- list(nS = 10000L,
                 neighbour = neighbour, x0 = x0,
                 printBar = FALSE, printDetail = 1000)
    res2 <- capture.output(res <- LSopt(OF, algo = algo, data = data))
    checkTrue(res$OFvalue < 0.005)
    checkEquals(sum(grepl("Best solution", res2)), 11L)
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
    xTRUE <- runif(5L)
    data <- list(xTRUE = xTRUE, step = 0.02)
    OF <- function(x, data)
        max(abs(x - data$xTRUE))

    neighbour <- function(x, data)
        x + runif(length(data$xTRUE))*data$step - data$step/2

    x0 <- runif(5L)
    algo <- list(q = 0.05, nS = 50L, nT = 5L,
                 neighbour = neighbour, x0 = x0,
                 printBar = FALSE, printDetail = FALSE)

    sols <- restartOpt(fun = TAopt, n = 10L,
                       OF = OF, algo = algo, data = data)
    checkEquals(length(sols), 10L)
}
