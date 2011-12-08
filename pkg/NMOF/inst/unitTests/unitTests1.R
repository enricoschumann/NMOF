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



## DE
test.DEopt <- function() {
    trefethen <- function(xx) {
        x <- xx[1L]; y <- xx[2L]
        exp(sin(50*x)) + sin(60*exp(y)) + sin(70*sin(x)) +
            sin(sin(80*y)) - sin(10*(x+y))  + (x^2+y^2)/4
    }

    algo <- list(nP = 100, nG = 300, F = 0.5, CR = 0.9,
                 min = c(-3, -3), max = c( 3, 3),
                 printBar = FALSE, printDetail = TRUE)

    ## DE should solve the problem
    sol <- DEopt(OF = trefethen, algo)
    format(sol$OFvalue, digits = 12)
    checkEquals(round(sol$OFvalue,2), -3.31)

    ## without max vector, DE should give error
    algo$max <- NULL
    checkException(res <- DEopt(OF = trefethen, algo), silent = TRUE)

    ## unused parameter 'z' should cause error
    algo$max <- c( 3,  3)
    checkException(res <- DEopt(OF = trefethen, algo, z = 2), silent = TRUE)

    ## 'z' is required but not provided >> error
    trefethen2 <- function(xx, z) {
        x <- xx[1]; y <- xx[2]
        res <- exp(sin(50*x)) + sin(60*exp(y)) + sin(70*sin(x)) +
            sin(sin(80*y)) - sin(10*(x+y))  + (x^2+y^2)/4
        res + z
    }
    checkException(res <- DEopt(OF = trefethen2, algo), silent = TRUE)

    ## 'z' is required and provided
    checkEquals(round(DEopt(OF = trefethen2, algo, z = 2)$OFvalue,2), -1.31)
    checkEquals(round(DEopt(OF = trefethen2, algo,     2)$OFvalue,2), -1.31)

    ## test function: DE should find minimum
    OF <- tfRosenbrock
    size <- 5L ## define dimension
    algo <- list(printBar = FALSE,
                 printDetail = FALSE,
                 nP = 100L, nG = 500L,
                 F = 0.5, CR = 0.9,
                 min = rep(-50, size),
                 max = rep( 50, size))

    sol <- DEopt(OF = OF, algo = algo)
    checkEquals(sol$OFvalue, 0)

    ## exception: wrong size of initP
    algo$initP <- array(0, dim = c(20,20))
    checkException(res <- DEopt(OF = OF, algo), silent = TRUE)
    algo$initP <- function() array(0, dim = c(5,20))
    checkException(res <- DEopt(OF = OF, algo), silent = TRUE)
    algo$initP <- NULL

    ## exception: CR > 1, CR < 0
    algo$CR <- 2
    checkException(res <- DEopt(OF = OF, algo), silent = TRUE)
    algo$CR <- -1
    checkException(res <- DEopt(OF = OF, algo), silent = TRUE)

    ## check if Fmat/xlist are returned
    ## ...if FALSE
    trefethen <- function(xx) {
        x <- xx[1L]; y <- xx[2L]
        res <- exp(sin(50*x)) + sin(60*exp(y)) + sin(70*sin(x)) +
            sin(sin(80*y)) - sin(10*(x+y))  + (x^2+y^2)/4
        res
    }
    algo <- list(nP = 100, nG = 300, F = 0.5, CR = 0.9,
                 min = c(-3, -3), max = c( 3,  3),
                 printBar = FALSE, printDetail = FALSE,
                 storeF = FALSE, storeSolutions = FALSE)
    sol <- DEopt(OF = trefethen, algo)
    checkTrue(is.na(sol$Fmat))
    checkTrue(is.na(sol$xlist))
    ## ...if TRUE
    algo <- list(nP = 100, nG = 300, F = 0.5, CR = 0.9,
                 min = c(-3, -3), max = c( 3,  3),
                 printBar = FALSE, printDetail = FALSE,
                 storeF = TRUE, storeSolutions = TRUE)
    sol <- DEopt(OF = trefethen, algo)
    checkEquals(dim(sol$Fmat), c(algo$nG, algo$nP))
    checkEquals(length(sol$xlist[[1L]]), algo$nG)
    checkEquals( dim(sol$xlist[[c(1L,algo$nG)]]),
                c(length(algo$min), algo$nP) )

}



## PSopt
test.PSopt <- function() {
    ## test function: PS should find minimum
    OF <- tfRosenbrock
    size <- 3L ### define dimension
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
    algo$initP <- function() array(0, dim = c(5,20))
    checkException(res <- PSopt(OF = OF, algo), silent = TRUE)
    algo$initP <- NULL


    ## check if Fmat/xlist are returned
    ## ...if FALSE
    algo <- list(printBar = FALSE,
                 printDetail = FALSE,
                 nP = 50L, nG = 1000L,
                 c1 = 0.0, c2 = 1.5,
                 iner = 0.8, initV = 0.50, maxV = 50,
                 min = rep(-50, size),
                 max = rep( 50, size),
                 storeF = FALSE,
                 storeSolutions = FALSE)
    sol <- PSopt(OF = OF, algo = algo)
    checkTrue(is.na(sol$Fmat))
    checkTrue(is.na(sol$xlist))
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
    xTRUE <- runif(5)
    data <- list(xTRUE = xTRUE, step = 0.02)
    OF <- function(x, data) max(abs(x - data$xTRUE))
    neighbour <- function(x, data) {
        x <- x + runif(length(data$xTRUE))*data$step - data$step/2
        x
    }
    x0 <- runif(5)
    algo <- list(q = 0.05, nS = 1000L, nT = 15L,
                 neighbour = neighbour, x0 = x0,
                 printBar = FALSE,
                 printDetail = FALSE)
    res <- TAopt(OF, algo = algo, data = data)
    checkTrue(res$OFvalue < 0.005)

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
    algo <- list(q = 0, nS = 1000L, nT = 15L,
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
    neighbour <- function(x, data) {
        x <- x + runif(length(data$xTRUE))*data$step - data$step/2
        x
    }
    x0 <- runif(5)
    algo <- list(nS = 10000L,
                 neighbour = neighbour, x0 = x0,
                 printBar = FALSE, printDetail = FALSE)
    res <- LSopt(OF, algo = algo, data = data)
    checkTrue(res$OFvalue < 0.005)

    algo <- list(nS = 10000L,
                 neighbour = neighbour, x0 = x0,
                 printBar = FALSE, printDetail = 1000)
    res <- LSopt(OF, algo = algo, data = data)
    checkTrue(res$OFvalue < 0.005)

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


## GAopt
test.GAopt <- function() {
    size <- 20L; y <- runif(size) > 0.5; x <- runif(size) > 0.5
    data <- list(y = y)
    algo <- list(nB = size, nP = 25L, nG = 150L, prob = 0.002,
                 printBar = FALSE, printDetail = FALSE,
                 crossover = "uniform")
    OF <- function(x, data) sum(x != y)
    solGA <- GAopt(OF, algo = algo, data = data)
    checkEqualsNumeric(solGA$OFvalue, 0)

    ## error -- wrong size of initP
    algo$initP <- array(FALSE, dim = c(20,10))
    checkException(res <- GAopt(OF = OF, algo), silent = TRUE)
    algo$initP <- function() array(FALSE, dim = c(20L,20L))
    checkException(res <- GAopt(OF = OF, algo), silent = TRUE)
    algo$initP <- NULL

    ## error -- prob > 1, prob < 0
    algo$prob <- 2
    checkException(res <- GAopt(OF = OF, algo), silent = TRUE)
    algo$prob <- -0.1
    checkException(res <- GAopt(OF = OF, algo), silent = TRUE)

    ## error -- wrong crossover type
    algo <- list(nB = size, nP = 25L, nG = 150L, prob = 0.002,
                 printBar = FALSE, printDetail = FALSE,
                 crossover = "twoPoint")
    checkException(solGA <- GAopt(OF, algo = algo, data = data),
                   silent = TRUE)

    ## error -- OF not a function
    algo <- list(nB = size, nP = 25L, nG = 150L, prob = 0.002,
                 printBar = FALSE, printDetail = FALSE,
                 crossover = "uniform")
    a <- numeric(10L)
    checkException(solGA <- GAopt(a, algo = algo, data = data),
                   silent = TRUE)

    ## check repair
    resample <- function(x, ...) x[sample.int(length(x), ...)]
    repairK <- function(x, data) {
        sx <- sum(x)
        if (sx > data$kmax) {
            i <- resample(which(x), sx - data$kmax)
            x[i] <- FALSE
        }
        x
    }
    repairK2 <- function(x, data) {
        sx <- colSums(x)
        whichCols <- which(sx > data$kmax)
        for (j in seq(along.with = whichCols)) {
            jj <- whichCols[j]
            i <- resample(which(x[ ,jj]), sx[jj] - data$kmax)
            x[i, jj] <- FALSE
        }
        x
    }
    data$kmax <- 5
    tempP <- array(TRUE, dim = c(20,10))
    checkTrue(all(colSums(repairK2(tempP,data))<=data$kmax))


    algo$repair <- repairK
    solGA <- GAopt(OF, algo = algo, data = data)
    checkTrue(sum(solGA$xbest)<=data$kmax)

    algo$repair <- repairK2; algo$loopRepair <- FALSE
    solGA <- GAopt(OF, algo = algo, data = data)
    checkTrue(sum(solGA$xbest)<=data$kmax)


    ## warning changed to error/reset on.exit
    op <- options("warn"); on.exit(options(op))
    options(warn = 2)
    size <- 20L
    OF <- function(x, y) {
        sum(x != y)
    }
    y <- runif(size) > 0.5 ## the true solution
    algo <- list(nB = size, nP = 20L, nG = 10L, prob = 0.002,
                 printBar = FALSE, methodOF = "snow")
    checkException(sol <- GAopt(OF, algo = algo, y = y), silent = TRUE)

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
