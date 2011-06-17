require("NMOF")

# MA
test.MA <- function() {
    x <- rnorm(100); myMA <- numeric(length(x)); order <- 5 
    for (i in order:length(x)) 
        myMA[i] <- sum(x[(i - order + 1):i])/order   
    
    checkEquals(MA(x,order = order)[-(1:(order-1))], 
        myMA[-(1:(order-1))])
    
    x <- rnorm(100); myMA <- numeric(length(x)); order <- 1
    checkEquals(x, MA(x, order = order))
    checkEquals(x, MA(x, order = order, pad = NA))
}



# HESTON
test.callHestoncf <- function() {
    S <- 100; X <- 100; tau <- 1; r <- 0.02; q <- 0.01
    v0  <- 0.2^2; vT  <- 0.2^2
    rho <- -0.5; k <- 0.5; sigma <- 0.5
    result <- callHestoncf(S = S, X = X, tau = tau, r = r, q = q, 
        v0 = v0, vT = vT, rho = rho, k = k, sigma = sigma)
    checkEquals(round(result[[1]], 3), 7.119)
    
    S <- 100; X <- 100; tau <- 1; r <- 0.02; q <- 0.01
    v0  <- 0.2^2; vT  <- 0.2^2
    rho <- -0.5; k <- 0.5; sigma <- 0.01
    result <- callHestoncf(S = S, X = X, tau = tau, r = r, q = q, 
        v0 = v0, vT = vT, rho = rho, k = k, sigma = sigma)
    checkEquals(round(result[[1]], 3), 8.347)
    
    S <- 100; X <- 90; tau <- 1; r <- 0.02; q <- 0.01
    v0  <- 0.2^2; vT  <- 0.2^2
    rho <- -0.5; k <- 0.5; sigma <- 1
    result <- callHestoncf(S = S, X = X, tau = tau, r = r, q = q, 
        v0 = v0, vT = vT, rho = rho, k = k, sigma = sigma)
    checkEquals(round(result[[1]], 3), 13.362)
}



# EUROPEAN
test.EuropeanCall <- function() {
    S0 <- 10; X <- 10; r <- 0.02; tau <- 1; sigma <- 0.20; M = 101
    res <- EuropeanCall(S0 = S0, X = X, 
        r = r, tau = tau, sigma = sigma, M = M)
    checkEquals(round(res,2), 0.89)
    
    S0 <- 10; X <- 6; r <- 0.02; tau <- 1; sigma <- 0.20; M = 101
    res <- EuropeanCall(S0 = S0, X = X, 
        r = r, tau = tau, sigma = sigma, M = M)
    checkEquals(round(res,2), 4.12)
    
    S0 <- 10; X <- 10; r <- 0.00; tau <- 1; sigma <- 0.20; M = 101
    res <- EuropeanCall(S0 = S0, X = X, 
        r = r, tau = tau, sigma = sigma, M = M)
    checkEquals(round(res,2), 0.80)
    
    S0 <- 10; X <- 10; r <- 0.02; tau <- 1/12; sigma <- 0.20; M = 101
    res <- EuropeanCall(S0 = S0, X = X, 
        r = r, tau = tau, sigma = sigma, M = M)
    checkEquals(round(res,2), 0.24)
    
    S0 <- 10; X <- 10; r <- 0.02; tau <- 1/12; sigma <- 0.80; M = 101
    res <- EuropeanCall(S0 = S0, X = X, 
        r = r, tau = tau, sigma = sigma, M = M)
    checkEquals(round(res,2), 0.93)
    
    S0 <- 10; X <- 10; r <- 0.02; tau <- 1/12; sigma <- 0.02; M = 101
    res <- EuropeanCall(S0 = S0, X = X, 
        r = r, tau = tau, sigma = sigma, M = M)
    checkEquals(round(res,2), 0.03)
    
}
test.EuropeanCallBE <- function() {
    S0 <- 10; X <- 10; r <- 0.02; tau <- 1; sigma <- 0.20; M = 101
    res <- EuropeanCall(S0 = S0, X = X, 
        r = r, tau = tau, sigma = sigma, M = M)
    res2 <- EuropeanCallBE(S0 = S0, X = X, 
        r = r, tau = tau, sigma = sigma, M = M)
    checkEquals(res,res2)
    
    S0 <- 10; X <- 6; r <- 0.02; tau <- 1; sigma <- 0.20; M = 101
    res <- EuropeanCall(S0 = S0, X = X, 
        r = r, tau = tau, sigma = sigma, M = M)
    res2 <- EuropeanCallBE(S0 = S0, X = X, 
        r = r, tau = tau, sigma = sigma, M = M)
    checkEquals(res,res2)
    
    S0 <- 10; X <- 10; r <- 0.00; tau <- 1; sigma <- 0.20; M = 101
    res <- EuropeanCall(S0 = S0, X = X, 
        r = r, tau = tau, sigma = sigma, M = M)
    res2 <- EuropeanCallBE(S0 = S0, X = X, 
        r = r, tau = tau, sigma = sigma, M = M)
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



# DE
test.DEopt <- function() {
    trefethen <- function(xx) {
        x <- xx[1]; y <- xx[2]
        res <- exp(sin(50*x)) + sin(60*exp(y)) + sin(70*sin(x)) +
            sin(sin(80*y)) - sin(10*(x+y))  + (x^2+y^2)/4
        res
    }
    
    algo <- list(nP = 100, nG = 300,
        F = 0.5, CR = 0.9, 
        min = c(-3, -3), max = c( 3,  3), 
        printBar = FALSE, printDetail = FALSE)
    
    # test 1: DE should solve the problem
    sol <- DEopt(OF = trefethen, algo)
    format(sol$OFvalue, digits = 12)
    checkEquals(round(sol$OFvalue,2), -3.31)
    
    # test 2: without max vector, DE should give error
    algo$max <- NULL
    checkException(res <- DEopt(OF = trefethen, algo), silent = TRUE)
    
    # test 3: unused parameter 'z' should cause error
    algo$max <- c( 3,  3)
    checkException(res <- DEopt(OF = trefethen, algo, z = 2), silent = TRUE)
    
    # test 4: 'z' is required but not provided >> error 
    trefethen2 <- function(xx, z) {
        x <- xx[1]; y <- xx[2]
        res <- exp(sin(50*x)) + sin(60*exp(y)) + sin(70*sin(x)) +
            sin(sin(80*y)) - sin(10*(x+y))  + (x^2+y^2)/4
        res + z
    }
    checkException(res <- DEopt(OF = trefethen2, algo), silent = TRUE)
    
    # test 5: 'z' is required and provided
    checkEquals(round(
            DEopt(OF = trefethen2, algo, z = 2)$OFvalue,2),-1.31)
    checkEquals(round(
            DEopt(OF = trefethen2, algo,     2)$OFvalue,2),-1.31)       
    
    
    # test function
    OF <- tfRosenbrock
    size <- 5L        # define dimension
    algo <- list(
        printBar = FALSE, 
        printDetail = FALSE,
        nP = 100L, nG = 500L,
        F = 0.5, CR = 0.9, 
        min = rep(-50, size), 
        max = rep( 50, size))
    
    system.time(sol <- DEopt(OF = OF, algo = algo))
    checkEquals(sol$OFvalue, 0)
    
}



# PSopt
test.PSopt <- function() {
    # test function
    OF <- tfRosenbrock
    size <- 5L        # define dimension
    algo <- list(
        printBar = FALSE, 
        printDetail = FALSE,
        nP = 100L, nG = 2000L,
        c1 = 0.0, c2 = 1.5,
        iner = 0.8, initV = 0.00, maxV = 50,
        min = rep(-50, size), 
        max = rep( 50, size))
    
    system.time(sol <- PSopt(OF = OF, algo = algo))
    sol$OFvalue
    checkEquals(sol$OFvalue, 0)
    
}



# TAopt
test.TAopt <- function() {
    xTRUE <- runif(5)
    data <- list(xTRUE = xTRUE)
    OF <- function(x, data) max(abs(x - data$xTRUE))
    neighbour <- function(x, data) {
        bit <- sample.int(length(data$xTRUE), 1L)
        x <- x + rnorm(length(data$xTRUE))*0.01
        x
    }
    x0 <- runif(5)
    algo <- list(
        q = 0.01, nS = 5000L, nT = 15L, 
        neighbour = neighbour, x0 = x0,
        printBar = FALSE, 
        printDetail = FALSE)
    res <- TAopt(OF, algo = algo, data = data)
    checkTrue(res$OFvalue < 0.005)
    checkTrue(length(res$vT) == algo$nT)
    algo$vT <- c(0.1,0.05,0)
    algo$nS <- 1000L
    res <- TAopt(OF, algo = algo, data = data)
    checkEqualsNumeric(res$vT,algo$vT) 
    algo$stepUp <- 2L
    res <- TAopt(OF, algo = algo, data = data)
    checkEqualsNumeric(res$vT, rep(algo$vT, 3L))    
    algo$scale <- 1.5
    res <- TAopt(OF, algo = algo, data = data)
    checkEqualsNumeric(res$vT, algo$scale*c(0.1,0.05,0))    
}


# LSopt
test.LSopt <- function() {
    xTRUE <- runif(5)
    data <- list(xTRUE = xTRUE)
    OF <- function(x, data) max(abs(x - data$xTRUE))
    neighbour <- function(x, data) {
        bit <- sample.int(length(data$xTRUE), 1L)
        x <- x + rnorm(length(data$xTRUE)) * 0.01
        x
    }
    x0 <- runif(5)
    algo <- list(nS = 10000L, 
        neighbour = neighbour, x0 = x0,
        printBar = FALSE, printDetail = FALSE)
    res <- LSopt(OF, algo = algo, data = data)
    checkTrue(res$OFvalue < 0.005)        
}