require("NMOF")

# MA
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



# HESTON
test.callHestoncf <- function() {
    S <- 100; X <- 100; tau <- 1; r <- 0.02; q <- 0.01
    v0  <- 0.2^2; vT  <- 0.2^2
    rho <- -0.5; k <- 0.5; sigma <- 0.5
    result <- callHestoncf(S = S, X = X, tau = tau, r = r, q = q, 
        v0 = v0, vT = vT, rho = rho, k = k, sigma = sigma)
    checkEquals(round(result[[1L]], 3), 7.119)
    
    S <- 100; X <- 100; tau <- 1; r <- 0.02; q <- 0.01
    v0  <- 0.2^2; vT  <- 0.2^2
    rho <- -0.5; k <- 0.5; sigma <- 0.01
    result <- callHestoncf(S = S, X = X, tau = tau, r = r, q = q, 
        v0 = v0, vT = vT, rho = rho, k = k, sigma = sigma)
    checkEquals(round(result[[1L]], 3), 8.347)
    
    S <- 100; X <- 90; tau <- 1; r <- 0.02; q <- 0.01
    v0  <- 0.2^2; vT  <- 0.2^2
    rho <- -0.5; k <- 0.5; sigma <- 1
    result <- callHestoncf(S = S, X = X, tau = tau, r = r, q = q, 
        v0 = v0, vT = vT, rho = rho, k = k, sigma = sigma)
    checkEquals(round(result[[1L]], 3), 13.362)
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
    # EuropeanCall and EuropeanCallBE should give the same results
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
        x <- xx[1L]; y <- xx[2L]
        res <- exp(sin(50*x)) + sin(60*exp(y)) + sin(70*sin(x)) +
            sin(sin(80*y)) - sin(10*(x+y))  + (x^2+y^2)/4
        res
    }
    
    algo <- list(nP = 100, nG = 300,
        F = 0.5, CR = 0.9, 
        min = c(-3, -3), max = c( 3,  3), 
        printBar = FALSE, printDetail = FALSE)
    
    # DE should solve the problem
    sol <- DEopt(OF = trefethen, algo)
    format(sol$OFvalue, digits = 12)
    checkEquals(round(sol$OFvalue,2), -3.31)
    
    # without max vector, DE should give error
    algo$max <- NULL
    checkException(res <- DEopt(OF = trefethen, algo), silent = TRUE)
    
    # unused parameter 'z' should cause error
    algo$max <- c( 3,  3)
    checkException(res <- DEopt(OF = trefethen, algo, z = 2), silent = TRUE)
    
    # 'z' is required but not provided >> error 
    trefethen2 <- function(xx, z) {
        x <- xx[1]; y <- xx[2]
        res <- exp(sin(50*x)) + sin(60*exp(y)) + sin(70*sin(x)) +
            sin(sin(80*y)) - sin(10*(x+y))  + (x^2+y^2)/4
        res + z
    }
    checkException(res <- DEopt(OF = trefethen2, algo), silent = TRUE)
    
    # 'z' is required and provided
    checkEquals(round(
            DEopt(OF = trefethen2, algo, z = 2)$OFvalue,2),-1.31)
    checkEquals(round(
            DEopt(OF = trefethen2, algo,     2)$OFvalue,2),-1.31)       
    
    # test function: DE should find minimum
    OF <- tfRosenbrock
    size <- 5L        # define dimension
    algo <- list(
        printBar = FALSE, 
        printDetail = FALSE,
        nP = 100L, nG = 500L,
        F = 0.5, CR = 0.9, 
        min = rep(-50, size), 
        max = rep( 50, size))
    
    sol <- DEopt(OF = OF, algo = algo)
    checkEquals(sol$OFvalue, 0)
    
    # exception: wrong size of initP
    algo$initP <- array(0, dim = c(20,20))
    checkException(res <- DEopt(OF = OF, algo), silent = TRUE)
    algo$initP <- function() array(0, dim = c(5,20))
    checkException(res <- DEopt(OF = OF, algo), silent = TRUE)
    algo$initP <- NULL
    
    # exception: CR > 1, CR < 0
    algo$CR <- 2
    checkException(res <- DEopt(OF = OF, algo), silent = TRUE)
    algo$CR <- -1
    checkException(res <- DEopt(OF = OF, algo), silent = TRUE)    
}



# PSopt
test.PSopt <- function() {
    # test function: PS should find minimum 
    OF <- tfRosenbrock
    size <- 3L        # define dimension
    algo <- list(
        printBar = FALSE, 
        printDetail = FALSE,
        nP = 50L, nG = 1000L,
        c1 = 0.0, c2 = 1.5,
        iner = 0.8, initV = 0.50, maxV = 50,
        min = rep(-50, size), 
        max = rep( 50, size))
    
    sol <- PSopt(OF = OF, algo = algo)
    checkEquals(sol$OFvalue, 0)
    
    # exception: wrong size of initP
    algo$initP <- array(0, dim = c(20L,20L))
    checkException(res <- PSopt(OF = OF, algo), silent = TRUE)
    algo$initP <- function() array(0, dim = c(5,20))
    checkException(res <- PSopt(OF = OF, algo), silent = TRUE)
    algo$initP <- NULL    
    
}



# TAopt
test.TAopt <- function() {
    
    # TA should come close to the minimum
    xTRUE <- runif(5)
    data <- list(xTRUE = xTRUE, step = 0.02)
    OF <- function(x, data) max(abs(x - data$xTRUE))
    neighbour <- function(x, data) {
        #bit <- sample.int(length(data$xTRUE), 1L)
        x <- x + runif(length(data$xTRUE))*data$step - data$step/2
        x
    }
    x0 <- runif(5)
    algo <- list(
        q = 0.05, nS = 1000L, nT = 15L, 
        neighbour = neighbour, x0 = x0,
        printBar = FALSE, 
        printDetail = FALSE)
    res <- TAopt(OF, algo = algo, data = data)
    checkTrue(res$OFvalue < 0.005)
    
    # length(returned thresholds) == specified length(thresholds) 
    checkTrue(length(res$vT) == algo$nT)
    
    # specified thresholds are used
    algo$vT <- c(0.1,0.05,0)
    algo$nS <- 1000L
    res <- TAopt(OF, algo = algo, data = data)
    checkEqualsNumeric(res$vT,algo$vT) 
    
    # stepUp is used
    algo$stepUp <- 2L
    res <- TAopt(OF, algo = algo, data = data)
    checkEqualsNumeric(res$vT, rep(algo$vT, 3L))    
    
    # scale is used
    algo$stepUp <- 0L
    algo$scale <- 1.5
    res <- TAopt(OF, algo = algo, data = data)
    checkEqualsNumeric(res$vT, algo$scale*c(0.1,0.05,0))    
}


# LSopt
test.LSopt <- function() {
    xTRUE <- runif(5)
    data <- list(xTRUE = xTRUE, step = 0.02)
    OF <- function(x, data) max(abs(x - data$xTRUE))
    neighbour <- function(x, data) {
        #bit <- sample.int(length(data$xTRUE), 1L)
        x <- x + runif(length(data$xTRUE))*data$step - data$step/2
        x
    }
    x0 <- runif(5)
    algo <- list(nS = 10000L, 
        neighbour = neighbour, x0 = x0,
        printBar = FALSE, printDetail = FALSE)
    res <- LSopt(OF, algo = algo, data = data)
    checkTrue(res$OFvalue < 0.005)        
}


# TESTfunctions
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


# GAopt
test.GAopt <- function() {
    size <- 20L; y <- runif(size) > 0.5; x <- runif(size) > 0.5
    data <- list(y = y)
    algo <- list(nB = size, nP = 25L, nG = 150L, prob = 0.002, 
        printBar = FALSE, printDetail = FALSE, crossover = "uniform")
    OF <- function(x, data) sum(x != y)
    solGA <- GAopt(OF, algo = algo, data = data)
    checkEqualsNumeric(solGA$OFvalue, 0)
    
    # error -- wrong size of initP
    algo$initP <- array(FALSE, dim = c(20,10))
    checkException(res <- GAopt(OF = OF, algo), silent = TRUE)
    algo$initP <- function() array(FALSE, dim = c(20,20))
    checkException(res <- GAopt(OF = OF, algo), silent = TRUE)
    algo$initP <- NULL
    
    # error -- prob > 1, prob < 0
    algo$prob <- 2
    checkException(res <- GAopt(OF = OF, algo), silent = TRUE)
    algo$prob <- -0.1
    checkException(res <- GAopt(OF = OF, algo), silent = TRUE)
    
    # error -- wrong crossover type
    algo <- list(nB = size, nP = 25L, nG = 150L, prob = 0.002, 
        printBar = FALSE, printDetail = FALSE, crossover = "twoPoint")
    checkException(solGA <- GAopt(OF, algo = algo, data = data), silent = TRUE)
    
    # error -- OF not a function
    algo <- list(nB = size, nP = 25L, nG = 150L, prob = 0.002, 
        printBar = FALSE, printDetail = FALSE, crossover = "uniform")
    a <- numeric(10)
    checkException(solGA <- GAopt(a, algo = algo, data = data), silent = TRUE)
}

# TA portfolio optimisation
test.TAopt.Applications <- function() {
    # [requires quadprog]
    # Test if TA can solve two simple portfolio optimisation
    # problems: the minimum-variance portfolio and the tangency
    # portfolio (subject to constraints).
    if (require(quadprog, quietly = TRUE)) {
        # na      - number of assets
        # ns      - observations
        # R       - returns
        # wsup    - maximum holding size
        # winf    - minimum holding size
        # mu, mu2 - means, excess means over risk-free rate rf
        
        na <- 50L; ns <- 100L 
        R  <- array(rnorm(ns*na, mean = 0.005, sd = 0.015), dim = c(ns,na))
        mu <- colMeans(R); rf <- 0.0001; mu2 <- mu - rf     
        
        ## TEST 1: minimum-variance portfolio
        wsup <- 0.05; winf <- -0.05; Q <- 2*cov(R)  
        A <- array( 1, dim = c(1,na)); a <- 1
        B <- rbind(-diag(na),diag(na))
        b <- rbind(array(-wsup, dim = c(na,1)),
            array( winf, dim = c(na,1)))
        result <- solve.QP(Dmat = Q, dvec = rep(0,na),
            Amat = t(rbind(A,B)), bvec = rbind(a,b),
            meq = 1)        
        data <- list(RR = cov(R), na = na, ns = ns, 
            eps = 0.25/100, winf = winf, wsup = wsup)
        resample <- function(x, ...) x[sample.int(length(x), ...)]
        neighbour <- function(w, data){
            eps <- runif(1) * data$eps
            toSell <- w > data$winf; toBuy  <- w < data$wsup
            i <- resample(which(toSell), size = 1L)
            j <- resample(which(toBuy),  size = 1L)
            eps <- min(w[i] - data$winf, data$wsup - w[j], eps)
            w[i] <- w[i] - eps; w[j] <- w[j] + eps
            w
        }
        OF <- function(w, data) {
            aux <- crossprod(data$RR,w) 
            crossprod(w,aux)
        }
        w0 <- runif(na); w0 <- w0/sum(w0)
        algo <- list(x0 = w0, neighbour = neighbour, 
            nS = 2500L, nT = 10L, nD = 2000L, q = 0.05,
            printBar = FALSE, printDetail = FALSE)
        res <- TAopt(OF,algo,data)
        dF <- as.numeric(16 * 100 *sqrt(res$OFvalue)) - 
            as.numeric(16 * 100 *sqrt(result$value))
        checkEquals(round(dF,4),0)
        
        ## TEST 2: tangency portfolio with non-negative weights
        winf <- 0; Q <- cov(R)            
        A <- array(mu2, dim = c(1L, na)); a <- 1
        B <- diag(na); b <- array( winf, dim = c(na,1L))
        result <- solve.QP(Dmat = Q, dvec = rep(0,na),
            Amat = t(rbind(A,B)), bvec = rbind(a,b),
            meq = 1)
        w <- as.matrix(result$solution/sum(result$solution))
        SR <- t(w) %*% mu2 / sqrt(t(w) %*% Q %*% w)
        OF2 <- function(w, data) {
            aux <- crossprod(data$RR,w) 
            sqrt(crossprod(w,aux)) / t(w) %*% data$mu2
        }
        w0 <- runif(na); w0 <- w0/sum(w0)
        data <- list(RR = cov(R), na = na, ns = ns, mu2 = mu2,
            eps = 0.25/100, winf = winf, wsup = 1)
        res <- TAopt(OF2,algo,data)
        dSR <- 1/res$OFvalue - as.numeric(SR)
        checkEquals(as.numeric(round(dSR,3)),0)        
    }
}
