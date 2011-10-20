## tests for 'multicore' and 'snow': _not_ run automatically

runMC <- FALSE

if (runMC) {

    require("NMOF")

    ## packages for distr. computing/testing
    if (!require("multicore", quietly = TRUE))
        warning("package 'multicore' not available")
    if (!require("snow", quietly = TRUE))
        warning("package 'snow' not available")
    if (!require("RUnit", quietly = TRUE))
        warning("package 'RUnit' not available")

    ## packages for parallel random numbers
    if (!require("rlecuyer", quietly = TRUE))
        warning("package 'rlecuyer' not available")
    if (!require("rsprng", quietly = TRUE))
        warning("package 'rsprng' not available")

    ## gridSearch ##
    testFun  <- function(x) {
        Sys.sleep(0.1)
        x[1L] + x[2L]^2
    }
    lower <- 1:2; upper <- 5; n <- 5
    cat(system.time(sol1 <- gridSearch(fun = testFun,
                                       lower = lower, upper = upper,
                                       n = n, printDetail = FALSE)))
    cat(system.time(sol2 <- gridSearch(fun = testFun,
                                       lower = lower, upper = upper,
                                       n = n, printDetail = FALSE,
                                       method = "multicore")))
    cat(system.time(sol3 <- gridSearch(fun = testFun,
                                       lower = lower, upper = upper,
                                       n = n, printDetail = FALSE,
                                       method = "snow", cl = 2L)))
    checkEquals(sol1, sol2)
    checkEquals(sol1, sol3)
    checkEquals(sol3$minlevels, 1:2)

    ## ... with additional argument 'k'
    testFun  <- function(x, k) {
        Sys.sleep(0.1)
        x[1L] + x[2L]^2+k
    }
    lower <- 1:2; upper <- 5; n <- 5; k <- 1
    system.time(sol1 <- gridSearch(fun = testFun,k=k,
                                   lower = lower, upper = upper,
                                   n = n, printDetail = FALSE))
    system.time(sol2 <- gridSearch(fun = testFun,k=k,
                                   lower = lower, upper = upper,
                                   n = n, printDetail = FALSE,
                                   method = "multicore"))
    system.time(sol3 <- gridSearch(fun = testFun,k=k,
                                   lower = lower, upper = upper,
                                   n = n, printDetail = FALSE,
                                   method = "snow", cl = 2L))
    checkEquals(sol1, sol2)
    checkEquals(sol1, sol3)
    checkEquals(sol3$minlevels, 1:2)

    ## ... seeds
    testFun  <- function(x) {
        Sys.sleep(0.1)
        x[1L] + x[2L] + runif(1)
    }
    lower <- 1:2; upper <- 5; n <- 3
    sol2 <- gridSearch(fun = testFun,
                       lower = lower, upper = upper,
                       n = n, printDetail = FALSE,
                       method = "multicore",
                       mc.control = list(mc.set.seed = FALSE))

    cl <- makeCluster(c(rep("localhost", 2)), type = "SOCK")
    clusterSetupSPRNG(cl, seed=rep(12345, 2))
    sol3 <- gridSearch(fun = testFun,
                       lower = lower, upper = upper,
                       n = n, printDetail = FALSE,
                       method = "snow", cl = cl)
    stopCluster(cl)
    temp <- sol3$values
    cl <- makeCluster(c(rep("localhost", 2)), type = "SOCK")
    ##clusterSetupRNGstream (cl, seed=rep(12345, 2))
    clusterSetupSPRNG(cl, seed=rep(12345, 2))
    sol3 <- gridSearch(fun = testFun,
                       lower = lower, upper = upper,
                       n = n, printDetail = FALSE,
                       method = "snow", cl = cl)
    stopCluster(cl)
    checkEquals(sol3$values, temp)


    cl <- makeCluster(c(rep("localhost", 2)), type = "SOCK")
    clusterSetupRNGstream (cl, seed=rep(12345, 2))
    sol3 <- gridSearch(fun = testFun,
                       lower = lower, upper = upper,
                       n = n, printDetail = FALSE,
                       method = "snow", cl = cl)
    stopCluster(cl)
    temp <- sol3$values
    cl <- makeCluster(c(rep("localhost", 2)), type = "SOCK")
    clusterSetupRNGstream (cl, seed=rep(12345, 2))
    sol3 <- gridSearch(fun = testFun,
                       lower = lower, upper = upper,
                       n = n, printDetail = FALSE,
                       method = "snow", cl = cl)
    stopCluster(cl)
    checkEquals(sol3$values, temp)



    ## bracketing ##
    testFun <- function(x) {
        Sys.sleep(0.1)
        cos(1/x^2)
    }
    system.time(sol1 <- bracketing(testFun, interval = c(0.3, 0.9),
                                   n = 100L))
    system.time(sol2 <- bracketing(testFun, interval = c(0.3, 0.9),
                                   n = 100L, method = "multicore"))
    system.time(sol3 <- bracketing(testFun, interval = c(0.3, 0.9),
                                   n = 100L, method = "snow", cl = 2L))
    checkEquals(sol1, sol2)
    checkEquals(sol1, sol3)


    ## restartOpt ##
    xTRUE <- runif(5L)
    data <- list(xTRUE = xTRUE, step = 0.02)
    OF <- function(x, data)
        max(abs(x - data$xTRUE))
    neighbour <- function(x, data)
        x + runif(length(data$xTRUE))*data$step - data$step/2
    x0 <- runif(5L)
    algo <- list(q = 0.05, nS = 100L, nT = 5L,
                 neighbour = neighbour, x0 = x0,
                 printBar = FALSE, printDetail = FALSE)
    system.time(sols1 <- restartOpt(fun = TAopt, n = 100L,
                                    OF = OF, algo = algo, data = data))
    checkEquals(length(sols1), 100L)
    system.time(sols2 <- restartOpt(fun = TAopt, n = 100L,
                                    OF = OF, algo = algo, data = data,
                                    method = "multicore"))
    checkEquals(length(sols2), 100L)
    system.time(sols3 <- restartOpt(fun = TAopt, n = 100L,
                                    OF = OF, algo = algo, data = data,
                                    method = "snow", cl = 2L))
    checkEquals(length(sols3), 100L)
}

