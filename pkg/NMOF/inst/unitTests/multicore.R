## tests for 'multicore' and 'snow': _not_ run

runMC <- FALSE

if (runMC) {
    require("NMOF")
    if (!require("multicore"))
        stop("package 'multicore' not available")
    if (!require("snow"))
        stop("package 'snow' not available")
    if (!require("RUnit"))
        stop("package 'RUnit' not available")


    ## gridSearch ##
    testFun  <- function(x) {
        Sys.sleep(0.1)
        x[1L] + x[2L]^2
    }
    lower <- 1:3; upper <- 5; n <- 3
    system.time(sol1 <- gridSearch(fun = testFun,
                                   lower = lower, upper = upper,
                                   n = n, printDetail = FALSE))
    system.time(sol2 <- gridSearch(fun = testFun,
                                   lower = lower, upper = upper,
                                   n = n, printDetail = FALSE,
                                   method = "multicore"))
    system.time(sol3 <- gridSearch(fun = testFun,
                                   lower = lower, upper = upper,
                                   n = n, printDetail = FALSE,
                                   method = "snow", cl = 2L))
    checkEquals(sol1, sol2)
    checkEquals(sol1, sol3)
    checkEquals(sol3$minlevels, 1:3)


    ## bracketing ##
    testFun <- function(x,k) {
        Sys.sleep(0.1)
        cos(1/x^2)
    }
    system.time(bracketing(testFun, interval = c(0.3, 0.9),
                           n = 100L))
    system.time(bracketing(testFun, interval = c(0.3, 0.9),
                           n = 100L, method = "multicore"))
    system.time(bracketing(testFun, interval = c(0.3, 0.9),
                           n = 100L, method = "snow", cl = 4))


    ## restartOpt ##
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
    sols1 <- restartOpt(fun = TAopt, n = 100L,
                       OF = OF, algo = algo, data = data)
    checkEquals(length(sols1), 10L)
    sols2 <- restartOpt(fun = TAopt, n = 100L,
                       OF = OF, algo = algo, data = data,
                        method = "multicore")
    checkEquals(length(sols2), 10L)
    sols3 <- restartOpt(fun = TAopt, n = 100L,
                       OF = OF, algo = algo, data = data,
                        method = "snow", cl = 2L)
    checkEquals(length(sols3), 10L)



}
