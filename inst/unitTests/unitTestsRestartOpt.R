test.restartOpt <- function() {
    testParallel <- FALSE

    xTRUE <- rep(0, 5)
    data <- list(xTRUE = xTRUE)
    OF <- function(x, data)
        max(abs(x - data$xTRUE))
    neighbour <- function(x, data)
        rep(0, 5)

    x0 <- runif(5L)
    algo <- list(q = 0.01, nS = 1L, nT = 5L,
                 neighbour = neighbour, x0 = x0,
                 printBar = FALSE, printDetail = FALSE)

    sols <- restartOpt(fun = TAopt, n = 5L,
                       OF = OF, algo = algo, data = data)
    checkEquals(length(sols), 5L)

    ## w <- getOption("warn")
    ## options(warn = 2)
    ## checkException(sols <- restartOpt(
    ##                    fun = TAopt, n = 5L,
    ##                    OF = OF, algo = algo, data = data,
    ##                    best.only = TRUE), silent = TRUE)
    ## options(warn = w)


    
    ## tests for snow/multicore: slow!
    if (testParallel) {
        OF <- function(x, data) {
            Sys.sleep(1e-3)
            max(abs(x - data$xTRUE))
        }
        if (require("parallel", quietly = TRUE)){
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
        if (suppressWarnings(require("parallel", quietly = TRUE))) {
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

