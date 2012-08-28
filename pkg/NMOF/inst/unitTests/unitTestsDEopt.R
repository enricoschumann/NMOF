## -*- truncate-lines: t; -*-
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
    ##format(sol$OFvalue, digits = 12)
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
    algo <- list(nP = 100, nG = 100, F = 0.5, CR = 0.9,
                 min = c(-3, -3), max = c( 3,  3),
                 printBar = FALSE, printDetail = FALSE,
                 storeF = FALSE, storeSolutions = FALSE)
    sol <- DEopt(OF = trefethen, algo)
    checkTrue(is.na(sol$Fmat))
    checkEquals(length(sol$Fmat), 1L)
    checkTrue(is.na(sol$xlist))
    checkEquals(length(sol$xlist), 1L)
    ## ...if TRUE
    algo <- list(nP = 100, nG = 100, F = 0.5, CR = 0.9,
                 min = c(-3, -3), max = c( 3,  3),
                 printBar = FALSE, printDetail = FALSE,
                 storeF = TRUE, storeSolutions = TRUE)
    sol <- DEopt(OF = trefethen, algo)
    checkEquals(dim(sol$Fmat), c(algo$nG, algo$nP))
    checkEquals(length(sol$xlist[[1L]]), algo$nG)
    checkEquals(dim(sol$xlist[[c(1L,algo$nG)]]),
                c(length(algo$min), algo$nP) )


    ## a trivial problem: 'recover' a vector X
    X <- 1:10 - 5
    OF <- function(x, X)
        sum(abs(x - X))
    algo <- list(nP = 30, nG = 700,
                 min = rep(-3, length(X)),
                 max = rep( 3,  length(X)),
                 minmaxConstr = FALSE,
                 printBar = FALSE, printDetail = FALSE,
                 storeF = FALSE, storeSolutions = FALSE)
    sol <- DEopt(OF, algo, X)
    checkEquals(round(sol$xbest,3), X)
    algo$minmaxConstr <- TRUE
    sol <- DEopt(OF, algo, X)
    round(sol$xbest,3)
    checkEquals(round(sol$xbest,3), c(-3, -3, -2, -1, 0, 1, 2, 3, 3, 3))

    ## vectorised comp: error if
    X <- 1:10 - 5
    OF <- function(x, X)  ## correct
        colSums(abs(x - X))
    OF <- function(x, X) {  ## WRONG
        res <- colSums(abs(x - X))
        res <- res[-1]
    }
    algo <- list(nP = 20, nG = 2,
                 min = rep(-3, length(X)),
                 max = rep( 3,  length(X)),
                 loopOF = FALSE,
                 printBar = FALSE, printDetail = FALSE,
                 storeF = FALSE, storeSolutions = FALSE)
    checkException(sol <- DEopt(OF, algo, X), silent = TRUE)

    ## CHECK STATE
    trefethen <- function(xx) {
        x <- xx[1L]; y <- xx[2L]
        exp(sin(50*x)) + sin(60*exp(y)) + sin(70*sin(x)) +
            sin(sin(80*y)) - sin(10*(x+y))  + (x^2+y^2)/4
    }
    algo <- list(nP = 100, nG = 2, F = 0.5, CR = 0.9,
                 min = c(-3, -3), max = c( 3, 3),
                 printBar = FALSE, printDetail = TRUE)

    ## DE should solve the problem
    sol <- DEopt(OF = trefethen, algo)
    if (!is.na(sol$initial.state[1L])) {
        assign(".Random.seed", sol$initial.state, envir = .GlobalEnv)
        sol2 <- DEopt(OF = trefethen, algo)
        checkEquals(sol, sol2)
    }

}
