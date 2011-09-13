## gridSearch
test.gridSearch <- function() {
    testFun  <- function(x) x[1L]   + x[2L]^2
    testFun2 <- function(x) x[[1L]] + x[[2L]]^2
    testFun3 <- function(x) sum(x^2)
    testFun4 <- function(x) sum(sapply(x, `^`, 2L))

    ## ERROR -- upper not sorted
    lower <- 1:3; upper <- 5:4; n <- 8
    checkException(sol <- gridSearch(fun = testFun,
                                     lower = lower, upper = upper,
                                     n = n, printDetail = FALSE),
                   silent = TRUE)

    ## ERROR -- upper < lower
    lower <- 1:3; upper <- 2; n <- 8
    checkException(sol <- gridSearch(fun = testFun,
                                     lower = lower, upper = upper, n = n,
                                     printDetail = FALSE), silent = TRUE)

    ##
    lower <- 1:3; upper <- 5; n <- 8
    sol <- gridSearch(fun = testFun,
                      lower = lower, upper = upper,
                      n = n, printDetail = FALSE)
    checkEquals(sol$minlevels,1:3)

    ##
    levels <- list(a = 1:2, b = 1:3, c = 4:6)
    sol <- gridSearch(fun = testFun3, levels = levels, printDetail = FALSE)
    checkEquals(sol$minlevels,c(1,1,4))
    ## check levels
    l1 <- do.call("rbind",sol$levels)
    l2 <- as.matrix(expand.grid(levels))
    dimnames(l2) <- NULL
    checkEquals(l1,l2)

    ##
    lower <- 1; upper <- 5; n <- 1
    sol <- suppressWarnings(
                            gridSearch(fun = testFun3,
                                       lower = lower, upper = upper,
                                       n = n, printDetail = FALSE))
    checkEquals(sol$minlevels,1L)
    ##
    ## NSS fit
    tm <- seq_len(10); trueP <- c(2,1,1,5,1,3); y <- NSS(trueP, tm)
    qfit <- function(x, tm, y) {
        X <- NSSf(x[1], x[2], tm)
        mm <- lm(y ~ -1 + X)
        sum(abs(y - NSS(c(coef(mm), x[1], x[2]), tm)))
    }
    res <- gridSearch(qfit, y = y, tm = tm,
                      lower = 0.0, upper = 5, npar = 2L, n = 11L)
    ll <-  res$minlevels
    X <- NSSf(ll[1L], ll[2L], tm)
    model <- lm(y ~ -1 + X)
    checkEquals(sum(abs(y - NSS(c(coef(model), ll[1L], ll[2L]), tm))), 0)
}


## bracketing
test.bracketing <- function() {
    ## example ch. 11/p. 290
    res0 <- structure(c(0.3, 0.348, 0.444, 0.78,
                       0.324, 0.372, 0.468, 0.804), .Dim = c(4L, 2L))
    testFun <- function(x) cos(1/x^2)
    checkEquals(res0, bracketing(testFun, interval = c(0.3, 0.9), n = 26L))
    checkEquals(res0, bracketing(testFun, interval = c(0.3, 0.9), n = 26L),
                method = "vectorise")

    ## run with multicore (not a formal test)
    testFun <- function(x,k) {
        Sys.sleep(0.1)
        cos(1/x^2)
    }
    t1 <- system.time(bracketing(testFun, interval = c(0.3, 0.9),
                                 n = 10L, method = "vectorise"))
    t2 <- suppressWarnings(system.time(bracketing(testFun, interval = c(0.3, 0.9),
                                                  n = 10L, method = "multicore")))

    ## no zero
    testFun <- function(x) 1
    res <- bracketing(testFun, interval = c(1,10), n = 10L)
    checkTrue(all(dim(res) == c(0L, 2L)))

    ## no zero either
    testFun <- function(x) 0
    res <- bracketing(testFun, interval = c(1,10), n = 10L)
    checkTrue(all(dim(res) == c(0L, 2L)))

    ## additional parameter
    testFun <- function(x,k) cos(1/x^2) + k
    res <- bracketing(testFun, k = 0, interval = c(0.3,0.9), n = 26L,
                      method = "vectorise")
    checkEquals(res0, res)
    res <- bracketing(testFun, k = 1, interval = c(0.3,0.9), n = 26L,
                      method = "vectorise")
    checkTrue(all(dim(res) == c(0L, 2L)))
}
