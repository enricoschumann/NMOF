## -*- truncate-lines: t; -*-

test.ytm <- function() {
    cf <- c(5, 5, 5, 5, 5, 105)   ## cashflows
    times <- 1:6                  ## maturities
    y <- 0.0127                   ## the "true" yield
    b0 <- vanillaBond(cf, times, yields = y)
    cf <- c(-b0, cf); times <- c(0, times)    
    checkEquals(y, ytm(cf, times), tolerance = 1e-5)
    checkException(checkEquals(y, ytm(cf, times), tolerance = 1e-7),
                   silent = TRUE)
    checkEquals(y, ytm(cf, times, tol = 1e-8), tolerance = 1e-7)
}

