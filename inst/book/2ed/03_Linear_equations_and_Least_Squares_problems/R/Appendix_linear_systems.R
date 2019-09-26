### R code from "Numerical Methods and Optimization in Finance"

###################################################
### code chunk number 1: appendix-settings
###################################################
Sys.setenv(LANGUAGE = "en")
options(continue = "  ",
        digits = 4,
        width = 55,
        str = strOptions(strict.width = "cut"),
        useFancyQuotes = FALSE,
        warn = 2)


###################################################
### code chunk number 2: create-matrix
###################################################
n <- 5
x1 <- numeric(n) + 1
x2 <- numeric(n) + 2
A <- array(sample(1:50, n*n, replace = TRUE),
           dim = c(n, n))
A
b1 <- A %*% x1
b2 <- A %*% x2


###################################################
### code chunk number 3: solve
###################################################
solve(A, b1)
solve(A, b2)


###################################################
### code chunk number 4: solve-both
###################################################
solve(A, cbind(b1, b2))


###################################################
### code chunk number 5: data
###################################################
nr <- 100
np <- 3
X <- array(rnorm(nr*np), dim = c(nr, np))
y <- rnorm(nr)


###################################################
### code chunk number 6: lm
###################################################
lm(y ~ X)


###################################################
### code chunk number 7: qr
###################################################
qr.solve(cbind(1, X), y)


###################################################
### code chunk number 8: lm-fit
###################################################
str(.lm.fit(cbind(1, X), y))
str( lm.fit(cbind(1, X), y))


###################################################
### code chunk number 9: speed-comparison
###################################################
library("rbenchmark")
benchmark(lm(y ~ X),
          .lm.fit(cbind(1, X), y),
          lm.fit(cbind(1, X), y),
          qr.solve(cbind(1, X), y),
          order = "relative",
          replications = 1000)[, 1:4]
