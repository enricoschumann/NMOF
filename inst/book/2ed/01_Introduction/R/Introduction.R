### R code from "Numerical Methods and Optimization in Finance"

###################################################
### code chunk number 1: chapter-settings
###################################################
library("NMOF")
options(continue = "  ",
        digits = 3,
        width = 55,
        str = strOptions(strict.width = "cut"),
        useFancyQuotes = FALSE,
        warn = 2)
par.nmof <- list(bty = "n",
                 las = 1,
                 mar = c(3, 3, 1, 1),
                 mgp = c(2, 0.5, 0),
                 tck = 0.01,
                 ps = 9)


###################################################
### code chunk number 2: linear-systems
###################################################
m <- 100000  ## number of rows
n <- 10      ## number of columns
###
X <- array(rnorm(m * n), dim = c(m, n))
y <- rnorm(m)
###
## QR decomposition
fit_qr <- function(X, y)
    qr.solve(X, y)
###
## form (X'X) and (X'y)
fit_normal_eq <- function(X, y)
    solve(crossprod(X), crossprod(X,y))
###
## Cholesky
fit_cholesky <- function(X, y) {
    C <- chol(crossprod(X))
    rhs <- crossprod(X, y)
    backsolve(C, forwardsolve(t(C), rhs))
}
###
## check whether the solutions are the same
isTRUE(all.equal(
    fit_qr(X, y),
    c(fit_normal_eq(X, y)))) ## c() drops the dimension

## check whether the solutions are the same
isTRUE(all.equal(
    fit_normal_eq(X, y),
    fit_cholesky(X, y)))

## compare speed
library("rbenchmark")
benchmark(
    fit_qr(X, y),
    fit_normal_eq(X, y),
    fit_cholesky(X, y),
    order = "relative")[, 1:4]


###################################################
### code chunk number 3: true
###################################################
0.1 + 0.1 == 0.2


###################################################
### code chunk number 4: false
###################################################
0.1 + 0.1 + 0.1 == 0.3
