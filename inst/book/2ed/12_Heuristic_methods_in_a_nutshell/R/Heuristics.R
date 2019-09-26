### R code from "Numerical Methods and Optimization in Finance"

###################################################
### code chunk number 1: chapter-settings
###################################################
Sys.setenv(LANGUAGE = "en")
library("NMOF")
library("PMwR")
library("parallel")
library("quadprog")
library("zoo")
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
### code chunk number 2: fast-ma-1
###################################################
N <- 1000  ## length of times series
K <- 50    ## order of moving average
y <- rnorm(N)
trials <- 100
###
library("rbenchmark")
###
MA_mean <- function(y, K) {
    N <- length(y)
    ans <- numeric(N)
    for(t in K:N)
        ans[t] <- mean(y[(t-K+1):t])
    ans
}
benchmark(MA_mean(y, K),
          replications = 500,
          order = "relative")[, 1:4]


###################################################
### code chunk number 3: fast-ma-2
###################################################
## variant 2 -- compute mean
MA_sum <- function(y, K) {
    N <- length(y)
    ans <- numeric(N)
    for(t in K:N)
        ans[t] <- sum(y[(t-K+1):t])/K
    ans
}
###
all.equal(MA_mean(y, K), MA_sum(y, K))
benchmark(MA_mean(y, K),
          MA_sum (y, K),
          replications = 500,
          order = "relative")[, 1:4]


###################################################
### code chunk number 4: fast-ma-3
###################################################
MA_mean_default <- function(y, K) {
    N <- length(y)
    ans <- numeric(N)
    for(t in K:N)
        ans[t] <- mean.default(y[(t-K+1):t])
    ans
}
all.equal(MA_mean(y, K), MA_mean_default(y, K))
benchmark(MA_mean(y, K),
          MA_sum (y, K),
          MA_mean_default(y, K),
          replications = 500,
          order = "relative")[, 1:4]


###################################################
### code chunk number 5: fast-ma-4
###################################################
MA_update <- function(y, K) {
    N <- length(y)
    ans <- numeric(N)
    ans[K] <- sum(y[1:K])/K
    for(t in (K+1):N)
        ans[t] <- ans[t-1] + y[t]/K - y[t-K]/K
    ans
}
all.equal(MA_mean(y, K), MA_mean_default(y, K))
benchmark(MA_mean(y, K),
          MA_sum (y, K),
          MA_mean_default(y, K),
          MA_update(y, K),
          replications = 500,
          order = "relative")[, 1:4]


###################################################
### code chunk number 6: fast-ma-5
###################################################
MA_filter <- function(y, K)
    filter(y, rep(1/K, K), sides = 1)
all.equal(MA_mean  (y, K)[K:length(y)],
          MA_filter(y, K)[K:length(y)])
benchmark(MA_mean(y, K),
          MA_sum (y, K),
          MA_mean_default(y, K),
          MA_update(y, K),
          MA_filter(y, K),
          replications = 500,
          order = "relative")[, 1:4]


###################################################
### code chunk number 7: fast-ma-6
###################################################
MA_cumsum <- function(y, K) {
    ans <- cumsum(y)/K
    ans[K:N] <- ans[K:N] - c(0, ans[1:(N-K)])
    ans
}
all.equal(MA_mean  (y, K)[K:length(y)],
          MA_cumsum(y, K)[K:length(y)])
benchmark(MA_mean(y, K),
          MA_sum (y, K),
          MA_mean_default(y, K),
          MA_update(y, K),
          MA_filter(y, K),
          MA_cumsum(y, K),
          replications = 500,
          order = "relative")[, 1:4]


###################################################
### code chunk number 8: fast-cor
###################################################
nc <- 200
x <- matrix(rnorm(2500*nc), ncol = nc)
benchmark(cor(x),
          crossprod(scale(x)),
          replications = 50,
          order = "relative")[, 1:4]
all.equal(cor(x),
          crossprod(scale(x))/(nrow(x)-1))


