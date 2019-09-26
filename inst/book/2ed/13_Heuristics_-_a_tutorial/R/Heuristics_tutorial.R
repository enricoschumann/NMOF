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
        useFancyQuotes = FALSE)
par.tutorial <- list(bty = "n",
                     las = 1,
                     mar = c(3, 3, 1, 1),
                     mgp = c(2, 0.5, 0),
                     tck = 0.01,
                     ps = 9)
col.greedy <- gray(0.5)
col.random <- gray(0.7)
pch.random <- 21
col.ls <- gray(.3)
col.ta <- gray(.1)


###################################################
### code chunk number 2: data
###################################################
n <- 100L
X <- runif(n)
s0 <- 2


###################################################
### code chunk number 3: seed
###################################################
set.seed(298359)


###################################################
### code chunk number 4: random-sol
###################################################
x <- runif(n) > 0.5
summary(x)


###################################################
### code chunk number 5
###################################################
abs(sum(X[x]) - s0)


###################################################
### code chunk number 6: OF1
###################################################
OF <- function(x, X, s0)
    abs(sum(X[x]) - s0)
###
OF(x, X, s0)


###################################################
### code chunk number 7
###################################################
sum(numeric(0L))


###################################################
### code chunk number 8
###################################################
x <- logical(n)
OF(x, X, s0)


###################################################
### code chunk number 9: Data
###################################################
Data <- list(X = runif(100), n = 100L, s0 = 2)


###################################################
### code chunk number 10
###################################################
OF <- function(x, Data)
    abs(sum(Data$X[x]) - Data$s0)


###################################################
### code chunk number 11
###################################################
OF(x, Data)


###################################################
### code chunk number 12
###################################################
## sample elements included in xTrue (assume n > 2)
true <- sort(sample(seq_len(Data$n),
                    sample(2:Data$n, 1)))
true


###################################################
### code chunk number 13: xTrue
###################################################
xTrue <- logical(Data$n)
xTrue[true] <- TRUE
###
## scale sum of xTrue to be exactly 2
Data$X[xTrue] <- Data$X[xTrue]/sum(Data$X[xTrue]) *
                 Data$s0


###################################################
### code chunk number 14: checks
###################################################
sort(which(xTrue))    ## should be the same as 'true'
sum(Data$X[xTrue])    ## should be 2
OF(xTrue, Data)       ## should be 0


###################################################
### code chunk number 15: choose
###################################################
noquote(sprintf("%-30.0f", choose(Data$n, 30)))


###################################################
### code chunk number 16: Rmpfr
###################################################
library("Rmpfr")
chooseMpfr(100, 30)


###################################################
### code chunk number 17
###################################################
chooseMpfr.all(n = Data$n)


###################################################
### code chunk number 18: ii
###################################################
ii <- c(1, seq(10, 100, by = 5))
ii


###################################################
### code chunk number 19
###################################################
## chooseMpfr.all(n = 100)[ii]


###################################################
### code chunk number 20: all-possibilities
###################################################
sum(chooseMpfr.all(n = 100))


###################################################
### code chunk number 21
###################################################
i <- which(cumsum(Data$X) > Data$s0)[1]
###
xConstr <- logical(Data$n)
xConstr[1:i] <- TRUE
OF(xConstr, Data)

xConstr[i] <- FALSE  ## slightly below 2
OF(xConstr, Data)


###################################################
### code chunk number 22
###################################################
ii <- order(Data$X)
i <- which(cumsum(Data$X[ii]) > Data$s0)[1]
###
xConstr <- logical(Data$n)
xConstr[ii[1:i]] <- TRUE
OF(xConstr, Data)
###
xConstr[ii[i]] <- FALSE ## slightly below 2
OF(xConstr, Data)


###################################################
### code chunk number 23
###################################################
z <- sample(5)
z


###################################################
### code chunk number 24
###################################################
randomSort <- function(x) {
    while (is.unsorted(x))
        x <- sample(x)
    x
}


###################################################
### code chunk number 25
###################################################
library("rbenchmark")
benchmark(randomSort(3:1))[, 1:3]
benchmark(randomSort(5:1))[, 1:3]
benchmark(randomSort(7:1))[, 1:3]


###################################################
### code chunk number 26: randomSol
###################################################
randomSol <- function(Data) {
    x <- logical(Data$n)
    k <- sample(Data$n, size = 1L)
    x[sample(Data$n, size = k)] <- TRUE
    x
}


###################################################
### code chunk number 27
###################################################
OF(randomSol(Data), Data)
OF(randomSol(Data), Data)
OF(randomSol(Data), Data)


###################################################
### code chunk number 28
###################################################
trials <- 1e6
OFvalues <- numeric(trials)
solutions <- vector("list", length = trials)
for (i in seq_len(trials)) {
    solutions[[i]] <- randomSol(Data)
    OFvalues[i] <- OF(solutions[[i]], Data)
}


###################################################
### code chunk number 29: best-random
###################################################
best100 <- order(OFvalues)[1:100]
random.OF <- OFvalues[best100]


###################################################
### code chunk number 30
###################################################
## ## An alternative random-sampling scheme:
## ## a best-of-n strategy
## trials <- 1e5
## nr <- 5  ## best-of-nr
## OFvalues <- numeric(trials)
## solutions <- vector("list", length = trials)
## tmp.OFvalues <- numeric(nr)
## tmp.solutions <- vector("list", length = nr)
## for (i in seq_len(trials)) {
##     for (j in seq_len(nr)) {
##         tmp.solutions[[j]] <- randomSol(Data)
##         tmp.OFvalues[j] <- OF(tmp.solutions[[j]], Data)
##     }
##     ii <- which.min(tmp.OFvalues)
##     solutions[[i]] <- tmp.solutions[[ii]]
##     OFvalues[i] <- tmp.OFvalues[[ii]]
## }


###################################################
### code chunk number 31: best-of-random1
###################################################
par(bty = "n", las = 1, mar = c(2, 2, 0, 0.2),
    ps = 8, tck = 0.01, mgp = c(1, 0.5, 0))
plot( ecdf(OFvalues[1:2000]), main = "", ylab = "", xlab = "",
     cex = 0.1, pch = 19, col = col.random, xlim = c(0,10))


###################################################
### code chunk number 32: best-of-random2
###################################################
par(bty = "n", las = 1, mar = c(2, 2, 0, 0.2),
    ps = 8, tck = 0.01, mgp = c(1,0.5,0))
plot( ecdf(random.OF), main = "", ylab = "", xlab = "",
     cex = 0.2, pch = 19, col = col.random, xlim = c(0,0.002))
abline(v = OF(xTrue, Data))


###################################################
### code chunk number 33: greedy
###################################################
greedy <- function(fun, x0, ..., maxit = 1000L) {
    done <- FALSE
    xbest <- xc <- x0
    xbestF <- xcF <- fun(xbest, ...)
    ic <- 0

    while (!done) {
        if (ic > maxit)
            break
        else
            ic <- ic + 1L

        done <- TRUE
        xc <- xbest
        for (i in seq_len(Data$n)) {
            xn <- xc
            xn[i] <- !xn[i]
            xnF <- fun(xn, ...)
            if (xnF < xbestF) {
                xbest <- xn
                xbestF <- xnF
                done <- FALSE
            }
        }
    }
    list(xbest = xbest, OFvalue = xbestF, ic = ic)
}


###################################################
### code chunk number 34: run-greedy
###################################################
x0 <- randomSol(Data)
result <- greedy(fun = OF, x0 = x0,
                 Data = Data, maxit = 1000L)
trials <- 100
greedy.ic <- greedy.OF <- numeric(trials)
greedy.solutions <- vector("list", length = trials)
###
for (i in seq_len(trials)) {
    g <- greedy(fun = OF,
                  x0 = randomSol(Data),
                  Data = Data,
                  maxit = 1000L)
    greedy.ic[i] <- g$ic
    greedy.OF[i] <- g$OFvalue
    greedy.solutions[[i]] <- g$xbest
}


###################################################
### code chunk number 35: xGreedy
###################################################
xGreedy <- greedy.solutions[[which.min(greedy.OF)]]
OF(xGreedy, Data)


###################################################
### code chunk number 36: greedy-steps
###################################################
summary(greedy.ic)


###################################################
### code chunk number 37: greedy1
###################################################
par(bty = "n", las = 1, mar = c(2, 2, 0, 0),
    ps = 8, tck = 0.01, mgp = c(1, 0.5, 0))
plot(ecdf(greedy.OF), main = "", ylab = "", xlab = "",
     cex = 0.4, pch = 19,
     xlim = c(0, 0.005), ylim = c(0,1),
     col = col.greedy)
lines(ecdf(random.OF), cex=0.4, col = col.random)
abline(v = OF(xTrue, Data))
legend(x = "bottomright",
       legend = c("Greedy search", "Random search"),
       col = c(col.greedy, col.random),
       pch = c(19, 19, 19), lty = c(1, 1, 1),
       y.intersp = 0.75, box.lty = 0)



###################################################
### code chunk number 38: N
###################################################
neighbour <- function(x, Data) {
    p <- sample.int(n = Data$n,
                    size = Data$stepsize)
    x[p] <- !x[p]
    x
}


###################################################
### code chunk number 39
###################################################
library("rbenchmark")
benchmark(sample(100, 5),
          sample.int(100, 5),
          replications = 20000, order = "relative")[, 1:4]


###################################################
### code chunk number 40: stepsize
###################################################
Data$stepsize <- 1L
summary(x)
summary(neighbour(x, Data))
Data$stepsize <- 2L
summary(neighbour(x, Data))


###################################################
### code chunk number 41: random-walk
###################################################
x <- randomSol(Data)
randomWalk <- numeric(1000L)
###
for (i in seq_along(randomWalk)) {
    x <- neighbour(x, Data)
    randomWalk[i] <- OF(x, Data)
}


###################################################
### code chunk number 42: random-walk-fig
###################################################
do.call(par, par.tutorial)
plot(1:length(randomWalk), randomWalk,
     type = "l",
     ylab = "Objective function value",
     xlab = "Iteration")


###################################################
### code chunk number 43: cor-rw
###################################################
cor(randomWalk[-1L],
    randomWalk[-length(randomWalk)])


###################################################
### code chunk number 44
###################################################
tmp <- numeric(1e2)
OFvalues <- list(n0 = tmp, n1 = tmp, n2 = tmp, n3 = tmp)
for (i in seq_along(tmp)) {
    x0 <- randomSol(Data)
    OFvalues[["n0"]][i] <- OF(x0, Data)
    Data$stepsize <- 1L
    OFvalues[["n1"]][i] <- OF(neighbour(x0, Data), Data)
    Data$stepsize <- 5L
    OFvalues[["n2"]][i] <- OF(neighbour(x0, Data), Data)
    Data$stepsize <- 10L
    OFvalues[["n3"]][i] <- OF(neighbour(x0, Data), Data)
}


###################################################
### code chunk number 45: stepsizes-dist
###################################################
do.call(par, par.tutorial)
plot(ecdf(abs(OFvalues[["n3"]] - OFvalues[["n0"]])),
     col = grey(0.7), pch = 22,
     main = "", ylab = "",
     xlab = "Objective function value")
lines(ecdf(abs(OFvalues[["n2"]] - OFvalues[["n0"]])),
      col = grey(0.5), pch = 22)
lines(ecdf(abs(OFvalues[["n1"]] - OFvalues[["n0"]])),
      col = grey(0.3), pch = 22)


###################################################
### code chunk number 46: stepsizes
###################################################
par(mfrow = c(1L, 3L), bty = "n", mar = c(4, 4, 0, 1),
    tck = 0.01, las = 1, ps = 9, mgp = c(1.5,.5,0))

prx <- pretty(unlist(OFvalues))
lims <- c(min(prx), max(prx))

plot(OFvalues[["n1"]], OFvalues[["n0"]],
     xlim = lims, ylim = lims,
     xlab = "OF value of x0 with 1 changed",
     ylab = "Objective function value of x0",
     col = grey(0.3), pch = 19, cex = 0.3, xaxt = "n", yaxt = "n",
     cex.lab = 1.2)
axis(1); axis(2)

par(mar = c(4,2,0,2))
plot(OFvalues[["n2"]], OFvalues[["n0"]],
     xlim = lims, ylim = lims, ylab = "",
     xlab = "OF value of x0 with 5 changed",
     col = grey(0.5), pch = 19, cex = 0.3, xaxt = "n", yaxt = "n",
     cex.lab = 1.2)
axis(1)

par(mar = c(4,1,0,4))
plot(OFvalues[["n3"]], OFvalues[["n0"]],
     xlim = lims, ylim = lims, ylab = "",
     xlab = "OF value of x0 with 10 changed",
     col = grey(0.7), pch = 19, cex = 0.3, xaxt = "n", yaxt = "n",
     cex.lab = 1.2)
axis(1)


###################################################
### code chunk number 47: no-structure
###################################################
par(mfrow = c(1L, 3L), bty = "n", mar = c(4, 4, 0, 1),
    tck = 0.01, las = 1, ps = 9, mgp = c(3,1,0.5))

prx <- pretty(unlist(OFvalues))
lims <- c(min(prx), max(prx))

plot.new()
plot(OFvalues[["n1"]], sample(OFvalues[["n0"]]),
     xlim = lims, ylim = lims,
     xlab = "OF value of neighbor to x0",
     ylab = "OF value of x0",
     col = grey(0.2), pch = 19, cex = 0.3, xaxt = "n",
     cex.lab = 1.2)
axis(1); axis(2)


###################################################
### code chunk number 48: no-structure-example1
###################################################
par(mar = c(0.1,0.1,0.1,0.1))
z <- array(0, dim = c(40,40))
z[22, 18] <- 10
par(cex.lab = 0.5)
persp(-z, zlim = c(-11,1), xlab = "", ylab = "",
      box = TRUE, phi = 30, theta = 20, lwd = 0.2,
      zlab = "Objective function value")


###################################################
### code chunk number 49: no-structure-example2
###################################################
par(mar=c(0.1,0.1,0.1,0.1))
par(cex.lab = 0.5)
z <- array(runif(30*30), dim = c(30,30))
persp(z, zlim = c(0,1.1), xlab = "", ylab = "",
      box = TRUE, phi = 30, theta = 20, lwd = 0.2,
      zlab = "Objective function value", cex = 0.5)


###################################################
### code chunk number 50: LSopt.
###################################################
LSopt. <- function(OF, algo = list(), ...) {
    xc  <- algo$x0
    xcF <- OF(xc, ...)
    for (s in seq_len(algo$nS)) {
        xn <- algo$neighbour(xc, ...)
        xnF <- OF(xn, ...)
        if (xnF <= xcF) {
            xc  <- xn
            xcF <- xnF
        }
    }
    list(xbest = xc, OFvalue = xcF)
}


###################################################
### code chunk number 51
###################################################
LSopt.(OF,
       list(x0 = randomSol(Data),
            neighbour = neighbour,
            nS = 50000),
       Data = Data)$OFvalue


###################################################
### code chunk number 52: run-LSopt
###################################################
library("NMOF")
x0 <- randomSol(Data)
algo <- list(x0 = x0,
             neighbour = neighbour,
             printBar = FALSE,
             nS = 50000)
sol1 <- LSopt(OF, algo, Data = Data)
sol2 <- LSopt(OF, algo, Data = Data)


###################################################
### code chunk number 53: Fmat
###################################################
dim(sol1$Fmat)


###################################################
### code chunk number 54: ls-over-time
###################################################
do.call(par, par.tutorial)
par(mar = c(3, 4, 1, 1))
plot(sol1$Fmat[, 2], type = "l", log = "y",
     ylab = "Objective function value",
     xlab = "Iteration",
     ylim = c(1e-6, 17))
lines(sol2$Fmat[, 2], type = "l",
     ylab = "Objective function value")


###################################################
### code chunk number 55: TAopt.
###################################################
TAopt. <- function(OF, algo = list(), ...) {
    xbest  <- xc  <- algo$x0
    xbestF <- xcF <- OF(xc, ...)
    for (t in seq_along(algo$vT)) {
        for (s in seq_len(algo$nS)) {
            xn <- algo$neighbour(xc, ...)
            xnF <- OF(xn, ...)
            if (xnF <= xcF + algo$vT[t]) {
                xc  <- xn
                xcF <- xnF
                if (xnF <= xbestF) {
                    xbest <- xn
                    xbestF <- xnF
                }
            }
        }
    }
    list(xbest = xc, OFvalue = xcF)
}


###################################################
### code chunk number 56: algo
###################################################
algo <- list(x0 = randomSol(Data),
             neighbour = neighbour,
             nS = 50000,
             vT = c(0.1, 0.02, 0),
             printBar = FALSE,
             printDetail = FALSE)


###################################################
### code chunk number 57
###################################################
TAopt(OF, algo = algo, Data = Data)$OFvalue


###################################################
### code chunk number 58: thresholds-only
###################################################
algo$vT <- NULL
algo$nT <- 10
algo$thresholds.only <- TRUE
thresholds <- TAopt(OF, algo = algo, Data = Data)
str(thresholds)
algo$vt <- thresholds$vt

algo$thresholds.only <- FALSE
(TAopt(OF, algo = algo, Data = Data)$OFvalue)


###################################################
### code chunk number 59: x0-list
###################################################
tmp <- randomSol(Data)
x0 <- list(x  = tmp,
           sx = sum(Data$X[tmp]))


###################################################
### code chunk number 60: OF2
###################################################
OF2 <- function(x, Data)
    abs(x$sx - Data$s0)
OF2(x0, Data)
OF(x0$x, Data)  ## check


###################################################
### code chunk number 61: neighbour2
###################################################
neighbour2 <- function(x, Data) {
    p <- sample.int(Data$n, size = Data$stepsize)
    x$x[p] <- !x$x[p]
    x$sx <- x$sx + sum(Data$X[p] * (2 * x$x[p] - 1))
    x
}


###################################################
### code chunk number 62: larger-size
###################################################
Data$n <- 10000L
Data$X <- rnorm(Data$n)


###################################################
### code chunk number 63: sol1-sol2
###################################################
set.seed(56447)
x0 <- randomSol(Data)
algo <- list(x0 = x0,
             printDetail = FALSE, printBar = FALSE,
             neighbour = neighbour)
sol1 <- TAopt(OF, algo = algo, Data = Data)
set.seed(56447)
tmp <- randomSol(Data)
x0 <- list(x = tmp, sx = sum(Data$X[tmp]))
algo <- list(x0 = x0,
             printDetail = FALSE, printBar = FALSE,
             neighbour = neighbour2)
sol2 <- TAopt(OF2, algo = algo, Data = Data)


###################################################
### code chunk number 64: test-OF
###################################################
OF( sol1$xbest, Data)
OF2(sol2$xbest, Data)


###################################################
### code chunk number 65: updating-speedup
###################################################
full_sum <- expression({
    x0 <- randomSol(Data)
    algo <- list(x0 = x0,
                 printDetail = FALSE, printBar = FALSE,
                 neighbour = neighbour)
    TAopt(OF, algo = algo, Data = Data)
})
###
updating <- expression({
    tmp <- randomSol(Data)
    x0 <- list(x = tmp, sx = sum(Data$X[tmp]))
    algo <- list(x0 = x0,
                 printDetail = FALSE, printBar = FALSE,
                 neighbour = neighbour2)
    TAopt(OF2, algo = algo, Data = Data)
})
###
library("rbenchmark")
benchmark(full_sum,
          updating,
          replications = 5, order = "relative")[, 1:4]


###################################################
### code chunk number 66: run-ls-ta
###################################################
cl <- makePSOCKcluster(10)  ## set number of cores
clusterSetRNGStream(cl, 42858)
##
algo <- list(neighbour = neighbour,
             x0 = randomSol(Data),
             nI = 200000,
             printBar = FALSE,
             printDetail = FALSE)
sols.LS <- restartOpt(LSopt, n = 100, OF,
                      algo = algo, Data = Data, cl = cl)
sols.TA <- restartOpt(TAopt, n = 100, OF,
                      algo = algo, Data = Data, cl = cl)
stopCluster(cl)


###################################################
### code chunk number 67: extract-ls-ta
###################################################
sols.LS <- sapply(sols.LS, `[[`, "OFvalue")
sols.TA <- sapply(sols.TA, `[[`, "OFvalue")


###################################################
### code chunk number 68: summaries
###################################################
summary(greedy.OF)
summary(random.OF)
summary(sols.LS)
summary(sols.TA)


###################################################
### code chunk number 69: greedy-restarts
###################################################
do.call(par, par.tutorial)
plot( ecdf(greedy.OF), main = "", ylab = "", xlab = "",
     cex = 0.4, pch = 19, col = col.greedy,
     xlim = c(0,0.002), ylim = c(0,1))
lines(ecdf(random.OF), col = col.random, cex=0.4)
lines(ecdf(sols.LS), col = col.ls, cex=0.4)
lines(ecdf(sols.TA), col = col.ta, cex=0.4)
legend(x = "bottomright",
       legend = c("Greedy search", "Random search",
                  "Local Search", "Threshold Accepting"),
       col = c(col.greedy, col.random, col.ls, col.ta),
       pch = c(19, 19, 19), lty = c(1, 1, 1, 1),
       y.intersp = 0.75, box.lty = 0)



###################################################
### code chunk number 70
###################################################
min(greedy.OF)
min(random.OF)
min(sols.LS)
min(sols.TA)


###################################################
### code chunk number 71
###################################################
## algo$nI <- 500000
## sols.TA <- restartOpt(TAopt, n = 50, OF,
##                       algo = algo, Data = Data, cl = 8)
## sols.TA <- sapply(sols.TA, `[[`, "OFvalue")
## min(sols.TA)


###################################################
### code chunk number 72: R-randomData
###################################################
randomData <-
    function(p = 200L,      ## number of available regressors
             n = 200L,      ## number of observations
             maxReg = 10L,  ## max. number of
                            ##      included regressors
             s = 1,         ## standard deviation of residuals
             constant = TRUE ) {

    X <- array(rnorm(n * p), dim = c(n, p))
    if (constant)
        X[, 1L] <- 1

    k <- sample.int(maxReg, 1L)   ## number of true regressors
    K <- sort(sample.int(p, k))   ## set of true regressors
    betatrue <- rnorm(k)          ## true coefficients

    ## the response variable y
    y <- X[, K] %*% as.matrix(betatrue) + rnorm(n, sd = s)

    list(X = X, y = y,
         betatrue = betatrue,
         K = K, n = n, p = p)
}


###################################################
### code chunk number 73: R-rD
###################################################
rD <- randomData(p = 100L, n = 200L, s = 1,
                 constant = TRUE, maxReg = 10L)


###################################################
### code chunk number 74: R-Data
###################################################
Data <- list(X = rD$X,
             y = rD$y,
             n = rD$n,
             p = rD$p,
             maxk  = 30L,  ## maximum number of regressors included in model
             lognn = log(rD$n)/rD$n)


###################################################
### code chunk number 75: R-random-solution
###################################################
x0 <- logical(Data$p)
temp <- sample.int(Data$maxk, 1L)
temp <- sample.int(Data$p, temp)
x0[temp] <- TRUE


###################################################
### code chunk number 76
###################################################
rD$K


###################################################
### code chunk number 77
###################################################
which(x0)


###################################################
### code chunk number 78: R-coefficients
###################################################
result1 <- lm(Data$y ~ -1 + Data$X[, x0])
result2 <- qr.solve(Data$X[, x0], Data$y)
result3 <- .lm.fit(Data$X[, x0, drop = FALSE], Data$y)
## ... coefficients should be the same
all.equal(as.numeric(coef(result1)), as.numeric(result2))
all.equal(as.numeric(coef(result1)), as.numeric(coef(result3)))


###################################################
### code chunk number 79: R-timing
###################################################
require("rbenchmark")
benchmark(.lm.fit(Data$X[, x0, drop = FALSE], Data$y),
          lm(Data$y ~ -1 + Data$X[, x0]),
          qr.solve(Data$X[ ,x0], Data$y),
          columns = c("test", "elapsed", "relative"),
          order = "relative",
          replications = 1000L)


###################################################
### code chunk number 80: R-OF
###################################################
OF <- function(x, Data) {
    e <- .lm.fit(Data$X[, x, drop = FALSE], Data$y)$residuals
    log(crossprod(e)/Data$n) + sum(x) * Data$lognn
}


###################################################
### code chunk number 81
###################################################
OF(x0, Data)


###################################################
### code chunk number 82: R-N
###################################################
neighbour <- function(xc, Data) {
    xn <- xc
    ex <- sample.int(Data$p, 1L)
    xn[ex] <- !xn[ex]
    sumx <- sum(xn)
    if (sumx < 1L || sumx > Data$maxk)
        xc
    else
        xn
}


###################################################
### code chunk number 83
###################################################
OF(neighbour(x0, Data), Data)
OF(neighbour(x0, Data), Data)
OF(neighbour(x0, Data), Data)


###################################################
### code chunk number 84
###################################################
algo <- list(nT = 10L,    ## number of thresholds
             nS = 200L,   ## number of steps per threshold
             nD = 1000L,  ## number of random steps to
                          ##   compute thresholds
             neighbour = neighbour,
             x0 = x0,
             printBar = FALSE)
sol1 <- TAopt(OF, algo = algo, Data = Data)


###################################################
### code chunk number 85
###################################################
sol1$OFvalue
which(sol1$xbest)  ## the selected regressors
rD$K               ## the true regressors


###################################################
### code chunk number 86
###################################################
xtrue <- logical(Data$p)
xtrue[rD$K] <- TRUE
OF(sol1$xbest, Data)
OF(xtrue, Data)


###################################################
### code chunk number 87: R-restarts
###################################################
restarts <- 100L
algo$printDetail <- FALSE
res <- restartOpt(TAopt,
                  n = restarts,
                  OF = OF,
                  algo = algo,
                  Data = Data,
                  cl = 4)


###################################################
### code chunk number 88: R-regression-restarts
###################################################
do.call(par, par.tutorial)
## extract solution quality and plot cdf
plot(ecdf(sapply(res, `[[`, "OFvalue")),
     cex = 0.4, main = "", ylab = "", xlab = "",
     verticals = TRUE)


###################################################
### code chunk number 89: R-overview
###################################################
## extract all solutions
xbestAll <- sapply(res, `[[`, "xbest")
## get included regressors
inclReg  <- which(rowSums(xbestAll) > 0L)
inclReg  <- sort(union(rD$K, inclReg))
data.frame(regressor  = inclReg,
           `included` = paste0(rowSums(xbestAll)[inclReg],
                               "/", restarts),
           `true regressor?` = inclReg %in% rD$K,
           check.names = FALSE)


###################################################
### code chunk number 90: P-fundData
###################################################
dim(fundData)
summary(apply(fundData, 2, sd)*sqrt(52))


###################################################
### code chunk number 91: P-Data
###################################################
Data <- list(
    R = t(fundData),
    na = dim(fundData)[2L],  ## number of assets
    ns = dim(fundData)[1L],  ## number of scenarios
    eps = 0.1/100,           ## stepsize
    wmin = 0.00,
    wmax = 0.05,
    resample = function(x, ...)
                   x[sample.int(length(x), ...)])


###################################################
### code chunk number 92: P-OF
###################################################
OF <- function(w, Data) {
    Rw <- crossprod(Data$R, w)
    crossprod(Rw)
}


###################################################
### code chunk number 93: P-N
###################################################
neighbour <- function(w, Data) {
    toSell <- w > Data$wmin
    toBuy  <- w < Data$wmax
    i <- Data$resample(which(toSell), size = 1L)
    j <- Data$resample(which(toBuy),  size = 1L)
    eps <- runif(1L) * Data$eps
    eps <- min(w[i] - Data$wmin,
               Data$wmax - w[j],
               eps)
    w[i] <- w[i] - eps
    w[j] <- w[j] + eps
    w
}


###################################################
### code chunk number 94: P-run-TAopt
###################################################
w0 <- runif(Data$na)  ## a random solution
w0 <- w0/sum(w0)
###
algo <- list(x0 = w0,
             neighbour = neighbour,
             nS = 5000L,
             nT = 10L,
             q = 0.10,
             printBar = FALSE)
res <- TAopt(OF,algo,Data)


###################################################
### code chunk number 95: P-check-constraints
###################################################
min(res$xbest) ## should not be smaller than Data$wmin
max(res$xbest) ## should not be greater than Data$wmax
sum(res$xbest) ## should be 1


###################################################
### code chunk number 96: exact-solution
###################################################
library("quadprog")
covMatrix <- crossprod(fundData)
A <- rep(1, Data$na)
a <- 1
B <- rbind(-diag(Data$na),
            diag(Data$na))
b <- rbind(array(-Data$wmax, dim = c(Data$na, 1L)),
           array( Data$wmin, dim = c(Data$na, 1L)))
result <- solve.QP(Dmat = covMatrix,
                    dvec = rep(0, Data$na),
                    Amat = t(rbind(A, B)),
                    bvec = rbind(a, b),
                    meq  = 1L)
wqp <- result$solution


###################################################
### code chunk number 97: P-OF-values
###################################################
c(100 * sqrt(crossprod(fundData %*% wqp)/Data$ns))       ## QP
c(100 * sqrt(crossprod(fundData %*% res$xbest)/Data$ns)) ## TA


###################################################
### code chunk number 98: P-psim
###################################################
psim <- function(x, y) { ## portfolio similarity

    stopifnot(length(x) == length(y))
    same.sign <- sign(x) == sign(y)

    list(same.assets = sum(same.sign),
         weight.overlap = sum(pmin(abs(x[same.sign]),
                                   abs(y[same.sign]))),
         max.abs.difference = max(abs(x-y)),
         mean.abs.difference = sum(abs(x-y))/length(x))

}
###
psim(res$xbest, wqp)


###################################################
### code chunk number 99: P-funs-updating
###################################################
OFU <- function(sol, Data)
    crossprod(sol$Rw)
###
neighbourU <- function(sol, Data){
    wn <- sol$w
    toSell <- wn > Data$wmin
    toBuy  <- wn < Data$wmax
    i <- Data$resample(which(toSell), size = 1L)
    j <- Data$resample(which(toBuy), size = 1L)
    eps <- runif(1) * Data$eps
    eps <- min(wn[i] - Data$wmin, Data$wmax - wn[j], eps)
    wn[i] <- wn[i] - eps
    wn[j] <- wn[j] + eps
    Rw <- sol$Rw + Data$R[, c(i,j)] %*% c(-eps,eps)
    list(w = wn, Rw = Rw)
}


###################################################
### code chunk number 100
###################################################
w0 <- runif(Data$na); w0 <- w0/sum(w0)  ## a random solution
Data$R <- fundData
sol <- list(w = w0, Rw = Data$R %*% w0)
algo <- list(x0 = sol,
              neighbour = neighbourU,
              nS = 2000L,
              nT = 10L,
              q = 0.10,
              printBar = FALSE,
              printDetail = FALSE)
res <- TAopt(OFU,algo,Data)


###################################################
### code chunk number 101
###################################################
wqp[200]


###################################################
### code chunk number 102
###################################################
fundData <- cbind(fundData, fundData[, 200L])
dim(fundData)
qr(fundData)$rank
qr(cov(fundData))$rank


###################################################
### code chunk number 103
###################################################
Data <- list(R = fundData,
              na = dim(fundData)[2L],
              ns = dim(fundData)[1L],
              eps = 0.5/100,
              wmin = 0.00, wmax = 0.05,
              resample = function(x, ...)
                            x[sample.int(length(x), ...)])

covMatrix <- crossprod(fundData)
A <- rep(1, Data$na)
a <- 1
B <- rbind(-diag(Data$na),
           diag(Data$na))
b <- rbind(array(-Data$wmax, dim = c(Data$na, 1L)),
           array( Data$wmin, dim = c(Data$na, 1L)))


###################################################
### code chunk number 104
###################################################
cat(try(result.QP <- solve.QP(Dmat = covMatrix,
                           dvec = rep(0, Data$na),
                           Amat = t(rbind(A,B)),
                           bvec = rbind(a,b),
                           meq = 1L)))


###################################################
### code chunk number 105
###################################################
w0 <- runif(Data$na); w0 <- w0/sum(w0)
x0 <- list(w = w0, Rw = fundData %*% w0)
algo <- list(x0 = x0,
             neighbour = neighbourU,
             nS = 2000L,
             nT = 10L,
             nD = 5000L,
             q = 0.20,
             printBar = FALSE,
             printDetail = FALSE)


###################################################
### code chunk number 106
###################################################
res2 <- TAopt(OFU, algo, Data)


###################################################
### code chunk number 107
###################################################
as.vector(100*sqrt(crossprod(fundData %*% res2$xbest$w)/Data$ns))


###################################################
### code chunk number 108
###################################################
res2$xbest$w[200:201]


###################################################
### code chunk number 109
###################################################
OF <- function(w, Data) { ## semi-variance
    Rw <- crossprod(Data$R, w) - Data$theta
    Rw <- Rw - abs(Rw)
    sum(Rw*Rw) / (4 * Data$ns)
}


###################################################
### code chunk number 110
###################################################
OF <- function(w, Data) { ## Omega
    Rw <- crossprod(Data$R, w) - Data$theta
    -sum(Rw - abs(Rw)) / sum(Rw + abs(Rw))
}


