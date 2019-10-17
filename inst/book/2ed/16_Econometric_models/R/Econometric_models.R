### R code from "Numerical Methods and Optimization in Finance"

###################################################
### code chunk number 1: chapter-settings
###################################################
library("MASS")
library("NMOF")
library("PMwR")
options(continue = "  ",
        digits = 3,
        width = 55,
        str = strOptions(strict.width = "cut"),
        useFancyQuotes = FALSE)
par.nmof <- list(bty = "n",
                 las = 1,
                 mar = c(3, 3, 1, 1),
                 mgp = c(2, 0.5, 0),
                 tck = 0.02,
                 ps = 9)
set.seed(325323)


###################################################
### code chunk number 2: newton-ytm
###################################################
cf <- c(5, 5, 5, 5, 5, 105) ## cash flows
tm <- 1:6                   ## times to maturity
ytm_TRUE <- 0.046           ## the 'true' yield
###
b0 <- sum(cf/(1+ytm_TRUE)^tm)
cf <- c(-b0, cf)
tm <- c(0, tm)
###
r <- 0.1   ## initial guess for r
h <- 1e-6  ## finite-diff step-size
dr <- 1    ## change in r
while(abs(dr) > 1e-5) {
    g <- sum(cf/((1+r)^tm))
    dg <- ( sum(cf/((1+r+h)^tm)) - g ) / h
    dr <- g/dg
    print(r <- r - dr)
}


###################################################
### code chunk number 3: ytm
###################################################
ytm(cf, tm)


###################################################
### code chunk number 4: Matrix
###################################################
library("Matrix")
(M <- Matrix(c(1,0,0,0,0,1), 3, 2))
image(M)


###################################################
### code chunk number 5: ns-factors
###################################################
NSf


###################################################
### code chunk number 6: nss-factors
###################################################
NSSf


###################################################
### code chunk number 7: cor-ns
###################################################
cor(NSf(lambda = 6, tm = 1:10))


###################################################
### code chunk number 8: cor-nss
###################################################
cor(NSSf(lambda1 = 1, lambda2 = 5, tm = 1:10))


###################################################
### code chunk number 9: ns-fit
###################################################
tm <- 1:10
paramTRUE <- c(4, -2, 2, 1)
yM <- NS(paramTRUE, tm)
do.call(par, par.nmof)
plot(tm, yM,
     xlab = "maturities in years",
     ylab = "yields in %-points",
     type = "b")


###################################################
### code chunk number 10: ns-fit2
###################################################
lambda <- 1.5 ## fix lambda and run regression
result <- lm(yM ~ -1 + NSf(lambda,tm))
###
## compare results
do.call(par, par.nmof)
plot(yM - result$fitted.values,
     xlab = "maturities in years",
     ylab = "errors in %-points",
     type = "h")
abline(h = 0)


###################################################
### code chunk number 11
###################################################
lambda <- 1
betaTRUE <- c(4, -2, 2, lambda)
tm <- 1:10


###################################################
### code chunk number 12: ns-lm
###################################################
yM <- NS(betaTRUE, tm)
lm(yM ~ NSf(lambda, tm) - 1)


###################################################
### code chunk number 13: ns-fit-noise
###################################################
trials <- 1000
yM_lambda_10 <- yM_lambda_1 <-
    array(NA, dim = c(trials, 3))
colnames(yM_lambda_10) <- colnames(yM_lambda_1) <-
    c("beta_1", "beta_2", "beta_3")
###
lambda <- 1
betaTRUE <- c(4, -2, 2, lambda)
tm <- 1:10
for (t in seq_len(trials)) {
    yM <- NS(betaTRUE, tm) +
          rnorm(length(tm), sd = 0.01)
    yM_lambda_1[t, ] <-
        .lm.fit(NSf(lambda, tm), yM)$coefficients
}
###
lambda <- 10
betaTRUE <- c(4, -2, 2, lambda)
for (t in seq_len(trials)) {
    yM <- NS(betaTRUE, tm) +
          rnorm(length(tm), sd = 0.01)
    yM_lambda_10[t, ] <-
        .lm.fit(NSf(lambda, tm), yM)$coefficients
}


###################################################
### code chunk number 14: fig-ns-fit-noise (eval = FALSE)
###################################################
## ## chunkname: fig-ns-fit-noise
## for (i in 1:3) {
##     plot(ecdf(yM_lambda_10[, i]),
##          xlab = "",
##          ylab = "",
##          main = substitute(beta[i], list(i = i)))
##     abline(v = betaTRUE[i], col = grey(0.6))
##     lines(ecdf(yM_lambda_1[, i]),
##           xlab = "",
##           ylab = "",
##           main = substitute(beta[i], list(i = i)),
##           col = grey(0.4))
## }


###################################################
### code chunk number 15: ns-estimates
###################################################
do.call(par, par.nmof)
par(mfrow = c(1, 3),
    mar = c(2, 2, 1, 1),
    ps = 10,
    mgp = c(3, 0.2, 0))
for (i in 1:3) {
    plot(ecdf(yM_lambda_10[, i]),
         xlab = "",
         ylab = "",
         main = substitute(beta[i], list(i = i)))
    abline(v = betaTRUE[i], col = grey(0.6))
    lines(ecdf(yM_lambda_1[, i]),
          xlab = "",
          ylab = "",
          main = substitute(beta[i], list(i = i)),
          col = grey(0.4))
}


###################################################
### code chunk number 16: negative-rates (eval = FALSE)
###################################################
## ## chunkname: negative-rates
## tm <- seq(1, 10, length.out = 30)  ## 1 to 10 years
## paramTRUE <- c(3, -2, -8, 1.5)     ## 'true' parameters
## yM <- NS(paramTRUE, tm)
## do.call(par, par.nmof)
## plot(tm, yM,
##      xlab = "maturities in years",
##      ylab = "yields in percent")
## abline(h = 0)


###################################################
### code chunk number 17: negative-rates-fig
###################################################
tm <- seq(1, 10, length.out = 30)  ## 1 to 10 years
paramTRUE <- c(3, -2, -8, 1.5)     ## 'true' parameters
yM <- NS(paramTRUE, tm)
do.call(par, par.nmof)
plot(tm, yM,
     xlab = "maturities in years",
     ylab = "yields in percent")
abline(h = 0)


###################################################
### code chunk number 18: DEopt.
###################################################
DEopt. <- function (OF, algo = list(), ...) {

    d <- length(algo$max)
    nP <- algo$nP
    vFv <- vF <- rep(NA, nP)
    shift <- c(nP, seq(from = 1, to = nP - 1))

    mP <- runif(d*nP) * (algo$max - algo$min) + algo$min
    dim(mP) <- c(d, nP)

    for (s in seq_len(nP))
        vF[s] <- OF(mP[, s], ...)
    for (g in seq_len(algo$nG)) {
        vI <- sample.int(nP)
        R1 <- vI[shift]
        R2 <- R1[shift]
        R3 <- R2[shift]
        mPv <- mP[, R1] + algo$F * (mP[, R2] - mP[, R3])
        mI <- runif(d * nP) > algo$CR
        mPv[mI] <- mP[mI]
        for (s in seq_len(nP))
            vFv[s] <- OF(mPv[, s], ...)
        is.better <- vFv < vF
        mP[, is.better] <- mPv[, is.better]
        vF[is.better] <- vFv[is.better]
    }
    list(xbest = mP[, which.min(vF)[1L]],
         OFvalue = min(vF))
}


###################################################
### code chunk number 19
###################################################
## DEopt(OF, algo, ...)


###################################################
### code chunk number 20
###################################################
## DEopt(OF, algo, Data)


###################################################
### code chunk number 21: recycle
###################################################
min <- 1:3
max <- 5:7
nP <- 6   ## population size
d <- length(min)
M <- (max - min)*runif(d*nP) + min
dim(M) <- c(d, nP)
M


###################################################
### code chunk number 22: shift
###################################################
shift <- function(x) {
    n <- length(x)
    c(x[n], x[seq_len(n - 1)])
}
shift(1:5)
shift(shift(1:5))


###################################################
### code chunk number 23
###################################################
## for (s in seq_len(nP))
##     vFv[s] <- OF(mPv[, s], ...)


###################################################
### code chunk number 24
###################################################
## vFv <- apply(mPv, 2, OF, ...)


###################################################
### code chunk number 25
###################################################
## if (algo$loopOF) {
##     for (s in 1:nP)
##         vF[s] <- OF(mPv[, s], ...)
## } else {
##     vF <- OF(mPv, ...)
## }


###################################################
### code chunk number 26
###################################################
## OF(solution, ...)


###################################################
### code chunk number 27: vignette (eval = FALSE)
###################################################
## ## chunkname: vignette
## ## to open a list of vignettes in the browser, say
## ##    browseVignettes("NMOF")
## ###
## vignette("vectorise", package = "NMOF")


###################################################
### code chunk number 28: OF
###################################################
OF <- function(param, Data) {
    y <- Data$model(param, Data$tm)
    maxdiff <- y - Data$yM
    maxdiff <- max(abs(maxdiff))
    if (is.na(maxdiff))
        maxdiff <- 1e10
    maxdiff
}


###################################################
### code chunk number 29: Data
###################################################
Data <- list(yM = yM,
             tm = tm,
             model = NS,
             pen.w = 0.1,
             min   = c( 0,-15,-30,  0),
             max   = c(15, 30, 30, 10))


###################################################
### code chunk number 30: OF-pen
###################################################
OF.pen <- function(param, Data) {
    y   <- Data$model(param, Data$tm)
    res <- max(abs(y - Data$yM))
###
    ## compute the penalty
    aux <- y - abs(y)  ## aux is zero for positive y
    aux <- -sum(aux) * Data$pen.w
    res + aux
}


###################################################
### code chunk number 31: penalty1 (eval = FALSE)
###################################################
## ## chunkname: penalty1
## penalty1 <- function(param, Data) {
##     y <- Data$model(param, Data$tm)
##     neg.y <- abs(y - abs(y)) ## equiv. to '-2*pmin(y, 0)'
##     sum(neg.y) * Data$pen.w
## }


###################################################
### code chunk number 32
###################################################
## fun(solution, ...)


###################################################
### code chunk number 33
###################################################
## algo$loopRepair <- FALSE
## algo$loopPen <- FALSE


###################################################
### code chunk number 34: penalty-vectorized
###################################################
penalty <- function(mP, Data) {
    minV <- Data$min
    maxV <- Data$max
    pen.w <- Data$pen.w

    ## if larger than maxV, element in A is positiv
    A <- mP - as.vector(maxV)
    A <- A + abs(A)

    ## if smaller than minV, element in B is positiv
    B <- as.vector(minV) - mP
    B <- B + abs(B)

    ## beta 1 + beta2 > 0
    C <- pen.w * ((mP[1, ] + mP[2, ]) -
               abs(mP[1, ] + mP[2, ]))
    A <- pen.w * colSums(A + B) - C
    A
}


###################################################
### code chunk number 35: penalty-example
###################################################
param1 <- c( 6, 3, 8, -1) ## invalid: lambda1 < 0
param2 <- c( 6, 3, 8,  1)
param3 <- c(-1, 3, 8,  1) ## invalid: beta1 < 0
P <- cbind(param1, param2, param3)
rownames(P) <- c("beta1","beta2","beta3","lambda1")
P


###################################################
### code chunk number 36
###################################################
penalty(P, Data)


###################################################
### code chunk number 37: pen.w
###################################################
Data$pen.w <- 0.5
penalty(P, Data)


###################################################
### code chunk number 38: valid-P
###################################################
param1 <- c( 5, 3, 8,  1)  ## three valid solutions
param2 <- c( 6, 3, 8,  1)
param3 <- c( 7, 3, 8,  1)
P <- cbind(param1, param2, param3)
rownames(P) <- c("beta1", "beta2", "beta3", "lambda1")
penalty(P, Data)


###################################################
### code chunk number 39: de-ns
###################################################
tm <- c(c(1, 3, 6, 9)/12, 1:10)
betaTRUE <- c(6, 3, 8, 1)
yM   <- NS(betaTRUE, tm)
###
OF <- function(param, Data) {
    y <- Data$model(param, Data$tm)
    maxdiff <- y - Data$yM
    maxdiff <- max(abs(maxdiff))
    if (is.na(maxdiff))
        maxdiff <- 1e10
    maxdiff
}
###
Data <- list(yM    = yM,
             tm    = tm,
             model = NS,
             pen.w = 0.1,
             min   = c( 0,-15,-30,  0),
             max   = c(15, 30, 30, 10))
###
algo <- list(
    nP = 100L,
    nG = 500L,
    F = 0.50,
    CR = 0.99,
    min = c( 0,-15,-30, 0),
    max = c(15, 30, 30,10),
    pen = penalty,
    repair = NULL,
    loopOF = TRUE,
    loopPen = FALSE,
    loopRepair = TRUE,
    printBar = FALSE)
###
system.time(DEopt(OF = OF, algo = algo, Data = Data))


###################################################
### code chunk number 40: ns-example (eval = FALSE)
###################################################
## ## chunkname: ns-example
## n.runs <- 5
## do.call(par, par.nmof)
## plot(tm, yM,
##      xlab = "maturities in years",
##      ylab = "yields in %")
## algo$printDetail <- FALSE
## for (i in seq_len(n.runs)) {
##     sol <- DEopt(OF = OF, algo = algo, Data = Data)
##     lines(tm, Data$model(sol$xbest, tm), col = grey(0.3))
##     s0 <- algo$min +
##         (algo$max - algo$min) * runif(length(algo$min))
##     sol2 <- nlminb(s0, OF, Data = Data,
##                    lower = Data$min,
##                    upper = Data$max,
##                    control = list(eval.max = 50000L,
##                                   iter.max = 50000L))
##     lines(tm, Data$model(sol2$par, tm),
##           col = grey(0.5), lty = 3)
## }
## legend(x = "topright",
##        legend = c("true yields", "DEopt", "nlminb"),
##        col = c("black", grey(0.3), grey(0.5)),
##        pch = c(1, NA, NA), lty = c(0, 1, 3),
##        y.intersp = 0.75, box.lty = 0)


###################################################
### code chunk number 41: ns-ex1
###################################################
n.runs <- 5
do.call(par, par.nmof)
plot(tm, yM,
     xlab = "maturities in years",
     ylab = "yields in %")
algo$printDetail <- FALSE
for (i in seq_len(n.runs)) {
    sol <- DEopt(OF = OF, algo = algo, Data = Data)
    lines(tm, Data$model(sol$xbest, tm), col = grey(0.3))
    s0 <- algo$min +
        (algo$max - algo$min) * runif(length(algo$min))
    sol2 <- nlminb(s0, OF, Data = Data,
                   lower = Data$min,
                   upper = Data$max,
                   control = list(eval.max = 50000L,
                                  iter.max = 50000L))
    lines(tm, Data$model(sol2$par, tm),
          col = grey(0.5), lty = 3)
}
legend(x = "topright",
       legend = c("true yields", "DEopt", "nlminb"),
       col = c("black", grey(0.3), grey(0.5)),
       pch = c(1, NA, NA), lty = c(0, 1, 3),
       y.intersp = 0.75, box.lty = 0)


###################################################
### code chunk number 42: checks
###################################################
sol <- DEopt(OF = OF, algo = algo, Data = Data)
###
## max. error and objective function value of solution
## ==> should be the same
all.equal(max(abs(Data$model(sol$xbest, Data$tm) -
          Data$model(betaTRUE, Data$tm))),
          sol$OFvalue)

## test: nlminb with random starting value s0
s0 <- algo$min + (algo$max - algo$min) * runif(length(algo$min))
sol2 <- nlminb(s0, OF, Data = Data,
               lower = Data$min,
               upper = Data$max,
               control = list(eval.max = 50000L,
                              iter.max = 50000L))
###
## max. error and objective function value of solution
## ==> should be the same
max(abs(Data$model(sol2$par, tm) -
        Data$model(betaTRUE, tm)))
sol2$objective


###################################################
### code chunk number 43: nss-example
###################################################
## set up yield curve (here: artificial data), and plot it
tm <- c(c(1, 3, 6, 9)/12, 1:10)
betaTRUE <- c(5, -2, 5, -5, 1, 6)
yM <- NSS(betaTRUE, tm)
###
Data <- list(## collect everything in Data
    yM = yM,
    tm = tm,
    model = NSS,
    min = c( 0,-15,-30,-30,  0,  5),
    max = c(15, 30, 30, 30,  5, 10),
    pen.w = 1)
###
algo <- list(
    nP = 100L,
    nG = 500L,
    F = 0.50,
    CR = 0.99,
    min = c( 0,-15,-30,-30, 0, 5),
    max = c(15, 30, 30, 30, 5, 10),
    pen = penalty,
    repair = NULL,
    loopOF = TRUE,
    loopPen = FALSE,
    loopRepair = TRUE,
    printBar = FALSE,
    printDetail = TRUE)
###
sol.DE <- DEopt(OF = OF, algo = algo, Data = Data)


###################################################
### code chunk number 44: ns-ex2
###################################################
n.runs <- 5
do.call(par, par.nmof)
plot(tm, yM,
     xlab = "maturities in years",
     ylab = "yields in %")
algo$printDetail <- FALSE
for (i in seq_len(n.runs)) {
    sol <- DEopt(OF = OF, algo = algo, Data = Data)
    lines(tm, Data$model(sol$xbest, tm), col = grey(0.3))
    s0 <- algo$min +
         (algo$max - algo$min) * runif(length(algo$min))
    sol2 <- nlminb(s0, OF, Data = Data,
                   lower = Data$min,
                   upper = Data$max,
                   control = list(eval.max = 50000L,
                                  iter.max = 50000L))
    lines(tm, Data$model(sol2$par, tm),
          col = grey(0.5), lty = 3)
}
###
legend(x = "topright",
       legend = c("true yields", "DEopt", "nlminb"),
       col = c("black", grey(0.3), grey(0.5)),
       pch = c(1, NA, NA), lty = c(0, 1, 3),
       y.intersp = 0.75, box.lty = 0)


###################################################
### code chunk number 45: time
###################################################
cf <- c(4.25, 4.25, 104.25)
mats <- c("2010-10-12", "2011-10-12", "2012-10-12")
###
## compute time to maturity in years
today <- as.Date("2010-05-31")
tm <- as.numeric((as.Date(mats) - today))/365


###################################################
### code chunk number 46: cf-matrix
###################################################
cfList <- bundData$cfList
tmList <- bundData$tmList
mats   <- unlist(tmList, use.names = FALSE)
mats   <- sort(unique(mats))
ISIN   <- names(bundData$cfList)
###
## set up cash flow matrix
nR <- length(mats)
nC <- length(cfList)
cfMatrix <- array( 0, dim = c(nR, nC) )
for (j in seq(nC))
    cfMatrix[mats %in% tmList[[j]], j] <- cfList[[j]]
rownames(cfMatrix) <- mats
colnames(cfMatrix) <- ISIN


###################################################
### code chunk number 47
###################################################
head(cfMatrix[, 1:3], 5)


###################################################
### code chunk number 48
###################################################
## df <- 1 / ((1 + y)^tm)


###################################################
### code chunk number 49
###################################################
## b <- df %*% cfMatrix


###################################################
### code chunk number 50: bond-example
###################################################
## reprice bonds with known yield curve
today <- as.Date("2010-05-31")
tm <- as.numeric((as.Date(mats) - today))/365
betaTRUE <- c(5, -2, 1, 10, 1, 5)
yM <- NSS(betaTRUE, tm)
df <- 1 / ((1 + yM/100)^tm)
bM <- df %*% cfMatrix
###
OF2 <- function(param, Data) {
    tm  <- Data$tm
    bM  <- Data$bM
    model <- Data$model
    cfMatrix <- Data$cfMatrix
    df <- 1 / ( (1 + model(param, tm)/100)^tm )
    b <- df %*% cfMatrix
    aux <- max(abs(b - bM))
    if (is.na(aux))
        1e5
    else
        aux
}
###
## collect all data in 'Data'
Data <- list(
    bM = bM,
    tm = tm,
    cfMatrix = cfMatrix,
    model = NSS,
    pen.w = 1,
    min = c( 0,-15,-30,-30,0,3),
    max = c(15, 30, 30, 30,3,6))
###
## list of parameters for DEopt
algo <- list(
    nP = 100,
    nG = 600,
    F = 0.5,
    CR = 0.9,
    min = c( 0,-15,-30,-30,0,3),
    max = c(15, 30, 30, 30,3,6),
    pen = penalty,
    repair = NULL,
    loopOF = TRUE,
    loopPen = FALSE,
    loopRepair = FALSE,
    printBar = FALSE)

system.time(sol <- DEopt(OF = OF2, algo = algo, Data = Data))
## maximum yield error
max(abs(Data$model(sol$xbest, tm) -
        Data$model(betaTRUE, tm)))
###
## max. abs. price error and obj. function
## ==> should be the same
df <- 1 / ((1 + NSS(sol$xbest,tm)/100)^tm)
b <- df %*% cfMatrix
max(abs(b - bM))
sol$OFvalue


###################################################
### code chunk number 51: ns-ex3
###################################################
n.runs <- 5
do.call(par, par.nmof)
plot(tm, yM,
     xlab = "maturities in years",
     ylab = "yields in %")
###
algo$printDetail <- FALSE
for (i in seq_len(n.runs)) {
    sol <- DEopt(OF = OF2, algo = algo, Data = Data)
    lines(tm, Data$model(sol$xbest, tm), col = grey(0.3))
    s0 <- algo$min +
         (algo$max - algo$min) * runif(length(algo$min))
    sol2 <- nlminb(s0, OF2, Data = Data,
                   lower = Data$min,
                   upper = Data$max,
                   control = list(eval.max = 50000L,
                                  iter.max = 50000L))
    lines(tm, Data$model(sol2$par, tm),
          col = grey(0.5), lty = 3)
}
###
legend(x = "bottomright",
       legend = c("true yields", "DEopt", "nlminb"),
       col = c("black", grey(0.3), grey(0.5)),
       pch = c(1, NA, NA), lty = c(0, 1, 3),
       y.intersp = 0.75, box.lty = 0)


###################################################
### code chunk number 52
###################################################
s0 <- algo$min + (algo$max - algo$min) *
    runif(length(algo$min))
system.time(sol2 <- nlminb(s0, OF2, Data = Data,
                           lower = Data$min,
                           upper = Data$max,
                           control = list(eval.max = 50000,
                                          iter.max = 50000)))
## maximum yield error
max(abs(Data$model(sol2$par, tm) -
        Data$model(betaTRUE, tm)))
###
## max. abs. price error and obj. function
## ==> should be the same
df <- 1 / ((1 + NSS(sol2$par, tm)/100)^tm)
b <- df %*% cfMatrix
max(abs(b - bM))
sol2$objective


###################################################
### code chunk number 53: errors
###################################################
## plot rate error against ttm of payment
do.call(par, par.nmof)
par(mar = c(3, 8, 1, 1),
    mgp = c(2.5, 0.5, 0))
plot(tm, NSS(sol$xbest, tm) - NSS(betaTRUE, tm),
     ylab = "Errors in rates",
     xlab = "Time to maturity")


###################################################
### code chunk number 54: errors2
###################################################
## plot price error against tm of bond
do.call(par, par.nmof)
par(mar = c(3, 7, 1, 1))
plot(c((as.Date(sapply(tmList, max)) - today)/365),
     c(b - bM),
     ylab = "Errors in prices",
     xlab = "Time to maturity")


###################################################
### code chunk number 55: yield
###################################################
compYield <- function(cf, tm, guess = 0.05) {
    fy <- function(ytm, cf, tm) sum(cf / ((1 + ytm)^tm))
    non.zero <- cf != 0
    cf <- cf[non.zero]
    tm <- tm[non.zero]
    ytm <- guess
    h <- 1e-8
    dF <- 1
    ci <- 0L
    while (abs(dF) > 1e-5) {
        ci <- ci + 1L
        if (ci > 5L)
            break
        FF <- fy(ytm, cf, tm)
        dFF <- (fy(ytm + h, cf, tm) - FF)/h
        dF <- FF/dFF
        ytm <- ytm - dF
    }
    ytm
}


###################################################
### code chunk number 56: OFyield
###################################################
OFyield <- function(param, Data) {
    tm <- Data$tm
    rM <- Data$rM
    model <- Data$model
    cfMatrix <- Data$cfMatrix
    nB <- dim(cfMatrix)[2L]
    zrates <- model(param, tm)
    aux <- 1e8
    df <- 1/((1 + zrates/100)^tm)
    b <- df %*% cfMatrix
    r <- numeric(nB)
    if (all(!is.na(b), df < 1, df > 0, b > 1)) {
        for (bb in seq_len(nB)) {
            if (bb == 1L)
                guess <- 0.05
            else
                guess <- r[bb - 1L]
            r[bb] <- compYield(c(-b[bb], cfMatrix[, bb]),
                               c(0, tm), guess)
        }
        aux <- abs(r - rM)
        aux <- sum(aux)
    }
    aux
}


###################################################
### code chunk number 57: yield-example
###################################################
cfList <- bundData$cfList
tmList <- bundData$tmList
mats   <- unlist(tmList, use.names = FALSE)
mats   <- sort(unique(mats))
ISIN   <- names(bundData[[1]])

## set up cash flow matrix
nR <- length(mats)
nC <- length(cfList)
cfMatrix <- array(0, dim = c(nR, nC))
for(j in seq(nC))
    cfMatrix[mats %in% tmList[[j]], j] <- cfList[[j]]
rownames(cfMatrix) <- mats
colnames(cfMatrix) <- ISIN
###
## compute artificial market prices
today <- as.Date("2010-05-31")
tm <- as.numeric((as.Date(mats) - today))/365
betaTRUE <- c(5,-2,1,10,1,3); yM <- NSS(betaTRUE, tm)
df <- 1 / ((1 + yM/100)^tm)
bM <- df %*% cfMatrix
rM <- apply(rbind(-bM, cfMatrix), 2, compYield, c(0, tm))
###
## collect all in dataList
Data <- list(
    rM = rM,
    tm = tm,
    cfMatrix = cfMatrix,
    model = NSS,
    min = c( 0, -15, -30, -30, 0  , 2.5),
    max = c(15,  30,  30,  30, 2.5, 5),
    pen.w = 0.1
)
###
## set parameters for de
algo <- list(
    nP = 50L,
    nG = 500L,
    F = 0.50,
    CR = 0.99,
    min = c( 0, -15, -30, -30, 0  , 2.5),
    max = c(15,  30,  30,  30, 2.5, 5),
    pen = penalty, repair = NULL,
    loopOF = TRUE, loopPen = FALSE,
    loopRepair = FALSE,
    printDetail = TRUE,
    printBar = FALSE)
###
system.time(sol <- DEopt(OF = OFyield,
                         algo = algo,
                         Data = Data))


###################################################
### code chunk number 58: ns-ex4
###################################################
n.runs <- 5
do.call(par, par.nmof)
plot(tm, yM,
     xlab = "maturities in years",
     ylab = "yields in %")
###
algo$printDetail <- FALSE
for (i in seq_len(n.runs)) {
    sol <- DEopt(OF = OFyield, algo = algo, Data = Data)
    lines(tm, Data$model(sol$xbest, tm), col = grey(0.3))
    s0 <- algo$min +
         (algo$max - algo$min) * runif(length(algo$min))
    sol2 <- nlminb(s0, OF2, Data = Data,
                   lower = Data$min,
                   upper = Data$max,
                   control = list(eval.max = 50000L,
                                  iter.max = 50000L))
    lines(tm, Data$model(sol2$par, tm),
          col = grey(0.5), lty = 3)
}
###
legend(x = "bottomright",
       legend = c("true yields", "DEopt", "nlminb"),
       col = c("black", grey(0.3), grey(0.5)),
       pch = c(1, NA, NA), lty = c(0, 1, 3),
       y.intersp = 0.75, box.lty = 0)


###################################################
### code chunk number 59: ns-ex4-errors
###################################################
## maximum error DEopt
max(abs(Data$model(sol$xbest, tm) - Data$model(betaTRUE, tm)))
## maximum abs. yield error and objective function DEopt
df <- 1 / ((1 + NSS(sol$xbest, tm)/100)^tm)
b <- df %*% cfMatrix
r <- apply(rbind(-b, cfMatrix), 2, compYield, c(0, tm))
sum(abs(r - rM))
sol$OFvalue
###
## nlminb
s0 <- algo$min + (algo$max - algo$min) *
    runif(length(algo$min))
system.time(sol2 <- nlminb(s0, OFyield, Data = Data,
                           lower = algo$min,
                           upper = algo$max,
                           control = list(eval.max = 50000L,
                                          iter.max = 50000L)))
## maximum error nlminb
max(abs(Data$model(sol2$par, tm) - Data$model(betaTRUE, tm)))
###
## maximum abs. yield error and objective function DEopt
df <- 1 / ((1 + NSS(sol2$par,tm)/100)^tm)
b <- df %*% cfMatrix
r <- apply(rbind(-b, cfMatrix), 2, compYield,c(0, tm))
sum(abs(r - rM))
sol2$objective


###################################################
### code chunk number 61
###################################################
## plot (apply(sol$Fmat, 1, median), type = "l")
## lines(apply(sol$Fmat, 1, min), type = "l", lty = 3)
## lines(apply(sol$Fmat, 1, max), type = "l", lty = 3)


###################################################
### code chunk number 64: tangency
###################################################
## create artifical data ('daily returns')
n  <- 100     # number of observations
p  <- 10      # number of assets
X  <- array(rnorm(n * p, mean = 0.001, sd = 0.01),
           dim = c(n, p))
rf <- 0.0001  # riskfree rate (2.5% pa)
m  <- apply(X, 2, mean)  # means
m2   <- m - rf           # excess means
###
## (1) solve the problem with qp
library("quadprog")
aMat  <- as.matrix(m2); bVec  <- 1
zeros <- array(0, dim = c(p,1))
solQP <- solve.QP(cov(X), zeros, aMat, bVec, meq = 1)
# rescale variables to obtain weights
w     <- solQP$solution/sum(solQP$solution)
# compute sharpe ratio
SR    <- t(w) %*% m2 / sqrt(t(w) %*% cov(X) %*% w)
###
## (2) solve with regression
X2     <- X - rf # excess returns
ones  <- array(1, dim = c(n,1))
# run regression
solR  <- lm(ones~-1 + X2)
# rescale variables to obtain weights
w2    <- coef(solR)
w2    <- w2/sum(w2)
###
## (3) solve first-order conditions
w3 <- solve(cov(X),m2)
# rescale
w3 <- w3/sum(w3)
###
## check they are the same
all.equal(as.vector(w),as.vector(w2))
all.equal(as.vector(w),as.vector(w3))
all.equal(as.vector(w2),as.vector(w3))


###################################################
### code chunk number 65: minimum-var
###################################################
## create artificial data with mean 0 and sd 5%
n <- 100 ## number of observations
p <- 10  ## number of assets
X <- array(rnorm(n * p, mean = 0, sd = 0.05),
           dim = c(n, p))
###
## (1) solve with QP
library("quadprog")
aMat  <- array(1, dim = c(1, p))
bVec  <- 1
zeros <- array(0, dim = c(p, 1))
solQP <- solve.QP(cov(X), zeros, t(aMat), bVec, meq = 1)
##     ... and check solution
all.equal(as.numeric(var(X %*% solQP$solution)),
          as.numeric(2 * solQP$value))

## (2) regression
y  <- X[, 1]           ## choose 1st asset as regressand
X2 <- X[, 1] - X[, 2:p] ## choose 1st asset as regressand
solR <- lm(y ~ X2)
## compare results of regression with qp
## ___ weights from qp
as.vector(solQP$solution)
## ___ weights from regression
as.vector(c(1 - sum(coef(solR)[-1]), coef(solR)[-1]))
## variance of portfolio
all.equal(as.numeric(var(X %*% solQP$solution)),
          var(solR$residuals))

## (3) solve first-order conditons
x <- solve(cov(X), numeric(p) + 1) ## or any other constant != 0
## rescale
x <- x/sum(x)
## compare results with QP solution
all.equal(solQP$solution, x)


###################################################
### code chunk number 66: PSopt.
###################################################
PSopt. <- function (OF, algo = list(), ...) {
    mRU <- function(m, n)
        array(runif(m * n), dim = c(m, n))
    mRN <- function(m, n)
        array(rnorm(m * n), dim = c(m, n))
    d <- length(algo$max)
    vF <- numeric(algo$nP)
    vF[] <- NA
    mP <- algo$min +
        diag(algo$max - algo$min) %*% mRU(d, algo$nP)
    mV <- algo$initV * mRN(d,algo$nP)
    for (s in 1:algo$nP)
        vF[s] <- OF(mP[, s], ...)
    mPbest <- mP
    vFbest <- vF
    sGbest <- min(vFbest)
    sgbest <- which.min(vFbest)[1]
    for (g in 1:algo$nG) {
        mDV <- algo$c1*mRU(d, algo$nP) * (mPbest - mP) +
               algo$c2*mRU(d, algo$nP) * (mPbest[, sgbest] - mP)
        mV <- algo$iner * mV + mDV
        logik <- mV > 0
        mV[logik] <- pmin(mV, algo$maxV)[logik]
        logik <- mV < 0
        mV[logik] <- pmax(mV, -algo$maxV)[logik]
        mP <- mP + mV
        for (s in 1:algo$nP)
            vF[s] <- OF(mP[, s], ...)
        is.better <- vF < vFbest
        mPbest[, is.better] <- mP[, is.better]
        vFbest[is.better] <- vF[is.better]
        if (min(vF) < sGbest) {
            sGbest <- min(vF)
            sgbest <- which.min(vF)[1]
        }
    }
    list(vPar = mPbest[,sgbest],
         OFvalue = sGbest, popF = vFbest)
}


###################################################
### code chunk number 67: PSopt (eval = FALSE)
###################################################
## ## chunkname: PSopt
## PSopt(OF, algo = list(), ...)


###################################################
### code chunk number 68
###################################################
## PSopt(OF, algo, Data)


###################################################
### code chunk number 69
###################################################
m <- 3
n <- 7
A <- array(0, dim = c(m, n))
b <- 1:m
A
A + b


###################################################
### code chunk number 70: vectorize
###################################################
## set up matrix X with n rows and p columns
n <- 100
p <- 5
X <- array(rnorm(n*p), dim = c(n, p))
###
## set up population P
nP <- 50
P <- array(rnorm(p*nP), dim = c(p, nP))
###
loop <- function(X, P) {
    ans <- array(0, dim = c(n, nP))
    for (i in seq_len(ncol(P)))
        ans[, i] <- X%*%P[, i]
    ans
}
vect <- function(X, P)
    X %*% P

library("rbenchmark")
benchmark(loop(X, P),
          vect(X, P),
          order = "relative",
          replications = 1000)[, 1:4]

all.equal(loop(X, P), vect(X, P))  ## ... should be TRUE


###################################################
### code chunk number 71: of-lqs
###################################################
OF <- function(param, Data) {
    X <- Data$X
    y <- Data$y
    ## as.vector(y) for recycling; param is a matrix
    aux <- as.vector(y) - X %*% param
    aux <- aux * aux
    aux <- apply(aux, 2, sort, partial = Data$h)
    aux[Data$h, ] ## LQS
}


###################################################
### code chunk number 72
###################################################
## colSums(aux[1:Data$h, ]) ## LTS


###################################################
### code chunk number 73: partial-sort
###################################################
x <- rnorm(101)
xp <- sort(x, partial = 51)
do.call(par, par.nmof)
plot(xp, pch = 19, cex = 0.5)
middle <- xp[51]
abline(h = middle, v = 51)


###################################################
### code chunk number 74: createData
###################################################
createData <- function(n, p,
                       constant = TRUE,
                       sigma = 2,
                       oFrac = 0.1) {
    X <- array(rnorm(n*p), dim = c(n, p))
    if (constant)
        X[, 1] <- 1
    b <- rnorm(p)
    y <- X %*% b + rnorm(n)*0.5
    nO <- ceiling(oFrac*n)
    when <- sample.int(n, nO)
    X[when, -1] <- X[when, -1] + rnorm(nO, sd = sigma)
    list(X = X, y = y)
}
###
n <- 100  ## number of observations
p <- 10   ## number of regressors
constant <- TRUE  ## include constant in model?
sigma <- 5        ## sd of outliers
oFrac  <- 0.15    ## fraction of outliers in data


###################################################
### code chunk number 75: h
###################################################
h <- 70   ## ...or use something like floor((n+1)/2)


###################################################
### code chunk number 76: settings
###################################################
tmp <- createData(n, p, constant, sigma, oFrac)
X <- tmp$X
y <- tmp$y
Data <- list(y = y,
             X = X,
             h = h)
###
popsize <- 100
generations <- 400
ps <- list(min = rep(-10, p),
           max = rep(10, p),
            c1 = 1.0,
            c2 = 2.0,
          iner = 0.8,
         initV = 0.0,
            nP = popsize,
            nG = generations,
          maxV = 3,
        loopOF = FALSE,
        printBar = FALSE)
de <- list(min = rep(-10, p),
           max = rep(10, p),
            nP = popsize,
            nG = generations,
             F = 0.2,
            CR = 0.5,
        loopOF = FALSE,
        printBar = FALSE)
###
system.time(solPS <- PSopt(OF, ps, Data))
system.time(solDE <- DEopt(OF, de, Data))

library("MASS")
system.time(test1 <- lqs(y ~ X[,-1],
                         adjust = TRUE,
                         nsamp = 100000,
                         method = "lqs",
                         quantile = h))
res1 <- sort((y - X %*% as.matrix(coef(test1)))^2)[h]
res2 <- sort((y - X %*% as.matrix(solPS$xbest))^2)[h]
res3 <- sort((y - X %*% as.matrix(solDE$xbest))^2)[h]
cat("\nlqs:   ", res1, "\n",
    "PSopt: ", res2, "\n",
    "DEopt: ", res3, "\n", sep = "")


###################################################
### code chunk number 77: robust
###################################################
## n <- 100  ## number of observations
## p <- 10   ## number of regressors
## constant <- TRUE; sigma <- 5; oFrac  <- 0.15
## h <- 70   ## ... or use something like floor((n+1)/2)
## aux <- createData(n,p,constant,sigma,oFrac)
## X <- aux$X
## y <- aux$y
###
trials <- 100
res1 <- numeric(trials)
for (t in 1:trials){
    modl <- lqs(y ~ X[, -1],
                adjust = TRUE,
                nsamp = 'best',
                method = 'lqs', quantile = h)
    res1[t] <- sort((y - X %*% as.matrix(coef(modl)))^2)[h]
}
###
res2 <- numeric(trials)
for (t in 1:trials){
    modl <- lqs(y ~ X[, -1],
                adjust = TRUE,
                nsamp = 10000,
                method = 'lqs',
                quantile = h)
    res2[t] <- sort((y - X %*% as.matrix(coef(modl)))^2)[h]
}
###
res3 <- numeric(trials)
for (t in 1:trials){
    modl <- lqs(y ~ X[, -1],
                adjust = TRUE,
                nsamp = 100000,
                method = 'lqs',
                quantile = h)
    res3[t] <- sort((y - X %*% as.matrix(coef(modl)))^2)[h]
}


###################################################
### code chunk number 78: summary
###################################################
summary(cbind(default = res1,
              nsamp.10k = res2,
              nsmap.100k = res3))


###################################################
### code chunk number 79: lqs-edf
###################################################
xx <- pretty(res1, res2, res3)
Y <- sort(res1)
N <- trials
do.call(par, par.nmof)
plot(c(Y[1] ,Y), (0:N)/N,
     type = 's',
     col = grey(0.6),
     xlim = range(res1, res2, res3),
     ylab = "Frequency",
     xlab = "OF value")
Y <- sort(res2)
lines(c(Y[1], Y), (0:N)/N, type = 's', col = grey(0.2))
Y <- sort(res3)
lines(c(Y[1], Y), (0:N)/N, type = 's', col = grey(0.0))


###################################################
### code chunk number 80: gendata
###################################################
genData <- function(nP, nO, ol, dy) {
    ## create data as in Salibian-Barrera & Yohai 2006
    ## nP .. regressors
    ## nO .. number of obs
    ## ol .. number of outliers
    ## dy .. outlier size ('M' in S-B&Y 2006): 90 to 200
    mRN <- function(m, n)
        array(rnorm(m * n), dim = c(m, n))
    y <- mRN(nO, 1)
    X <- cbind(as.matrix(numeric(nO) + 1),
               mRN(nO, nP - 1))
    zz <- sample(nO)
    z <- cbind(1, 100,
               array(0, dim = c(1, nP - 2)))
    for (i in 1:ol){
        X[zz[i], ] <- z
        y[zz[i]] <- dy
    }
    list(X = X, y = y)
}
