### R code from "Numerical Methods and Optimization in Finance"

###################################################
### code chunk number 1: chapter-settings
###################################################
library("NMOF")
library("PMwR")
library("quadprog")
library("rbenchmark")
options(continue = "  ",
        digits = 3,
        width = 55,
        str = strOptions(strict.width = "cut"),
        useFancyQuotes = FALSE,
        warn = 2)
par.portfolio <- list(bty = "n",
                      las = 1,
                      mar = c(3, 3, 1, 1),
                      mgp = c(2, 0.5, 0),
                      tck = 0.01,
                      ps = 9)


###################################################
### code chunk number 2: random-returns
###################################################
random_returns <- function(na, ns, sd, mean = 0, rho = 0) {
    ## sd   = vol of returns
    ## mean = means of returns
    ##      ==> both may be scalars or vectors of length na

    ans <- rnorm(ns*na)
    dim(ans) <- c(na, ns)

    if (rho != 0) {
        C <- array(rho, dim = c(na, na))
        diag(C) <- 1
        ans <- t(chol(C)) %*% ans
    }
    ans <- ans*sd
    ans <- ans + mean
    t(ans)
}


###################################################
### code chunk number 3: minimum-var
###################################################
## minimum-variance portfolio with budget constraint
library("quadprog")
###
## create random return scenarios
R <- random_returns(na = 10, ns = 60, sd = 0.015)
###
## minimize variance
Q <- 2 * cov(R)
A <- rbind(rep(1,10))
a <- 1
result <- solve.QP(Dmat = Q,
                   dvec = rep(0, 10),
                   Amat = t(A),
                   bvec = a,
                   meq  = 1)
###
## check budget constraint and solution
w <- result$solution
sum(w)  ## budget constraint: should be 1
all.equal(as.numeric(var(R %*% w)), result$value)


###################################################
### code chunk number 4: mean-variance
###################################################
library("quadprog")
###
## create random returns
R <- random_returns(na = 20, ns = 60,
                    sd = 0.015, mean = 0.0025)
na <- ncol(R)
m <- colMeans(R) ## create return forecasts ...
rd <- mean(m)    ## ... and required return
###
wmax <- 0.1      ## maximum holding size
wmin <- 0.0      ## minimum holding size
###
## set up matrices
Q <- 2 * cov(R)
A <- t(rep(1, na))
a <- 1
B <- rbind(t(m),
           -diag(na),
            diag(na))
b <- rbind(rd, array(-wmax, dim = c(na, 1)),
               array( wmin, dim = c(na, 1)))
result <- solve.QP(Dmat = Q,
                   dvec = rep(0, na),
                   Amat = t(rbind(A, B)),
                   bvec = rbind(a, b),
                   meq  = 1)
###
w <- result$solution
sum(w)         ## check budget constraint
c(w %*% m >=   ## check return constraint
  rd - sqrt(.Machine$double.eps))
summary(w)               ## check holding size constraint


###################################################
### code chunk number 5: mvPortfolio
###################################################
all.equal(w,
          mvPortfolio(m, cov(R), min.return = rd,
                      wmin = 0, wmax = 0.1))


###################################################
### code chunk number 6: tangency
###################################################
library("quadprog")
###
## create random returns
na <- 20
ns <- 60
R <- random_returns(na = na, ns = ns,
                    sd = 0.015, mean = 0.005)
m <- colMeans(R) ## means
rf  <- 0.0001    ## riskfree rate (about 2.5% pa)
m.ex <- m - rf   ## excess means
###
## set up matrices
Q <- cov(R)    ##covariance matrix
B <- t(m.ex)
b <- 1
result <- solve.QP(Dmat = Q,
                   dvec = rep(0, na),
                   Amat = t(B),
                   bvec = b,
                   meq  = 1)
###
## rescale variables to obtain weights
w <- result$solution/sum(result$solution)
###
## compute Sharpe ratio
SR <- c(t(w) %*% m.ex / sqrt(t(w) %*% Q %*% w))
sum(w)               ## check budget constraint
c(t(w) %*% m) > rf   ## check return constraint

## test 1: regression approach from Britten-Jones (1999)
R2 <- R - rf
ones <- array(1, dim = c(ns, 1))
solR  <- lm(ones ~ -1 + R2)
w2 <- coef(solR)
w2 <- w2/sum(w2)
## ... w2 should be the same as w
all.equal(as.numeric(w), as.numeric(w2))

## test 2: no inequality constraints >> solve FOC
w3 <- solve(Q, m.ex)
w3 <- w3/sum(w3)
# ... w3 should be the same as w2 and w
all.equal(as.numeric(w2), as.numeric(w3))


###################################################
### code chunk number 7: frontier
###################################################
library("quadprog")
library("NMOF")
###
R <- fundData[1:100, 1:50]
###
na <- dim(R)[2L]   ## number of assets
m  <- colMeans(R)
Sigma <- cov(R)
wmax  <- 1.0       ## maximum holding size
wmin  <- 0.0       ## minimum holding size
###
## compute frontier
nFP       <- 100   ## number of frontier points
lambdaSeq <- seq(0.001, 0.999, length = nFP)
A <- array( 1, dim = c(1, na))
B <- rbind(-diag(na), diag(na))
a <- 1
b <- rbind(array(-wmax, dim = c(na,1)),
           array( wmin, dim = c(na,1)))
###
## matrix for effcient portfolios
pMat <- array(NA, dim = c(na, nFP))
rownames(pMat) <- paste("asset", 1:na)
for(lambda in lambdaSeq) {
    result <- solve.QP(Dmat = 2*(1 - lambda)*Sigma,
                       dvec = lambda*m,
                       Amat = t(rbind(A, B)),
                       bvec = rbind(a, b),
                       meq  = 1)
    pMat[, which(lambda==lambdaSeq)] <- result$solution
}
###
## plot results, for included assets only
## (plot is not shown in the text)
incl <- apply(pMat, 1, function(x) any(x > 1e-4))
do.call(par, par.portfolio)
par(mgp = c(1.5, 0.5, 0))
par(mar = c(2,2,1,6))
cols <- grey.colors(nrow(pMat))
bp <- barplot(100*pMat, legend.text = FALSE, space = 0,
              ylab = "Weight in %",
              xlab = "",
              col = cols)
par(xpd = TRUE)
legend("topright",
       legend = rownames(pMat)[incl],
       col = cols[incl],
       lty = 1,
       lwd = 5,
       inset = c(-0.2, 0))
mtext("Increasing Risk", 1)


###################################################
### code chunk number 8: identity
###################################################
ns <- 100
na <- 10
R <- array(rnorm(ns * na), dim = c(ns, ns))
R1 <- crossprod(R)/ns
R2 <- ( (ns-1)/ns )*cov(R) + outer(colMeans(R), colMeans(R))
identical(R1, R2)
all.equal(R1, R2)


###################################################
### code chunk number 9
###################################################
N <- 1000 ## set size of matrix
###
## create matrices
D <- diag(runif(N))
C <- array(runif(N * N), dim = c(N, N))
###
## compute product / compare time
library("rbenchmark")
benchmark(Z1 <- D %*% C %*% D,
          Z2 <- outer(diag(D), diag(D)) * C,
          Z3 <- diag(D) %*% t(diag(D)) * C,
          order = "relative")[, 1:4]
###
## check difference between matrices
max(abs(Z1 - Z2)) # ... or use all.equal(Z1, Z2)
max(abs(Z1 - Z3)) # ... or use all.equal(Z1, Z3)
###
## ... or with the Matrix package
library("Matrix")
D2 <- Diagonal(x = diag(D))
benchmark(Z1 <- D %*% C %*% D,
          Z2 <- outer(diag(D), diag(D)) * C,
          Z3 <- diag(D) %*% t(diag(D)) * C,
          Z4 <- D2 %*% C %*% D2,
          Z5 <- outer(diag(D2), diag(D2)) * C,
          order = "relative")[, c(1,3,4)]
## check difference between matrices
all.equal(as.numeric(Z1), as.numeric(Z4))
all.equal(as.numeric(Z1), as.numeric(Z5))


###################################################
### code chunk number 10: eigen
###################################################
C <- matrix(c(1  , 0.9, 0.9,
              0.9, 1  , 0.2,
              0.9, 0.2, 1  ), nrow = 3, byrow = TRUE)
eigen(C)$values


###################################################
### code chunk number 11: repair-matrix
###################################################
repairMatrix


###################################################
### code chunk number 12: try-repair
###################################################
C
repairMatrix(C)


###################################################
### code chunk number 13: rank
###################################################
na   <- 10 ## number of assets
nobs <- 10 ## number of observations
###
R <- array(rnorm(nobs * na, sd = 0.01), dim = c(nobs, na))
qr(cov(R))$rank


###################################################
### code chunk number 14
###################################################
ew <- rep(1/na, na) ## equal-weight portfolio
c(sqrt(ew %*% cov(R) %*% ew))


###################################################
### code chunk number 15: zero-vol
###################################################
zerovol <- svd(cov(R))$v[, 10]
c(sqrt(abs(zerovol %*% cov(R) %*% zerovol)))


###################################################
### code chunk number 16: rank-deficient
###################################################
nc <- 3    ## columns
nr <- 10   ## rows
M <- array(rnorm(nr*nc), dim = c(nr, nc))
###
C <- array(0.5, dim = c(nc, nc))
diag(C) <- 1
M <- M %*% chol(C)
M <- M[, c(1, 1, 1, 2, 3)]
cor(M)


###################################################
### code chunk number 17
###################################################
head(M, 3)


###################################################
### code chunk number 18: colSubset
###################################################
colSubset


###################################################
### code chunk number 19: colSubset-example
###################################################
colSubset(M)


###################################################
### code chunk number 20
###################################################
css <- colSubset(M)
C <- cor(M[, css$columns])
nc <- ncol(C)
nr <- 1000
X <- array(rnorm(nr*nc), dim = c(nr, nc))
X <- X %*% chol(C)
X <- X %*% css$multiplier
cor(X)


###################################################
### code chunk number 21: LSopt.
###################################################
LSopt. <- function(OF, algo = list(), ...) {
    xc <- algo$x0
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
### code chunk number 22
###################################################
## LSopt(OF, algo = list(), ...)


###################################################
### code chunk number 23: const-cor
###################################################
const_cor <- function(rho, n) {
    C <- array(rho, dim = c(n, n))
    diag(C) <- 1
    C
}


###################################################
### code chunk number 24: random-data
###################################################
na <- 500L ## create random data
C <- const_cor(0.6, na)
vols <- runif(na, min = 0.2, max = 0.4)
Sigma <- outer(vols, vols) * C


###################################################
### code chunk number 25: Data
###################################################
Data <- list(Sigma = Sigma,
             Kmin = 30L,
             Kmax = 60L,
             na = na,
             n.changes = 1L)


###################################################
### code chunk number 26: random-solution
###################################################
if (Data$Kmin == Data$Kmax)
    n.assets <- Data$Kmin else
    n.assets <- sample(Data$Kmin:Data$Kmax, 1L)
###
assets <- sort(sample(na, n.assets))
x0 <- logical(na)
x0[assets] <- TRUE
table(x0)


###################################################
### code chunk number 27: OF
###################################################
OF <- function(x, Data) {
    w <- x[x]/sum(x)
    c(w %*% Data$Sigma[x, x] %*% w)
}


###################################################
### code chunk number 28
###################################################
OF_alternative <- function(x, Data) {
    w <- x[x]/sum(x)
    res <- crossprod(w, Data$Sigma[x, x])
    c(tcrossprod(w, res))
}
###
all.equal(OF(x0, Data), OF_alternative(x0, Data))
library("rbenchmark")
benchmark(OF(x0, Data),
          OF_alternative(x0, Data),
          replications = 10000,
          order = "relative")[, c(1,3,4)]


###################################################
### code chunk number 29: OF-simple
###################################################
OF_simple <- function(x, Data) {
    w <- 1/sum(x)
    sum(w * w * Data$Sigma[x, x])
}
all.equal(OF(x0, Data), OF_simple(x0, Data))

benchmark(OF(x0, Data),
          OF_alternative(x0, Data),
          OF_simple(x0, Data),
          replications = 10000,
          order = "relative")[, c(1,3,4)]


###################################################
### code chunk number 30: neighbour
###################################################
neighbour <- function(xc, Data) {
    xn <- xc
    p <- sample.int(Data$na, Data$n.changes,
                    replace = FALSE)
    xn[p] <- !xn[p]

    ## reject infeasible solution
    if( (sum(xn) > Data$Kmax) ||
        (sum(xn) < Data$Kmin) )
        xc
    else
        xn
}


###################################################
### code chunk number 31: str-Data
###################################################
str(Data)


###################################################
### code chunk number 32: algo
###################################################
algo <- list(x0 = x0,
      neighbour = neighbour,
             nS = 30000L,
       printBar = FALSE)


###################################################
### code chunk number 33: both-LS
###################################################
set.seed(48957)
sol1 <- LSopt (OF, algo, Data)  ## the function in NMOF
set.seed(48957)
sol2 <- LSopt.(OF, algo, Data)  ## the abbreviated function
all.equal(sol1$xbest, sol2$xbest)


###################################################
### code chunk number 34
###################################################
sqrt(sol1$OFvalue)


###################################################
### code chunk number 35: progress
###################################################
do.call(par, par.portfolio)
par(mar = c(3, 5, 1, 1))
plot(sqrt(sol1$Fmat[, 2]), type="l",
     ylab = "Portfolio volatility", xlab = "Iteration")


###################################################
### code chunk number 36
###################################################
## restartOpt. <- function (fun, n, OF, algo = NULL, ...) {
##     n <- as.integer(n)
##     stopifnot(n > 0L)
##     allResults <- vector('list', n)
##     for (i in seq_len(n))
##         allResults[[i]] <- fun(OF, algo, ...)
##     allResults
## }


###################################################
### code chunk number 37
###################################################
algo$printDetail <- FALSE
trials <- 100L
allRes <- restartOpt(LSopt, n = trials,
                     OF, algo = algo, Data = Data)
allResOF <- numeric(trials)
for (i in 1:trials)
    allResOF[i] <- sqrt(allRes[[i]]$OFvalue)


###################################################
### code chunk number 38: OF-paths
###################################################
tmp <- lapply(allRes[1:5], `[[`, "Fmat")
###
do.call(par, par.portfolio)
par(mar = c(3, 5, 1, 1))
plot(sqrt(tmp[[1]][, 2]),
     type = "l",col="black",
     xlab = "Iteration", ylab = "Portfolio volatility")

for(i in 2:5){
    lines(sqrt(tmp[[i]][, 2]),
          type="l", col = grey(0.3))
}


###################################################
### code chunk number 39: part-cond
###################################################
x <- rnorm(20)
###
## mean and probability of loss
theta <- 0.1 # ... or mean(x)
prob.loss <- ecdf(x)(theta)
exponent <- 2
###
## conditional moment (CM)
(cm <- mean((x[x < theta] - theta)^exponent))

## partial moment (PM)
xx <- x - theta
xx[xx > 0] <- 0
(pm <- mean(xx^exponent))

## relationship between PM and CM
all.equal(cm * prob.loss, pm)


###################################################
### code chunk number 40: v
###################################################
v <- rnorm(100, mean = 0.01, sd = 0.03)
v <- c(1, cumprod(1 + v))


###################################################
### code chunk number 41: abs-drawdown
###################################################
absD <- cummax(v) - v


###################################################
### code chunk number 42: relative-drawdown1
###################################################
logv <- log(v)
d <- logv - cummax(logv)
relD1 <- 1 - exp(d)


###################################################
### code chunk number 43: relative-drawdown2
###################################################
cv <- cummax(v)
relD2 <- (cv - v) / cv


###################################################
### code chunk number 44
###################################################
all.equal(relD1, relD2)


###################################################
### code chunk number 45: drawdown
###################################################
do.call(par, par.portfolio)
par(mfrow = c(2, 1))
plot(v, type = "l", xlab = "", ylab = "v")
plot(100*relD1, type = "l",
     xlab = "", ylab = "Drawdown in %")


###################################################
### code chunk number 46: e-squared
###################################################
library("rbenchmark")
n <- 50000
e <- rnorm(n)
benchmark(
    z1 <- sum(e^2),
    z2 <- e %*% e,
    z3 <- sum(e * e),
    z4 <- crossprod(e),
    replications = 1000, order = "relative")[, c(1,3,4)]


###################################################
### code chunk number 47: setup
###################################################
library("NMOF")
###
na <- dim(fundData)[2L]
ns <- dim(fundData)[1L]
wmin <- 0.0
wmax <- 0.05
Data <- list(R = t(fundData),
            RR = crossprod(fundData),
            na = dim(fundData)[2L],
            ns = dim(fundData)[1L],
           eps = 0.5/100,
          wmin = wmin,
          wmax = wmax)


###################################################
### code chunk number 48: nb
###################################################
neighbour <- function(w, Data){
    eps <- runif(1) * Data$eps
    toSell <- which(w > Data$wmin)
    toBuy  <- which(w < Data$wmax)
    i <- toSell[sample.int(length(toSell), size = 1L)]
    j <- toBuy [sample.int(length(toBuy),  size = 1L)]
    eps <- min(w[i] - Data$wmin,
               Data$wmax - w[j],
               eps)
    w[i] <- w[i] - eps
    w[j] <- w[j] + eps
    w
}


###################################################
### code chunk number 49: resample
###################################################
resample <- function(x, ...)
    x[sample.int(length(x), ...)]


###################################################
### code chunk number 50
###################################################
OF1 <- function(w, Data) {
    Rw <- crossprod(Data$R, w)
    crossprod(Rw)
}
###
OF2 <- function(w, Data) {
    aux <- crossprod(Data$RR, w)
    crossprod(w, aux)
}


###################################################
### code chunk number 51
###################################################
w0 <- runif(na); w0 <- w0/sum(w0)
###
algo <- list(x0 = w0,
      neighbour = neighbour,
             nS = 2000L,
             nT = 10L,
             nD = 5000L,
              q = 0.20,
       printBar = FALSE)
system.time(res <- TAopt(OF1, algo, Data))
c(100*sqrt(crossprod(fundData %*% res$xbest)/ns))
system.time(res <- TAopt(OF2, algo, Data)) ## should be faster
c(100*sqrt(crossprod(fundData %*% res$xbest)/ns))


###################################################
### code chunk number 52: RR-benchmark
###################################################
library("quadprog")
covMatrix <- crossprod(fundData)
A <- rep(1, na)
a <- 1
B <- rbind(-diag(na),
            diag(na))
b <- rbind(array(-Data$wmax, dim = c(na,1)),
           array( Data$wmin, dim = c(na,1)))
system.time({
    result <- solve.QP(Dmat = covMatrix,
                       dvec = rep(0,na),
                       Amat = t(rbind(A,B)),
                       bvec = rbind(a,b),
                        meq = 1)
})
wqp <- result$solution


###################################################
### code chunk number 53: check-results
###################################################
## compare results
c(100 * sqrt( crossprod(fundData %*% wqp)/ns ))
c(100 * sqrt( crossprod(fundData %*% res$xbest)/ns ))


###################################################
### code chunk number 54: check-constraints
###################################################
min(res$xbest) ## TA
max(res$xbest) ## TA
sum(res$xbest) ## TA

min(wqp) ## QP
max(wqp) ## QP
sum(wqp) ## QP


###################################################
### code chunk number 55: neighbourU
###################################################
neighbourU <- function(sol, Data){
    wn <- sol$w
    toSell <- which(wn > Data$wmin)
    toBuy  <- which(wn < Data$wmax)
    i <- toSell[sample.int(length(toSell), size = 1L)]
    j <- toBuy [sample.int(length(toBuy),  size = 1L)]
    eps <- runif(1) * Data$eps
    eps <- min(wn[i] - Data$wmin, Data$wmax - wn[j], eps)
    wn[i] <- wn[i] - eps
    wn[j] <- wn[j] + eps
    Rw <- sol$Rw + Data$R[, c(i, j)] %*% c(-eps,eps)
    list(w = wn, Rw = Rw)
}


###################################################
### code chunk number 56
###################################################
na <- dim(fundData)[2L]
ns <- dim(fundData)[1L]
wmin <- 0.0
wmax <- 0.05
Data <- list(R = fundData,
            na = na,
            ns = ns,
           eps = 0.5/100,
          wmin = wmin,
          wmax = wmax)


###################################################
### code chunk number 57
###################################################
OF <- function(sol, Data)
    crossprod(sol$Rw)


###################################################
### code chunk number 58
###################################################
w0 <- runif(Data$na)
w0 <- w0/sum(w0)
x0 <- list(w = w0, Rw = fundData %*% w0)
algo <- list(x0 = x0,
      neighbour = neighbourU,
             nS = 2000L,
             nT = 10L,
             nD = 5000L,
              q = 0.20,
       printBar = FALSE)
system.time(res2 <- TAopt(OF, algo, Data))
c(100*sqrt(crossprod(fundData %*% res2$xbest$w)/ns))


###################################################
### code chunk number 59
###################################################
neighbourUK <- function(sol, Data){
    wn <- sol$w
    J <- wn > 0
    K <- sum(J)
    eps <- Data$eps * runif(1)
     if (K > Data$Kmin && K < Data$Kmax) {
        toSell <- wn > 0
        toBuy  <- wn < Data$wmax
    } else {
        if (K == Data$Kmax) {
            toSell <- wn > 0
            toBuy  <- J & (wn < Data$wmax)
        } else {  ## at Data$Kmin
            toSell <- wn > eps
            toBuy  <- wn < Data$wmax
        }
    }
    i <- resample(which(toSell),1)
    j <- resample(which(toBuy),1)
    eps <- min(wn[i], Data$wmax - wn[j], eps)
    wn[i] <- wn[i] - eps
    wn[j] <- wn[j] + eps
    Rw <- sol$Rw + Data$R[, c(i,j)] %*% c(-eps,eps)
    list(w = wn, Rw = Rw)
}


###################################################
### code chunk number 60: OFcmr
###################################################
OFcmR <- function(sol,Data) {
    Rw <- sol$Rw
    losses <- Rw - abs(Rw)
    gains <- Rw + abs(Rw)
    nL <- sum(losses < 0)
    nG <- sum(gains  > 0)
    vG <- sum(gains^Data$eG)
    vL <- sum(abs(losses)^Data$eL)
    (vL/nL) / (vG/nG)
}


###################################################
### code chunk number 61
###################################################
## prepare Data
na <- dim(fundData)[2L]
ns <- dim(fundData)[1L]
###
Data <- list(R = fundData,
        na = na, ns = ns,
        eps = 0.5/100,
        wmax = 0.1,
        eG = 2, eL = 2,
        Kmax = 50L)
Data$Kmin <- ceiling(1/Data$wmax)  ## computed from Data$wmax
###
## initial solution
card0 <- sample(Data$Kmin:Data$Kmax, 1)
assets <- sample.int(Data$na, card0, replace = FALSE)
w0 <- numeric(Data$na); w0[assets] <- 1/card0
sol0 <- list(w = w0, Rw = fundData %*% w0)
###
algo <- list(x0 = sol0, neighbour = neighbourUK,
             nS = 1000L, nT = 10L,
             nD = 10000L, q = 0.9,
             printBar = FALSE)
system.time(res <- TAopt(OFcmR,algo,Data))
plot(res$Fmat[, 1], type = "l")  ## not shown in text
res$OFvalue; sum(res$xbest$w <= 1e-8); sum(res$xbest$w > 1e-8)


###################################################
### code chunk number 62
###################################################
## eps <- Data$eps * runif(1)


###################################################
### code chunk number 63
###################################################
## eps <- Data$eps


###################################################
### code chunk number 64
###################################################
## Data$eps <- 2/100


###################################################
### code chunk number 65: hybrid-Data
###################################################
R <- fundData
S <- cov(R)
na <- ncol(R)
Data <- list(R = R,
             S = S,
             na = na,
             Kmin = 25,
             Kmax = 60,
             wmin = 0.01,
             wmax = 0.05)


###################################################
### code chunk number 66: of-mv
###################################################
mv <- function(x, Data) {
    attr(NMOF::minvar(Data$S[x, x],
                      wmin = Data$wmin,
                      wmax = Data$wmax), "variance")
}


###################################################
### code chunk number 67: N
###################################################
N <- function(x, Data) {
    xn <- x
    k <- sum(xn)
    i.in <- which(xn)
    i.out <- which(!xn)
    if (k == Data$Kmax) {
        i <- sample(i.in, 1)
    } else if (k == Data$Kmin) {
        i <- sample(i.out, 1)
    } else {
        i <- sample(Data$na, 1)
    }
    xn[i] <- !xn[i]
    xn
}


###################################################
### code chunk number 68: random-x
###################################################
random_x <- function(Data) {
    ans <- logical(Data$na)
    k <- if (Data$Kmax > Data$Kmin)
             sample(Data$Kmin:Data$Kmax, 1)
         else
             Data$Kmin
    ans[sample(Data$na, k)] <- TRUE
    ans
}


###################################################
### code chunk number 69
###################################################
table(random_x(Data))
table(random_x(Data))


###################################################
### code chunk number 70: test-run
###################################################
x0 <- random_x(Data)
sol.ls <- LSopt(mv,
                list(x0 = x0,
                     neighbour = N,
                     nI = 10000,
                     classify = TRUE,
                     printBar = FALSE),
                Data = Data)

sol.ta <- TAopt(mv,
                list(x0 = x0,  ## same x0 as for LSopt
                     neighbour = N,
                     nI = 10000,
                     classify = TRUE,
                     printBar = FALSE),
                Data = Data)

all.equal(sol.ls$xbest, sol.ta$xbest)


###################################################
### code chunk number 71: random-x-fun
###################################################
random_x_fun <- function(Data) {
    na <- Data$na
    kmin <- Data$Kmin
    kmax <- Data$Kmax
    stopifnot(kmax > kmin)
    function() {
        ans <- logical(na)
        ## if the case 'kmin == kmax' would
        ## need to be supported, we needed to
        ## hedge: if kmin equals kmax, sample
        ## will produce one value from the
        ## range 1:kmax. See ?sample.
        k <- sample(kmin:kmax, 1)
        ans[sample(na, k)] <- TRUE
        ans
    }
}


###################################################
### code chunk number 72: x0-fun
###################################################
x0_fun <- random_x_fun(Data)
x0_fun


###################################################
### code chunk number 73
###################################################
ls(envir = environment(x0_fun))
get("kmin", envir = environment(x0_fun))
get("kmax", envir = environment(x0_fun))


###################################################
### code chunk number 74
###################################################
str(as.list(environment(x0_fun)))


###################################################
### code chunk number 75
###################################################
algo <- list(x0 = x0_fun,
             neighbour = N,
             nI = 500,
             classify = TRUE,
             printBar = FALSE)


###################################################
### code chunk number 76
###################################################
sol.ls.500 <- restartOpt(LSopt, n = 100, OF = mv,
                          algo = algo, Data = Data,
                          method = "snow", cl = 6)
sol.ta.500 <- restartOpt(LSopt, n = 100, OF = mv,
                          algo = algo, Data = Data,
                          method = "snow", cl = 6)


###################################################
### code chunk number 77: OF-values
###################################################
of.ls.500 <- sqrt(sapply(sol.ls.500, `[[`, "OFvalue"))
of.ta.500 <- sqrt(sapply(sol.ta.500, `[[`, "OFvalue"))


###################################################
### code chunk number 78: random-solutions
###################################################
of.random <- numeric(500)
for (i in seq_along(of.random))
    of.random[i] <- mv(random_x(Data), Data)
of.random <- sqrt(of.random)


###################################################
### code chunk number 79: hybrid-dist (eval = FALSE)
###################################################
## ## chunkname: hybrid-dist
## plot(ecdf(of.random),
##      main = "",
##      xlim = c(0,
##               max(of.random,
##                   of.ls.500,
##                   of.ta.500)),
##      col = grey(0.6), pch = NA, verticals = TRUE)
## lines(ecdf(of.ls.500),
##      col = grey(0.2), pch = NA, verticals = TRUE)
## lines(ecdf(of.ta.500),
##      col = grey(0.4), pch = NA, verticals = TRUE)


###################################################
### code chunk number 80
###################################################
algo$nI <- 1000
sol.ls.1000 <- restartOpt(LSopt, n = 100, OF = mv,
                          algo = algo, Data = Data,
                          method = "snow", cl = 4)
sol.ta.1000 <- restartOpt(LSopt, n = 100, OF = mv,
                          algo = algo, Data = Data,
                          method = "snow", cl = 4)

algo$nI <- 2000
sol.ls.2000 <- restartOpt(LSopt, n = 100, OF = mv,
                          algo = algo, Data = Data,
                          method = "snow", cl = 4)
sol.ta.2000 <- restartOpt(LSopt, n = 100, OF = mv,
                          algo = algo, Data = Data,
                          method = "snow", cl = 4)


###################################################
### code chunk number 81: hybrid-OF-values
###################################################
do.call(par, par.portfolio)
plot(ecdf(of.random),
     main = "",
     xlim = c(0,
              max(of.random,
                  of.ls.500,
                  of.ta.500)),
     col = grey(0.6), pch = NA, verticals = TRUE)
lines(ecdf(of.ls.500),
     col = grey(0.2), pch = NA, verticals = TRUE)
lines(ecdf(of.ta.500),
     col = grey(0.4), pch = NA, verticals = TRUE)


###################################################
### code chunk number 82: hybrid-OF-values2
###################################################
do.call(par, par.portfolio)
plot(ecdf(of.ls.500),
     main = "",
     col = grey(0.2), pch = NA, verticals = TRUE,
     xlim = c(0.003, 0.005))
lines(ecdf(of.ta.500),
     col = grey(0.4), pch = NA, verticals = TRUE)
###
lines(ecdf(sqrt(sapply(sol.ls.1000, `[[`, "OFvalue"))),
     col = grey(0.2), pch = NA, verticals = TRUE)
lines(ecdf(sqrt(sapply(sol.ta.1000, `[[`, "OFvalue"))),
     col = grey(0.4), pch = NA, verticals = TRUE)
###
lines(ecdf(sqrt(sapply(sol.ls.2000, `[[`, "OFvalue"))),
     col = grey(0.2), pch = NA, verticals = TRUE)
lines(ecdf(sqrt(sapply(sol.ta.2000, `[[`, "OFvalue"))),
     col = grey(0.4), pch = NA, verticals = TRUE)


###################################################
### code chunk number 83: returns
###################################################
## generate artificial price data
##  R = returns, P = prices
ns <- 100  ## number of scenarios
na <- 10   ## number of assets
R  <- 1 + array(rnorm(ns * na, sd = 0.01),
                dim = c(ns, na))
P  <- rbind(100, R)
P  <- apply(P, 2, cumprod)
matplot(P, type = "l") ## or 'ts.plot(P)' or 'plot(zoo::zoo(P))'

## discrete returns
## compute returns: rets should be equal to R
rets1 <- P[-1, ] / P[-nrow(P), ]
## ... or
rets2 <- diff(P) / P[-nrow(P), ] + 1;
max(abs(rets1-R))      ## not exactly equal
max(abs(rets2-R))      ## not exactly equal
max(abs(rets1-rets1))  ##     exactly equal

## log-returns
rets3 <- diff(log(P));
## ... almost like discrete returns
plot(c(rets1) - c(rets3) - 1, cex = 0.5) ## not shown


###################################################
### code chunk number 84: PMwR-returns
###################################################
library("PMwR")
returns(1:5)
returns(cbind(1:5, 1:5))


###################################################
### code chunk number 85: Xy
###################################################
nR <- 6
nC <- 3
X <- array(rnorm(nR*nC), dim = c(nR, nC))
y <- rnorm(nR)
X


###################################################
### code chunk number 86: OF1
###################################################
OF1 <- function(param, X, y)
    max(abs(y - X %*% param))


###################################################
### code chunk number 87: OF2
###################################################
createF <- function(X, y) {
    function(param)
        max(abs(y - X %*% param))
}
OF2 <- createF(X, y)


###################################################
### code chunk number 88
###################################################
OF1
OF2


###################################################
### code chunk number 89: compare-OF
###################################################
param <- rnorm(nC)
OF1(param, X, y)
OF2(param)


###################################################
### code chunk number 90: remove-Xy
###################################################
remove(list = c("X", "y"))
try(OF1(param, X, y))
OF2(param)


###################################################
### code chunk number 91: get
###################################################
ls(envir = environment(OF2))
get("X", envir = environment(OF2), inherits = FALSE)


###################################################
### code chunk number 92: omega
###################################################
omega <- function(r, theta) {
    rr <- r - theta
    -sum(rr - abs(rr)) / sum(rr + abs(rr))
}


###################################################
### code chunk number 93
###################################################
## omega <- -sum( rr[rr < 0] ) / sum( rr[rr > 0] )


###################################################
### code chunk number 94: pmin-pmax
###################################################
library("rbenchmark")
x <- rnorm(1000)
all.equal(pmax(x, 0) * 2, x + abs(x))
benchmark(pmax(x, 0), x + abs(x),
          replications = 20000,
          order = "relative")[, 1:4]
all.equal(pmin(x, 0) * 2, x - abs(x))
benchmark(pmin(x, 0), x - abs(x),
          replications = 20000,
          order = "relative")[, 1:4]


###################################################
### code chunk number 95
###################################################
## create artifical data
## ...ns = number of scenarios
## ...na = number of assets
ns <- 200
na <- 100
R  <- array(rnorm(ns*na)*0.05, dim = c(ns,na) )
###
## set up a random portfolio
w   <- runif(na)
w   <- w / sum(w)
###
## compute returns
rp <- R %*% w
###
## compute omega
omega(rp, theta = 0.001)


###################################################
### code chunk number 96: omega2
###################################################
## objective function, alternative
omega2 <- function(r,theta) {
    rr <- r - theta
    omega2 <- -colSums(rr - abs(rr)) / colSums(rr + abs(rr))
    omega2
}

## check: compute omega
omega2(rp, theta = 0.001)


###################################################
### code chunk number 97
###################################################
## set up a random population
## ...nP = population size
nP <- 200
P <- array(runif(na*nP), dim = c(na, nP))
P <- P / outer(numeric(na)+1, colSums(P)) # budget constraint
###
## evaluate population
rp <- R %*% P
loop_over_cols <- function() {
    ans <- numeric(nP)
    for (i in seq_len(nP))
        ans[i] <- omega(rp[, i], theta = 0.001)
    ans
}

benchmark(loop_over_cols(),
          apply(rp, 2, omega, theta = 0.001),
          omega2(rp, theta = 0.001),
          order = "relative",
          replications = 1000)[, c(1,3,4)]

rp <- R %*% P
a1 <- loop_over_cols()
a2 <- apply(rp, 2, omega, theta = 0.001)
a3 <- omega2(rp, theta = 0.001)
all.equal(a1, a2)
all.equal(a1, a3)


###################################################
### code chunk number 98: switch
###################################################
N_switch <- function(x, Data) {
    out <- !x
    in.i  <- which(x)
    out.i <- which(out)

    if (!length(out.i))
        return(x)
    i <- in.i [sample.int(length(in.i ), size = 1)]
    j <- out.i[sample.int(length(out.i), size = 1)]
    x[i] <- !x[i]
    x[j] <- !x[j]
    x
}


###################################################
### code chunk number 99: compareLogicals
###################################################
compareLogicals <- function(x, y, ...) {
    argsL <- list(...)
    if (!("sep" %in% names(argsL)))
        argsL$sep <- ""
    do.call("cat",
            c(list("\n", as.integer(x),
                   "\n", as.integer(y),
                   "\n", ifelse(x == y, " ", "^"), "\n"),
              argsL))
    message("The vectors differ in ",
            sum(x != y),
            " place(s).")
    invisible(sum(x != y))
}


###################################################
### code chunk number 100: xy
###################################################
x <- runif(10) > 0.5
x
N_switch(x)
compareLogicals(x, N_switch(x))
compareLogicals(x, N_switch(x))
compareLogicals(x, N_switch(x))
