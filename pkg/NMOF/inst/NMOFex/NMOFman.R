### R code from vignette source 'NMOFman.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: NMOFman.Rnw:6-10
###################################################
version <- as.Date("2012-11-08")
options(continue = " ", digits = 3, width = 70, scipen = 1e8)
require("cacheSweave")
setCacheDir("~/Books/NMOFman/cache/")


###################################################
### code chunk number 2: NMOFman.Rnw:169-171 (eval = FALSE)
###################################################
## code <- system.file("NMOFex/NMOFman.R", package = "NMOF")
## file.show(code, title = "NMOF manual")


###################################################
### code chunk number 3: NMOFman.Rnw:182-183
###################################################
require("NMOF")


###################################################
### code chunk number 4: NMOFman.Rnw:190-191
###################################################
require("rbenchmark")


###################################################
### code chunk number 5: NMOFman.Rnw:197-199
###################################################
resample <- function(x, ...)
    x[sample.int(length(x), ...)]


###################################################
### code chunk number 6: NMOFman.Rnw:254-255
###################################################
set.seed(123321)


###################################################
### code chunk number 7: NMOFman.Rnw:281-300
###################################################
## set number of rows
n <- 100L

## set a correlation between a and b
rho <- 0.7

## create correlation matrix
C <- array(rho, dim = c(2L, 2L))
diag(C) <- 1

## make columns of ab correlated
ab <- array(rnorm(n * 2), dim = c(n, 2L)) %*% chol(C)
colnames(ab) <- c("a", "b")

par(mfrow = c(1L, 3L), bty = "n", mar = c(4, 4, 0, 0),
    tck = 0.01, las = 1, ps = 9, mgp = c(3, 1, 0.5))
plot(ab, pch = 19, cex = 0.5,
     xaxt = "n", yaxt = "n", xlab = "a", ylab = "b")
axis(1); axis(2)


###################################################
### code chunk number 8: NMOFman.Rnw:311-346
###################################################

plotSubsets <- function(ab, subset1, subset2) {
    if (missing(subset2))
        subset2 <- !subset1
    if (cor(ab[subset1, ])[1L,2L] < cor(ab[subset2, ])[1L,2L]) {
        tmp <- subset1
        subset1 <- subset2
        subset2 <- tmp
    }

    par(mfrow = c(1L, 3L), bty = "n", mar = c(4, 4, 0, 0),
        tck = 0.01, las = 1, ps = 9, mgp = c(3,1,0.5))

    prx <- pretty(ab)
    lims <- c(min(prx), max(pretty(prx)))

    plot(ab[subset1, ], xlim = lims, ylim = lims, xlab = "a", ylab = "b",
         col = grey(0.2), pch = 19, cex = 0.5, xaxt = "n", yaxt = "n")
    axis(1); axis(2)
    lines(ab[subset2, ], col = grey(0.75), pch = 19, type = "p", cex = 0.5)
    axis(2)

    par(mar = c(4,2,0,2))
    plot(ab[subset1, ], xlim=lims, ylim = lims, xlab = "a", ylab = "",
         col = grey(0.2), pch = 19, cex = 0.5, xaxt = "n", yaxt = "n")
    axis(1)

    par(mar = c(4,0,0,4))
    plot(ab[subset2, ], xlim=lims, ylim = lims, xlab = "a", ylab = "",
         col = grey(0.75), pch = 19, cex = 0.5, xaxt = "n", yaxt = "n")
    axis(1)

    invisible(NULL)
}
plotSubsets(ab, 1:50, 51:100)


###################################################
### code chunk number 9: NMOFman.Rnw:352-354
###################################################
cor(ab[ 1: 50, ])
cor(ab[51:100, ])


###################################################
### code chunk number 10: NMOFman.Rnw:373-374
###################################################
x0 <- rep(c(TRUE, FALSE), each = 50L)


###################################################
### code chunk number 11: NMOFman.Rnw:379-380
###################################################
x0 <- runif(n) > 0.5


###################################################
### code chunk number 12: NMOFman.Rnw:391-393
###################################################
OF <- function(x, ab)
    -abs(cor(ab[x, ])[1L, 2L] - cor(ab[!x, ])[1L, 2L])


###################################################
### code chunk number 13: NMOFman.Rnw:399-402
###################################################
x0 <- rep(c(TRUE, FALSE), each = 50L)
OF( x0, ab)
OF(!x0, ab) ## should give the same result


###################################################
### code chunk number 14: NMOFman.Rnw:421-435
###################################################
trials <- 1e5
OFvalues <- numeric(trials)
solutions <- vector("list", trials)
for (i in seq_len(trials)) {

    ## create a random solution
    c1 <- sample(21:80, 1L)  ## cardinality of subset 1
    x0 <- logical(n)
    x0[sample.int(n, c1)] <- TRUE

    ## store results
    OFvalues[i] <- OF(x0, ab)
    solutions[[i]] <- x0
}


###################################################
### code chunk number 15: NMOFman.Rnw:440-443
###################################################
summary(OFvalues)
xbest <- which.min(OFvalues)
OFvalues[xbest]


###################################################
### code chunk number 16: NMOFman.Rnw:450-451
###################################################
xRandom <- solutions[[xbest]]


###################################################
### code chunk number 17: NMOFman.Rnw:474-479
###################################################
subset1 <- ab[ ,1L] * ab[ ,2L] >  0
subset2 <- ab[ ,1L] * ab[ ,2L] <= 0
c1 <- cor(ab[subset1, ])[1L, 2L]
c2 <- cor(ab[subset2, ])[1L, 2L]
OF(subset1, ab)


###################################################
### code chunk number 18: NMOFman.Rnw:483-485
###################################################
sum(subset1)
sum(subset2)


###################################################
### code chunk number 19: NMOFman.Rnw:492-499
###################################################
cr <- order(ab[ ,1L] * ab[ ,2L])
OFvalues <- numeric(n)
for (i in 20:80) {
    x0 <- logical(n)
    x0[cr[seq_len(i)]] <- TRUE
    OFvalues[i] <- OF(x0, ab)
}


###################################################
### code chunk number 20: NMOFman.Rnw:504-509
###################################################
cutoff <- which.min(OFvalues)
subset1 <- logical(n)
subset1[cr[seq_len(n) <= cutoff]] <- TRUE
subset2 <- !subset1
OF(subset1, ab)


###################################################
### code chunk number 21: NMOFman.Rnw:515-516
###################################################
xConstr <- subset1


###################################################
### code chunk number 22: NMOFman.Rnw:520-521
###################################################
plotSubsets(ab, subset1)


###################################################
### code chunk number 23: NMOFman.Rnw:538-575
###################################################
greedy <- function(fun, x0, ab, n, nmin, maxit = 1000L) {
    done <- FALSE
    xbest <- xc <- x0
    xbestF <- xcF <- fun(xbest, ab)
    ic <- 0

    while (!done) {
        if (ic > maxit)
            break
        else
            ic <- ic + 1L
        done <- TRUE
        xc <- xbest
        for (i in seq_len(n)) {

            ## create a new solution
            xn <- xc
            xn[i] <- !xn[i]

            ## check constraints
            sxn <- sum(xn)
            enough <- sxn >= nmin
            notTooMany <- sxn <= n - nmin

            ## update best solution
            if (enough && notTooMany) {
                xnF <- fun(xn, ab)
                if (xnF < xbestF) {
                    xbest <- xn
                    xbestF <- xnF
                    done <- FALSE
                }
            }
        }
    }
    list(xbest = xbest, OFvalue = xbestF, ic = ic)
}


###################################################
### code chunk number 24: NMOFman.Rnw:583-590
###################################################
x0 <- rep(c(TRUE, FALSE), each = 50L)
result <- greedy(fun = OF, x0 = x0, ab = ab,
                 n = 100, nmin = 20, maxit = 1000L)
xGreedy <- result$xbest
OF(x0, ab)
OF(xGreedy, ab)
result$OFvalue


###################################################
### code chunk number 25: NMOFman.Rnw:594-595
###################################################
result$ic


###################################################
### code chunk number 26: NMOFman.Rnw:602-618
###################################################
trials <- 1e3
OFvalues <- numeric(trials)
solutions <- vector("list", trials)
moves <- numeric(trials)
for (i in seq_len(trials)) {

    ## create a random solution and improve it
    x0 <- runif(n) > 0.5
    result <- greedy(fun = OF, x0 = x0, ab = ab,
                     n = 100, nmin = 20, maxit = 1000L)

    ## store results
    OFvalues[i] <- result$OFvalue
    solutions[[i]] <- result$xbest
    moves[i] <- result$ic
}


###################################################
### code chunk number 27: NMOFman.Rnw:622-623
###################################################
summary(OFvalues)


###################################################
### code chunk number 28: NMOFman.Rnw:630-631
###################################################
summary(moves)


###################################################
### code chunk number 29: NMOFman.Rnw:637-640
###################################################
xbest <- which.min(OFvalues)
OFvalues[xbest]
xGreedy <- solutions[[xbest]]


###################################################
### code chunk number 30: NMOFman.Rnw:657-658
###################################################
Data <- list(ab = ab, n = n, nmin = 20L)


###################################################
### code chunk number 31: NMOFman.Rnw:662-663
###################################################
x0 <- runif(n) > 0.5


###################################################
### code chunk number 32: NMOFman.Rnw:667-685
###################################################
neighbour <- function(xc, Data) {
    xn <- xc

    ## choose which elements to change
    p <- sample.int(Data$n, size = 1L)
    xn[p] <- !xn[p]

    ## check constraints
    sxn <- sum(xn)
    enough <- sxn >= Data$nmin
    notTooMany <- sxn <= (Data$n - Data$nmin)

    ## if constraints are ok: return new solution, else old solution
    if (enough && notTooMany)
        xn
    else
        xc
}


###################################################
### code chunk number 33: NMOFman.Rnw:691-692
###################################################
table(x0 == neighbour(x0, Data))


###################################################
### code chunk number 34: NMOFman.Rnw:696-702
###################################################
OF <- function(x, Data)
    -abs(cor(Data$ab[x, ])[1L, 2L] - cor(Data$ab[!x, ])[1L, 2L])

## check
OF(x0, Data)
OF(neighbour(x0, Data), Data)


###################################################
### code chunk number 35: NMOFman.Rnw:706-712
###################################################
algo <- list(nS = 3000L,             ## number of steps to make
             neighbour = neighbour,  ## neighbourhood function
             x0 = x0,                ## initial solution
             printBar = FALSE)
sol1 <- LSopt(OF, algo = algo, Data = Data)
sol1$OFvalue


###################################################
### code chunk number 36: NMOFman.Rnw:719-723
###################################################
subset1 <-  sol1$xbest
subset2 <- !sol1$xbest
c1 <- cor(ab[subset1, ])[1L, 2L]
c2 <- cor(ab[subset2, ])[1L, 2L]


###################################################
### code chunk number 37: NMOFman.Rnw:727-728
###################################################
plotSubsets(ab, subset1)


###################################################
### code chunk number 38: ranfun
###################################################
makex0 <- function() {
    x <- logical(100)
    while (sum(x) > 80 || sum(x) < 20)
        x <- runif(100) > 0.5
    x
}


###################################################
### code chunk number 39: NMOFman.Rnw:750-761
###################################################
algo <- list(nS = 4000L,             ## number of steps to make
             neighbour = neighbour,  ## neighbourhood function
             x0 = makex0,            ## initial solution
             printBar = FALSE,
             printDetail = FALSE)
restarts1 <- restartOpt(LSopt, 100, OF = OF, algo = algo, Data)
restarts1OFvalues <- sapply(restarts1, `[[`, "OFvalue")

algo$nS <- 8000L
restarts2 <- restartOpt(LSopt, 100, OF = OF, algo = algo, Data)
restarts2OFvalues <- sapply(restarts2, `[[`, "OFvalue")


###################################################
### code chunk number 40: NMOFman.Rnw:767-773
###################################################
par(bty = "n", las = 1, mar = c(3, 4, 0, 0), ps = 8, tck = 0.001)
plot( ecdf(restarts1OFvalues), main = "", ylab = "", xlab = "",
     cex = 0.4, pch = 19, col = grey(.2), xlim = c(-2,-1))
lines(ecdf(restarts2OFvalues),
     cex = 0.4, pch = 19, col = grey(.6))
abline(v = OF(xConstr, Data))


###################################################
### code chunk number 41: NMOFman.Rnw:794-799
###################################################
algo$q <- 0.9
algo$nT <- 10
algo$nS <- 400
sol2 <- TAopt(OF, algo = algo, Data = Data)
sol2$OFvalue


###################################################
### code chunk number 42: NMOFman.Rnw:803-808
###################################################
subset1 <-  sol2$xbest
subset2 <- !sol2$xbest
c1 <- cor(ab[subset1, ])[1L, 2L]
c2 <- cor(ab[subset2, ])[1L, 2L]
OF(sol2$xbest, Data)


###################################################
### code chunk number 43: NMOFman.Rnw:812-813
###################################################
plotSubsets(ab, subset1)


###################################################
### code chunk number 44: NMOFman.Rnw:817-825
###################################################
algo$printBar <- FALSE
algo$printDetail <- FALSE
restarts3 <- restartOpt(TAopt, 100, OF = OF, algo = algo, Data)
restarts3OFvalues <- sapply(restarts3, `[[`, "OFvalue")

algo$nS <- 1500
restarts4 <- restartOpt(TAopt, 100, OF = OF, algo = algo, Data)
restarts4OFvalues <- sapply(restarts4, `[[`, "OFvalue")


###################################################
### code chunk number 45: NMOFman.Rnw:828-838
###################################################
par(bty = "n", las = 1, mar = c(3, 4, 0, 0), ps = 8, tck = 0.001)
plot( ecdf(restarts1OFvalues), main = "", ylab = "", xlab = "",
     cex = 0.4, pch = 19, col = grey(.2), xlim = c(-2,-1))
lines(ecdf(restarts2OFvalues),
     cex = 0.4, pch = 19, col = grey(.6))
lines(ecdf(restarts3OFvalues),
     cex = 0.4, pch = 19, col = grey(.2), lty=2)
lines(ecdf(restarts4OFvalues),
     cex = 0.4, pch = 19, col = grey(.6), lty=2)
abline(v = OF(xConstr, Data))


###################################################
### code chunk number 46: NMOFman.Rnw:853-854
###################################################
set.seed(46457)


###################################################
### code chunk number 47: NMOFman.Rnw:879-888
###################################################
randomData <- function(p, n, rscale = 0.5) {
    X <- array(rnorm(n * p), dim = c(n, p))
    k <- sample.int(p, 1L)    ## the number of regressors
    K <- sample.int(p, k)     ## the set of regressors
    betatrue <- numeric(p)
    betatrue[K] <- rnorm(k)   ## the true coefficients
    y <- X %*% betatrue + rnorm(n)*rscale
    list(X = X, y = y, betatrue = betatrue, K = K, n = n, p = p)
}


###################################################
### code chunk number 48: NMOFman.Rnw:896-899
###################################################
n <- 60L
p <- 5L
rD <- randomData(p, n)


###################################################
### code chunk number 49: NMOFman.Rnw:912-913
###################################################
b0 <- rnorm(p)


###################################################
### code chunk number 50: NMOFman.Rnw:924-928
###################################################
Data <- list(X = rD$X,
             y = rD$y,
             p = rD$p,
             n = rD$n)


###################################################
### code chunk number 51: NMOFman.Rnw:934-938
###################################################
OFls <- function(b, Data) {
    tmp <- Data$y - Data$X %*% b
    sum(tmp * tmp)
}


###################################################
### code chunk number 52: NMOFman.Rnw:944-949
###################################################
tmp <- rnorm(1e4)
benchmark(ignore1 <- tmp * tmp * tmp,
          ignore2 <- tmp^3,
          columns = c("test", "elapsed", "relative"),
          replications = 1e3, order = "relative")


###################################################
### code chunk number 53: NMOFman.Rnw:954-955
###################################################
all.equal(ignore1, ignore2)


###################################################
### code chunk number 54: NMOFman.Rnw:960-966
###################################################
algo <- list(nG = 200,  ## number of generations
             nP = 50,   ## population size
             min = rep(-20, p),
             max = rep( 20, p),
             printBar = FALSE)
resDE <- DEopt(OFls, algo = algo, Data = Data)


###################################################
### code chunk number 55: NMOFman.Rnw:972-974
###################################################
data.frame(QR = qr.solve(Data$X, Data$y),
           DE = resDE$xbest)


###################################################
### code chunk number 56: NMOFman.Rnw:989-992
###################################################
algo <- list(nP = 50,
             min = rep(-20, p), max = rep( 20, p),
             printBar = FALSE, printDetail = FALSE)


###################################################
### code chunk number 57: NMOFman.Rnw:996-1004
###################################################
algo$nG <- 25
results1 <- restartOpt(DEopt, n = 50, OF= OFls, algo = algo, Data = Data)
algo$nG <- 50
results2 <- restartOpt(DEopt, n = 50, OF= OFls, algo = algo, Data = Data)
algo$nG <- 100
results3 <- restartOpt(DEopt, n = 50, OF= OFls, algo = algo, Data = Data)
algo$nG <- 200
results4 <- restartOpt(DEopt, n = 50, OF= OFls, algo = algo, Data = Data)


###################################################
### code chunk number 58: NMOFman.Rnw:1010-1015
###################################################
par(bty = "n", las = 1, mar = c(3, 4, 0, 0), ps = 8, tck = 0.001)
plot( ecdf(sapply(results1, `[[`, "OFvalue")), main = "", ylab = "", xlab = "",
     cex = 0.4, pch = 19, col = grey(.6), xlim = c(0,150), ylim = c(0,1))
lines(ecdf(sapply(results2, `[[`, "OFvalue")),
     cex = 0.4, pch = 19, col = grey(.4))


###################################################
### code chunk number 59: NMOFman.Rnw:1020-1025
###################################################
par(bty = "n", las = 1, mar = c(3, 4, 0, 0), ps = 8, tck = 0.001)
plot( ecdf(sapply(results2, `[[`, "OFvalue")), main = "", ylab = "", xlab = "",
     cex = 0.4, pch = 19, col = grey(.6), ylim = c(0,1))
lines(ecdf(sapply(results3, `[[`, "OFvalue")),
     cex = 0.4, pch = 19, col = grey(.4))


###################################################
### code chunk number 60: NMOFman.Rnw:1030-1035
###################################################
par(bty = "n", las = 1, mar = c(3, 4, 0, 0), ps = 8, tck = 0.001)
plot( ecdf(sapply(results3, `[[`, "OFvalue")), main = "", ylab = "", xlab = "",
     cex = 0.4, pch = 19, col = grey(.4), ylim = c(0,1))
lines(ecdf(sapply(results4, `[[`, "OFvalue")),
     cex = 0.4, pch = 19, col = grey(.2))


###################################################
### code chunk number 61: NMOFman.Rnw:1047-1052
###################################################
OFlts <- function(b, Data) {
    tmp <- Data$y - Data$X %*% b
    tmp <- sort(tmp * tmp, partial = Data$h)
    sum(tmp[seq_len(Data$h)])
}


###################################################
### code chunk number 62: NMOFman.Rnw:1060-1064
###################################################
require("robustbase")
alpha <- 0.9
Data <- list(X = rD$X, y = rD$y, p = rD$p, n = rD$n,
             h = h.alpha.n(alpha, n = n, p = p))


###################################################
### code chunk number 63: NMOFman.Rnw:1071-1076
###################################################
resDE  <- DEopt(OFlts, algo = algo, Data = Data)
resLTS <- ltsReg(rD$y ~ -1 + rD$X, alpha = alpha,
                use.correction = FALSE)
data.frame(fastLTS = resLTS$raw.coefficients,
           DE = resDE$xbest)


###################################################
### code chunk number 64: NMOFman.Rnw:1080-1086
###################################################
cLTS <- resLTS$raw.coefficients
cat("LTS")
sum(sort((Data$X %*%cLTS - Data$y)^2)[1:Data$h])
cDE <- resDE$xbest
cat("DEopt")
sum(sort((Data$X %*%cDE -  Data$y)^2)[1:Data$h])


###################################################
### code chunk number 65: NMOFman.Rnw:1094-1096
###################################################
any(b0 < 0)
sum(b0)


###################################################
### code chunk number 66: NMOFman.Rnw:1105-1109
###################################################
repair <- function(b, Data) {
    b <- abs(b)
    b/sum(b)
}


###################################################
### code chunk number 67: NMOFman.Rnw:1116-1119
###################################################
b1 <- repair(b0, Data)
all(b1 >= 0)  ## should be TRUE
sum(b1)       ## should be 1


###################################################
### code chunk number 68: NMOFman.Rnw:1129-1132
###################################################
b0
b0 - abs(b0)
sum(b0)


###################################################
### code chunk number 69: NMOFman.Rnw:1139-1145
###################################################
b <- rnorm(1000L)
benchmark(ignore1 <- sum(b - abs(b))/2,
          ignore2 <- sum(b[b < 0]),
          columns = c("test", "elapsed", "relative"),
          replications = 1e4, order = "relative")
all.equal(ignore1, ignore2)


###################################################
### code chunk number 70: NMOFman.Rnw:1149-1150
###################################################
abs(sum(b0) - 1)


###################################################
### code chunk number 71: NMOFman.Rnw:1155-1161
###################################################
Data$pw1 <- 100
Data$pw2 <- 100
penalty <- function(b, Data)
    Data$pw1 * -sum(b - abs(b)) + Data$pw2 * abs(sum(b) - 1)
penalty(b0, Data)
penalty(b1, Data)


###################################################
### code chunk number 72: NMOFman.Rnw:1166-1170
###################################################
algo <- list(nG = 300, nP = 50,
             min = rep(-20, p), max = rep( 20, p),
             printBar = FALSE)
resDE <- DEopt(OFls, algo = algo, Data = Data)


###################################################
### code chunk number 73: NMOFman.Rnw:1175-1179
###################################################
round(resDE$xbest, 5)
resDE$OFvalue
sum(resDE$xbest)
all(resDE$xbest >= 0)


###################################################
### code chunk number 74: NMOFman.Rnw:1183-1189
###################################################
algo$repair <- repair
resDE <- DEopt(OFls, algo = algo, Data = Data)
round(resDE$xbest,5)
resDE$OFvalue
sum(resDE$xbest)
all(resDE$xbest >= 0)


###################################################
### code chunk number 75: NMOFman.Rnw:1192-1199
###################################################
algo$repair <- NULL
algo$pen <- penalty
resDE <- DEopt(OFls, algo = algo, Data = Data)
round(resDE$xbest,20)
resDE$OFvalue
sum(resDE$xbest)
all(resDE$xbest >= 0)


###################################################
### code chunk number 76: NMOFman.Rnw:1213-1214
###################################################
OFlts


###################################################
### code chunk number 77: NMOFman.Rnw:1217-1220
###################################################
b0 <- rnorm(p)
b1 <- rnorm(p)
P <- cbind(b0 = b0, b1 = b1)


###################################################
### code chunk number 78: NMOFman.Rnw:1224-1227
###################################################
head(Data$y - Data$X %*% b0)
head(Data$y - Data$X %*% b1)
head(drop(Data$y) - Data$X %*% P)


###################################################
### code chunk number 79: NMOFman.Rnw:1236-1239
###################################################
head(Data$y - Data$X %*% b0)^2
head(Data$y - Data$X %*% b1)^2
head((drop(Data$y) - Data$X %*% P)*(drop(Data$y) - Data$X %*% P))


###################################################
### code chunk number 80: NMOFman.Rnw:1242-1248
###################################################
OFlts2 <- function(b, Data) {
    tmp <- drop(Data$y) - Data$X %*% b
    tmp <- tmp * tmp
    tmp <- apply(tmp, 2L, sort, partial = Data$h)
    .colSums(tmp[seq_len(Data$h), ,drop = FALSE], Data$h, ncol(b))
}


###################################################
### code chunk number 81: NMOFman.Rnw:1252-1255
###################################################
nP <- 100
P <- array(rnorm(p * nP), dim = c(p, nP))
sol0 <- OFlts2(P, Data)


###################################################
### code chunk number 82: NMOFman.Rnw:1258-1265
###################################################
sol1 <- numeric(nP)
benchmark(for (i in seq_len(nP))
          sol1[i] <- OFlts(P[ , i, drop = FALSE], Data),
          sol2 <- OFlts2(P, Data),
          columns = c("test", "elapsed", "relative"),
          replications = 100, order = "relative")
all.equal(ignore1, ignore2)


###################################################
### code chunk number 83: NMOFman.Rnw:1441-1448
###################################################
OF <- tfTrefethen
n <- 100L
surf <- matrix(NA, n, n)
x1 <- seq(from = -10, to = 10, length.out = n)
for (i in seq_len(n))
    for (j in seq_len(n))
        surf[i, j] <- tfTrefethen(c(x1[i], x1[j]))


###################################################
### code chunk number 84: NMOFman.Rnw:1456-1462
###################################################
par(bty = "n", las = 1, mar = c(3,4,0,0),
    ps = 8, tck = 0.001, mgp = c(3, 0.5, 0))
contour(x1, x1, surf, nlevels=5, col = grey(0.6))

## the actual minimum
abline(v = -0.02440308, h = 0.21061243, col = grey(0.6))


###################################################
### code chunk number 85: NMOFman.Rnw:1468-1479
###################################################
algo <- list(nP = 50L,
             nG = 300L,
             F = 0.6,
             CR = 0.9,
             min = c(-10,-10),
             max = c(10,10),
             printDetail = FALSE,
             printBar = FALSE,
             storeF = TRUE,
             storeSolutions = TRUE)
sol <- DEopt(OF = OF, algo = algo)


###################################################
### code chunk number 86: NMOFman.Rnw:1484-1490
###################################################
names(sol)
sd(sol$popF)
ts.plot(sol$Fmat, xlab = "generations", ylab = "OF")

length(sol$xlist)
xlist <- sol$xlist[[1L]]


###################################################
### code chunk number 87: NMOFman.Rnw:1499-1514
###################################################
## show solution 1 (column 1) in population over time
xlist[[  1L]][ ,1L]  ## at the end of generation 1
## ...
xlist[[ 10L]][ ,1L]  ## at the end of generation 10
## ...
xlist[[300L]][ ,1L]  ## at the end of generation 300

res  <- sapply(xlist, `[`, 1:2, 1)  ## get row 1 and 2 from column 1
res2 <- sapply(xlist, `[`, TRUE, 1) ## simpler
all.equal(res, res2)

dim(res)
res[ ,1L]
res[ ,2L]
res[ ,300L]


###################################################
### code chunk number 88: NMOFman.Rnw:1520-1534
###################################################
## show parameter 2 (row 2) in population over time
xlist[[  1L]][2L, ]  ## at the end of generation 1
## ...
xlist[[ 10L]][2L, ]  ## at the end of generation 10
## ...
xlist[[300L]][2L, ]  ## at the end of generation 300

res <- sapply(xlist, `[`, 2, 1:50)
res <- sapply(xlist, `[`, 2, TRUE)  ## simpler

dim(res)
res[ ,1L]
res[ ,2L]
res[ ,300L]


###################################################
### code chunk number 89: NMOFman.Rnw:1540-1549 (eval = FALSE)
###################################################
## ## transposing xlist[[i]] gives a two-column matrix -- see ?points
## ## initial solutions
## points(t(xlist[[1L]]), pch = 21, bg=grey(0.9), col = grey(.2))
## 
## ## solutions at the end of generation 100
## points(t(xlist[[100L]]), pch = 21, bg=grey(0.9), col = grey(.2))
## 
## ## solutions at the end of generation 100
## points(t(xlist[[300L]]), pch = 21, bg=grey(0.9), col = grey(.2))


###################################################
### code chunk number 90: NMOFman.Rnw:1553-1582
###################################################
setEPS()
postscript(file = "figures/c1.eps", width = 2, height = 2)
par(bty = "n", las = 1,mar = c(2,2,0,0),
    ps = 8, tck = 0.001, mgp = c(3, 0.5, 0))
contour(x1, x1, surf, nlevels=5, col = grey(0.6))
abline(v = -0.02440308, h = 0.21061243, col = grey(0.6))
points(t(xlist[[1L]]), pch = 21, bg=grey(0.9), col = grey(.2))
invisible(dev.off())

setEPS()
postscript(file = "figures/c2.eps", width = 2, height = 2)
par(bty = "n", las = 1,mar = c(2,2,0,0),
    ps = 8, tck = 0.001, mgp = c(3, 0.5, 0))
contour(x1, x1, surf, nlevels=5, col = grey(0.6))
abline(v = -0.02440308, h = 0.21061243, col = grey(0.6))
points(t(xlist[[100L]]), pch = 21, bg=grey(0.9), col = grey(.2))
invisible(dev.off())

setEPS()
postscript(file = "figures/c3.eps", width = 2, height = 2)
par(bty="n", las = 1,mar = c(2,2,0,0),
    ps = 8, tck = 0.001, mgp = c(3, 0.5, 0))
contour(x1, x1, surf, nlevels=5, col = grey(0.6))
abline(v = -0.02440308, h = 0.21061243, col = grey(0.6))
points(t(xlist[[300L]]), pch = 21, bg=grey(0.9), col = grey(.2))
invisible(dev.off())
cat("\\includegraphics{figures/c1.eps}",
    "\\includegraphics{figures/c2.eps}",
    "\\includegraphics{figures/c3.eps}", sep = "")


###################################################
### code chunk number 91: NMOFman.Rnw:1596-1611
###################################################
OF <- function(par, Data) {
    ## compute model yields
    y <- Data$model(par, Data$tm)

    ## all rates finite?
    validRates <- !any(is.na(y))

    if (validRates) {
        ## any rates negative? if yes, add penalty
        pen1 <- sum(abs(y - abs(y))) * Data$ww

        F <- max(abs(y - Data$yM)) + pen1
    } else F <- 1e8
    F
}


###################################################
### code chunk number 92: NMOFman.Rnw:1618-1635
###################################################
algo <- list(nP = 200L, nG = 100L,
             F = 0.50, CR = 0.99,
             min = c( 0,-10,-10,  0),
             max = c( 1, 10, 10, 10),
             storeSolutions = TRUE, printBar = FALSE)


## set up yield curve and put information in Data
tm <- 1:20                ## times to maturity
parTRUE <- c(5, 3, 2, 1)  ## true parameters
yM <- NS(parTRUE, tm)     ## true market yields
Data <- list(yM = yM, tm = tm, model = NS, ww = 0.1, maxb1 = 4)

## solve with DEopt
sol <- DEopt(OF = OF, algo = algo, Data = Data)
P <- sol$xlist[[1L]] ## all population matrices
p1 <- sapply(P, `[`, 1L, TRUE)


###################################################
### code chunk number 93: NMOFman.Rnw:1642-1651
###################################################
par(bty = "n", las = 1, mar = c(4,4,0,0),
    ps = 8, tck = 0.001, mgp = c(3, 0.5, 0))
plot(jitter(rep(seq_len(algo$nG), each = algo$nP), factor = 5),
     p1,
     pch = 21, cex = 0.01, ylim = c(-5,10),
     xlab = "", ylab = "")

mtext("generation", 1, line = 2)
mtext("parameter\nvalue", 2, line = 1)


###################################################
### code chunk number 94: NMOFman.Rnw:1659-1694
###################################################
OF2 <- function(par, Data) {
    ## compute model yields
    y <- Data$model(par, Data$tm)

    ## all rates finite?
    validRates <- !any(is.na(y))

    if (validRates) {
        ## any rates negative? if yes, add penalty
        pen1 <- sum(abs(y - abs(y))) * Data$ww

        ## is b1 greater than Data$maxb1? if yes, add penalty
        pen2 <- par[1L] - Data$maxb1
        pen2 <- pen2 + abs(pen2)
        pen2 <- pen2

        F <- max(abs(y - Data$yM)) + pen1 + pen2
    } else F <- 1e8
    F
}

## solve with DEopt
sol <- DEopt(OF = OF2, algo = algo, Data = Data)
P <- sol$xlist[[1L]] ### all population matrices
p1 <- sapply(P, `[`, 1, TRUE)

par(bty = "n", las = 1, mar = c(4,4,0,0),
    ps = 8, tck = 0.001, mgp = c(3, 0.5, 0))
plot(jitter(rep(seq_len(algo$nG), each = algo$nP), factor = 5),
     p1,
     pch = 21, cex = 0.01, ylim = c(-5,10),
     xlab = "", ylab = "")
abline(h = 4, col=grey(0.5))
mtext("generation", 1, line = 2)
mtext("parameter\nvalue", 2, line = 1)


###################################################
### code chunk number 95: NMOFman.Rnw:1785-1786
###################################################
tfRosenbrock


###################################################
### code chunk number 96: NMOFman.Rnw:1793-1797
###################################################
OF <- tfRosenbrock     ## see ?testFunctions
size <- 5L             ## set dimension
x <- rep.int(1, size)  ## the known solution ...
OF(x)                  ## ... should give zero


###################################################
### code chunk number 97: NMOFman.Rnw:1805-1812
###################################################
algo <- list(printBar = FALSE,
             nP = 50L,
             nG = 500L,
             F = 0.6,
             CR = 0.9,
             min = rep(-100, size),
             max = rep( 100, size))


###################################################
### code chunk number 98: NMOFman.Rnw:1819-1825
###################################################
## a vectorised OF: works only with *matrix* x
OF2 <- function(x) {
    n <- dim(x)[1L]
    xi <- x[1L:(n - 1L), ]
    colSums(100 * (x[2L:n, ] - xi * xi)^2 + (1 - xi)^2)
}


###################################################
### code chunk number 99: NMOFman.Rnw:1830-1834
###################################################
x <- matrix(rnorm(size * algo$nP), size, algo$nP)
c(OF(x[ ,1L]), OF(x[ ,2L]), OF(x[ ,3L]))
OF2(x)[1L:3L]  ## should give the same result
all.equal(OF2(x)[1L:3L], c(OF(x[ ,1L]), OF(x[ ,2L]), OF(x[ ,3L])))


###################################################
### code chunk number 100: NMOFman.Rnw:1845-1851
###################################################
set.seed(1223445)
(t1 <- system.time(sol <- DEopt(OF = OF, algo = algo)))

algo$loopOF <- FALSE
set.seed(1223445)
(t2 <- system.time(sol2 <- DEopt(OF = OF2, algo = algo)))


###################################################
### code chunk number 101: NMOFman.Rnw:1856-1859
###################################################
sol$OFvalue    ## both should be zero (with luck)
sol2$OFvalue
t1[[3L]]/t2[[3L]]  ## speedup


###################################################
### code chunk number 102: NMOFman.Rnw:1891-1902
###################################################
na <- 100L  ## number of assets
np <- 100L  ## size of population
trials <- seq_len(100L)  ## for speed test

## a covariance matrix
Sigma <- array(0.7, dim = c(na, na)); diag(Sigma) <- 1

## set up population
W <- array(runif(na * np), dim = c(na, np))
## budget constraint
scaleFun <- function(x) x/sum(x); W <- apply(W, 2L, scaleFun)


###################################################
### code chunk number 103: NMOFman.Rnw:1906-1926
###################################################
## variant 1
t1 <- system.time({
    for (i in trials) {
        res1 <- numeric(np)
        for (j in seq_len(np)) {
            w <- W[ ,j]
            res1[j] <- w %*% Sigma %*% w
        }
    }
})

## variant 2
t2 <- system.time({
    for (i in trials) res2 <- diag(t(W) %*% Sigma %*% W)
})

## variant 3
t3 <- system.time({
    for (i in trials) res3 <- colSums(Sigma %*% W * W)
})


###################################################
### code chunk number 104: NMOFman.Rnw:1931-1933
###################################################
all.equal(res1,res2)
all.equal(res2,res3)


###################################################
### code chunk number 105: NMOFman.Rnw:1938-1941
###################################################
t1  ##  speedup for variant 1
t2  ##  speedup for variant 2
t3  ##  speedup for variant 3


###################################################
### code chunk number 106: NMOFman.Rnw:1965-1979
###################################################
n <- 100L   ## number of observation
p <- 5L     ## number of regressors
np <- 100L  ## population size
trials <- seq_len(1000L)

## random data
X <- array(rnorm(n * p), dim = c(n, p))
y <- rnorm(n)

## random population
Theta <- array(rnorm(p * np), dim = c(p, np))

## empty residuals matrix
R1 <- array(NA, dim = c(n, np))


###################################################
### code chunk number 107: NMOFman.Rnw:1983-1992
###################################################
system.time({
  for (i in trials)
  for (j in seq_len(np))
  R1[ ,j] <- y - X %*% Theta[ ,j]
})
system.time({
  for (i in trials)
  R2 <- y - X %*% Theta
})


###################################################
### code chunk number 108: NMOFman.Rnw:1999-2000
###################################################
all.equal(R1, R2)  ## ... should be TRUE


###################################################
### code chunk number 109: NMOFman.Rnw:2018-2028
###################################################
testFun <- function(x) {
    Sys.sleep(0.1) ## wasting time :-)
    cos(1/x^2)
}
system.time(sol1 <- bracketing(testFun, interval = c(0.3, 0.9),
                               n = 100L))
system.time(sol2 <- bracketing(testFun, interval = c(0.3, 0.9),
                               n = 100L, method = "snow", cl = 2))

all.equal(sol1, sol2)


###################################################
### code chunk number 110: NMOFman.Rnw:2055-2059
###################################################
testFun  <- function(x) {
  Sys.sleep(0.1) ## just wasting time :-)
  x[1L] + x[2L]^2
}


###################################################
### code chunk number 111: NMOFman.Rnw:2064-2068
###################################################
lower <- c(1, 3); upper <- 5; n <- 5L
system.time(sol1 <- gridSearch(fun = testFun,
                               lower = lower, upper = upper,
                               n = n, printDetail = TRUE))


###################################################
### code chunk number 112: NMOFman.Rnw:2073-2075
###################################################
seq(from = 1, to = 5, length.out= n)  ## x_1
seq(from = 3, to = 5, length.out= n)  ## x_2


###################################################
### code chunk number 113: NMOFman.Rnw:2080-2082
###################################################
sol1$minfun
sol1$minlevels


###################################################
### code chunk number 114: NMOFman.Rnw:2087-2094
###################################################
system.time(sol2 <- gridSearch(fun = testFun,
                               lower = lower,
                               upper = upper,
                               n = n, printDetail = FALSE,
                               method = "snow",  ## use 'snow' ...
                               cl = 2L))         ## ... with 2 cores
all.equal(sol1, sol2)


###################################################
### code chunk number 115: NMOFman.Rnw:2123-2124
###################################################
cfBSM


###################################################
### code chunk number 116: NMOFman.Rnw:2128-2147
###################################################
S <- 100    ## spot
X <- 100    ## strike
tau <- 1    ## time-to-maturity
r <- 0.02   ## interest rate
q <- 0.08   ## dividend rate
v <- 0.2    ## volatility

## the closed-form solution
callBSM <- function(S,X,tau,r,q,v) {
    d1 <- (log(S/X) + (r - q + v^2 / 2)*tau) / (v*sqrt(tau))
    d2 <- d1 - v*sqrt(tau)
    S * exp(-q * tau) * pnorm(d1) -  X * exp(-r * tau) * pnorm(d2)
}
callBSM(S,X,tau,r,q,v)

## with the characteristic function
callCF(cf = cfBSM, S = S, X = X, tau = tau, r = r, q = q,
       v = v^2,  ## variance, not vol
       implVol = TRUE)


###################################################
### code chunk number 117: NMOFman.Rnw:2193-2209
###################################################
mu <- 1
sigma <- 2
a <- -1
b <-  4

at <- (a - mu)/sigma
bt <- (b - mu)/sigma

## "throw away" strategy
x0 <- rnorm(10000L, mean = mu, sd = sigma)
x0 <- x0[x0>=a & x0<=b]

## inverse strategy
u <- runif(length(x0))
z <- qnorm(pnorm(at) + u*(pnorm(bt) - pnorm(at)))
x1 <- z * sigma + mu


###################################################
### code chunk number 118: NMOFman.Rnw:2214-2219
###################################################
par(mfrow = c(1, 2), mar = c(3, 3, 1, 1),
    bty = "n", las = 1, ps = 8, tck = 0.001, mgp = c(3, 0.5, 0))

hist(x0, xlab = "")
hist(x1, xlab = "")


###################################################
### code chunk number 119: NMOFman.Rnw:2223-2227
###################################################
x1 <- x1[1:750]
x2 <- rnorm(200)
x3 <- runif(500)
x4 <- rbinom(100, size = 50, prob = 0.4)


###################################################
### code chunk number 120: NMOFman.Rnw:2231-2233
###################################################
cormat <- array(0.5, dim = c(4, 4))
diag(cormat) <- 1


###################################################
### code chunk number 121: NMOFman.Rnw:2237-2240
###################################################
results <- resampleC(x1 = x1, x2 = x2, x3 = x3, x4 = x4,
                     size = 50, cormat = cormat)
cor(results, method = "spearman")


###################################################
### code chunk number 122: NMOFman.Rnw:2243-2257
###################################################
## this function is taken from ?pairs
panel.hist <- function(x, ...) {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1L], y, col = grey(.5))
}
par(mar = c(3, 3, 1, 1),
    bty = "n", las = 1, ps = 8, tck = 0.001, mgp = c(3, 0.5, 0))
pairs(results,
      diag.panel = panel.hist,
      gap = 0, pch = 19, cex = 0.5)


###################################################
### code chunk number 123: NMOFman.Rnw:2262-2272
###################################################
par(mfrow = c(2, 4), mar = c(3, 5, 1, 1),
    bty = "n", las = 1, ps = 8, tck = 0.001, mgp = c(3, 0.5, 0))
hist(x1, xlab = "", ylab = "original")
hist(x2, xlab = "", ylab = "")
hist(x3, xlab = "", ylab = "")
hist(x4, xlab = "", ylab = "")
hist(results[ ,"x1"], xlab = "", ylab = "resampled")
hist(results[ ,"x2"], xlab = "", ylab = "")
hist(results[ ,"x3"], xlab = "", ylab = "")
hist(results[ ,"x4"], xlab = "", ylab = "")


###################################################
### code chunk number 124: NMOFman.Rnw:2296-2306
###################################################
## number of assets
na <- 500L

## correlation matrix
C <- array(0.6, dim = c(na,na)); diag(C) <- 1

## covariance matrix
minVol <- 0.20; maxVol <- 0.40
Vols <- (maxVol - minVol) * runif(na) + minVol
Sigma <- outer(Vols, Vols) * C


###################################################
### code chunk number 125: NMOFman.Rnw:2318-2324
###################################################
OF <- function(x, data) {
    sx <- sum(x)
    w <- rep.int(1/sx, sx)
    res <- crossprod(w, data$Sigma[x, x])
    tcrossprod(w, res)
}


###################################################
### code chunk number 126: NMOFman.Rnw:2328-2337
###################################################
neighbour <- function(xc, data) {
    xn <- xc
    p <- sample.int(data$na, data$nn, replace = FALSE)
    xn[p] <- !xn[p]
    ## reject infeasible solution
    sumx <- sum(xn)
    if ( (sumx > data$Ksup) || (sumx < data$Kinf) )
        xc else xn
}


###################################################
### code chunk number 127: NMOFman.Rnw:2344-2349
###################################################
data <- list(Sigma = Sigma,   ## cov-matrix
             Kinf  = 30L,     ## min cardinality
             Ksup  = 60L,     ## max cardinality
             na    = na,      ## number of assets
             nn    = 1L)      ## how many assets to change per iteration


###################################################
### code chunk number 128: NMOFman.Rnw:2353-2357
###################################################
card0 <- sample(data$Kinf:data$Ksup, 1L, replace = FALSE)
assets <- sample.int(na, card0, replace = FALSE)
x0 <- logical(na)
x0[assets] <- TRUE


###################################################
### code chunk number 129: NMOFman.Rnw:2363-2371
###################################################
## Local Search
algo <- list(x0 = x0, neighbour = neighbour, nS = 5000L,
             printDetail = FALSE, printBar = FALSE)
system.time(solLS <- LSopt(OF, algo = algo, data = data))

## Threshold Accepting
algo$nT <- 10L; algo$nS <- trunc(algo$nS/algo$nT); algo$q <- 0.2
system.time(solTA <- TAopt(OF, algo = algo, data = data))


###################################################
### code chunk number 130: NMOFman.Rnw:2453-2460
###################################################
OF2 <- function(x, data) {
    res <- colSums(data$Sigma %*% x * x)
    n <- colSums(x); res <- res / n^2
    ## penalise
    p <- pmax(data$Kinf - n, 0) + pmax(n - data$Ksup, 0)
    res + p
}


###################################################
### code chunk number 131: NMOFman.Rnw:2467-2470
###################################################
algo <- list(nB = na, nP = 100L, nG = 500L, prob = 0.002,
             printBar = FALSE, loopOF = FALSE)
system.time(solGA <- GAopt(OF = OF2, algo = algo, data = data))


###################################################
### code chunk number 132: NMOFman.Rnw:2475-2479
###################################################
cat("Local Search        ", format(sqrt(solLS$OFvalue), digits = 4), "\n",
    "Threshold Accepting ", format(sqrt(solTA$OFvalue), digits = 4), "\n",
    "Genetic Algorithm   ", format(sqrt(solGA$OFvalue), digits = 4), "\n",
    sep = "")


###################################################
### code chunk number 133: NMOFman.Rnw:2507-2515
###################################################
na <- 100L                                 ## number of assets
ns <- 200L                                 ## number of scenarios
vols <- runif(na, min = 0.2, max = 0.4)    ## marginal vols
C <- matrix(0.6, na, na); diag(C) <- 1     ## correlation matrix
R <- rnorm(ns * na)/16                     ## random returns
dim(R) <- c(ns, na)
R <- R %*% chol(C)
R <- R %*% diag(vols)


###################################################
### code chunk number 134: NMOFman.Rnw:2540-2550
###################################################
data <- list(R = t(R),              ## scenarios
             theta = 0.005,         ## return threshold
             na = na,               ## number of assets
             ns = ns,               ## number of scenarios
             max = rep( 0.05, na),  ## DE: vector of max. weight
             min = rep(-0.05, na),  ## DE: vector of min. weight
             wsup =  0.05,          ## TA: max weight
             winf = -0.05,          ## TA: min weight
             eps = 0.5/100,         ## TA: step size
             w = 1)                 ## penalty weight


###################################################
### code chunk number 135: NMOFman.Rnw:2557-2560
###################################################
x0 <- data$min + runif(data$na)*(data$max - data$min)
x0[1:5]
sum(x0)


###################################################
### code chunk number 136: NMOFman.Rnw:2566-2570
###################################################
temp <- R %*% x0             ## compute portfolio returns
temp <- temp - data$theta    ## subtract return threshold
temp <- (temp[temp < 0])^2   ## select elements below threshold
sum(temp)/ns                 ## compute semivariance


###################################################
### code chunk number 137: NMOFman.Rnw:2576-2583
###################################################
OF <- function(x, data) {
    Rx <- crossprod(data$R, x)
    Rx <- Rx - data$theta
    Rx <- Rx - abs(Rx)
    Rx <- Rx * Rx
    colSums(Rx) /(4*data$ns)
}


###################################################
### code chunk number 138: NMOFman.Rnw:2592-2594
###################################################
OF(x0, data)
OF(cbind(x0, x0, x0), data)


###################################################
### code chunk number 139: NMOFman.Rnw:2603-2616
###################################################
repair <- function(x, data) {
    myFun <- function(x)
        x/sum(x)
    if (is.null(dim(x)[2L]))
        myFun(x) else apply(x, 2L, myFun)
}

repair2 <- function(x, data) {
    myFun <- function(x)
        x + (1 - sum(x))/data$na
    if (is.null(dim(x)[2L]))
        myFun(x) else apply(x, 2L, myFun)
}


###################################################
### code chunk number 140: NMOFman.Rnw:2622-2628
###################################################
sum(x0)
sum(repair(x0, data))
sum(repair2(x0, data))

colSums(repair( cbind(x0, x0, x0), data))
colSums(repair2(cbind(x0, x0, x0), data))


###################################################
### code chunk number 141: NMOFman.Rnw:2634-2636
###################################################
summary(repair (x0, data)-x0)
summary(repair2(x0, data)-x0)


###################################################
### code chunk number 142: NMOFman.Rnw:2641-2652
###################################################
penalty <- function(x, data) {
    up <- data$max
    lo <- data$min
    xadjU <- x - up
    xadjU <- xadjU + abs(xadjU)
    xadjL <- lo - x
    xadjL <- xadjL + abs(xadjL)
    if (is.null(dim(x)[2L]))
        data$w * (sum(xadjU) + sum(xadjL)) else
    data$w * (colSums(xadjU) + colSums(xadjL))
}


###################################################
### code chunk number 143: NMOFman.Rnw:2660-2666
###################################################
x0[1L] <- 0.30
penalty(x0, data)
penalty(cbind(x0, x0, x0), data)
x0[1L] <- 0
penalty(x0, data)
penalty(cbind(x0, x0, x0), data)


###################################################
### code chunk number 144: NMOFman.Rnw:2671-2684
###################################################
algo <- list(nP = 100,        ## population size
             nG = 1000,       ## number of generations
             F = 0.25,        ## step size
             CR = 0.9,
             min = data$min,
             max = data$max,
             repair = repair,
             pen = penalty,
             printBar = FALSE,
             printDetail = TRUE,
             loopOF = TRUE,      ## do not vectorise
             loopPen = TRUE,     ## do not vectorise
             loopRepair = TRUE)  ## do not vectorise


###################################################
### code chunk number 145: NMOFman.Rnw:2690-2697
###################################################
system.time(sol <- DEopt(OF = OF,algo = algo,data = data))
16 * 100 * sqrt(sol$OFvalue)   ## solution quality

## check constraints
all(all.equal(sum(sol$xbest), 1),  ## budget constraint
    sol$xbest <= data$max,         ## holding size constraints
    sol$xbest >= data$min)


###################################################
### code chunk number 146: NMOFman.Rnw:2707-2717
###################################################
## looping over the population
algo$loopOF <- TRUE; algo$loopPen <- TRUE; algo$loopRepair <- TRUE
t1 <- system.time(sol <- DEopt(OF = OF,algo = algo, data = data))

## evaluating the population in one step
algo$loopOF <- FALSE; algo$loopPen <- FALSE; algo$loopRepair <- FALSE
t2 <- system.time(sol <- DEopt(OF = OF,algo = algo, data = data))

## speedup
t1[[3L]]/t2[[3L]]


###################################################
### code chunk number 147: NMOFman.Rnw:2728-2741
###################################################
algo$printDetail <- FALSE
restartsDE <- restartOpt(fun = DEopt,      ## what function
                         n = 20L,          ## how many restarts
                         OF = OF,
                         algo = algo,
                         data = data,
                         method = "snow",  ## using package snow
                         cl = 2)           ## 2 cores

## extract best solution
OFvaluesDE <- sapply(restartsDE, `[[`, "OFvalue")
OFvaluesDE <- 16 * 100 * sqrt(OFvaluesDE)
weightsDE  <- sapply(restartsDE, `[[`, "xbest")


###################################################
### code chunk number 148: NMOFman.Rnw:2746-2751
###################################################
par(bty = "n", las = 1, mar = c(3, 4, 0, 0),
    ps = 8, tck = 0.001)
plot(sort(OFvaluesDE), (seq_len(length(OFvaluesDE))) / length(OFvaluesDE),
     type = "S", ylim = c(0, 1), xlab = "", ylab = "")
mtext("OF value",  side = 1, line = 2)


###################################################
### code chunk number 149: NMOFman.Rnw:2755-2761
###################################################
par(bty = "n", las = 1, mar = c(3, 4, 0, 0),
    ps = 8, tck = 0.001)
boxplot(t(weightsDE),
        outline = FALSE, boxwex = 0.4, ylim = c(-0.06,0.06))
mtext("assets",  side = 1, line = 2)
mtext("weights", side = 2, line = 1.3, las = 1, padj = -4)


###################################################
### code chunk number 150: NMOFman.Rnw:2774-2789
###################################################
algo$printDetail <- FALSE;  algo$nP <- 200L; restarts <- 20L
nGs <- c(500L, 1500L, 3000L)
lstOFvaluesDE <- list()
for (i in 1:3) {
    algo$nG <- nGs[i]
    restartsDE <- restartOpt(fun = DEopt,
                             n = restarts,
                             OF = OF,  algo = algo, data = data,
                             method = "snow", cl = 2)
    ## extract best solution
    OFvaluesDE <- sapply(restartsDE, `[[`, "OFvalue")
    OFvaluesDE <- 16 * 100 * sqrt(OFvaluesDE)
    lstOFvaluesDE[[i]] <- OFvaluesDE
}
res <- simplify2array(lstOFvaluesDE)


###################################################
### code chunk number 151: NMOFman.Rnw:2794-2808
###################################################
algo$repair <- repair2
lstOFvaluesDE <- list()
for (i in 1:3) {
    algo$nG <- nGs[i]
    restartsDE <- restartOpt(fun = DEopt,
                             n = restarts,
                             OF = OF,  algo = algo, data = data,
                             method = "snow", cl = 2)
    ## extract best solution
    OFvaluesDE <- sapply(restartsDE, `[[`, "OFvalue")
    OFvaluesDE <- 16 * 100 * sqrt(OFvaluesDE)
    lstOFvaluesDE[[i]] <- OFvaluesDE
}
res2 <- simplify2array(lstOFvaluesDE)


###################################################
### code chunk number 152: NMOFman.Rnw:2813-2823
###################################################
allres <- as.vector(rbind(res,res2))
xlims <- pretty(allres); xlims <- c(min(xlims), max(xlims))
par(bty = "n", las = 1, mar = c(3, 4, 0, 0),
    ps = 8, tck = 0.001)
plot(ecdf(res[ ,3L]), xlim = xlims, cex = 0.4,
     main = "", ylab = "", xlab = "")
for (i in 1:2)
    lines(ecdf(res[  ,i]), cex = 0.4)
for (i in 1:3)
    lines(ecdf(res2[ ,i]), col = "blue", cex = 0.4)


###################################################
### code chunk number 153: NMOFman.Rnw:2833-2840
###################################################
weightsDE <- sapply(restartsDE, `[[`, "xbest")
par(bty = "n", las = 1, mar = c(3, 4, 0, 0),
ps = 8, tck = 0.001)
boxplot(t(weightsDE),
outline = FALSE, boxwex = 0.4, ylim = c(-0.06, 0.06))
mtext("assets",  side = 1, line = 2)
mtext("weights", side = 2, line = 1.3, las = 1, padj = -4)


###################################################
### code chunk number 154: NMOFman.Rnw:2847-2864
###################################################
algo <- list(nP = 100L,      ## population size
             nG = 1000L,     ## number of generations
             c1 = 0.5,       ## weight for individually best solution
             c2 = 1.5,       ## weight for overall best solution
             min = data$min,
             max = data$max,
             repair = repair, pen = penalty,
             iner = 0.7, initV = 1, maxV = 0.2,
             printBar = FALSE, printDetail = TRUE)

system.time(sol <- PSopt(OF = OF,algo = algo,data = data))
16 * 100 * sqrt(sol$OFvalue)      ## solution quality

## check constraints
all(all.equal(sum(sol$xbest),1),  ## budget constraint
sol$xbest <= data$max,
sol$xbest >= data$min)


###################################################
### code chunk number 155: NMOFman.Rnw:2872-2879
###################################################
changeV <- function(x, data) {
    myFun <- function(x) x - (sum(x))/data$na
    if (is.null(dim(x)[2L]))
        myFun(x) else apply(x, 2L, myFun)
}
sum(changeV(x0, data))
colSums(changeV(cbind(x0, x0, x0), data))


###################################################
### code chunk number 156: NMOFman.Rnw:2884-2888
###################################################
initP <- data$min + diag(data$max - data$min) %*%
    array(runif(length(data$min) * algo$nP),
          dim = c(length(data$min),  algo$nP))
colSums(initP <- repair(initP,data))[1:10] ## check


###################################################
### code chunk number 157: NMOFman.Rnw:2894-2900
###################################################
algo$changeV <- changeV        ## function to adjust velocity
algo$initP <- initP            ## initial population
algo$repair <- NULL            ## not needed anymore

system.time(sol <- PSopt(OF = OF,algo = algo, data = data))
16 * 100 * sqrt(sol$OFvalue)   ## solution quality


###################################################
### code chunk number 158: NMOFman.Rnw:2905-2908
###################################################
all(all.equal(sum(sol$xbest), 1), ## budget constraint
sol$xbest <= data$max,
sol$xbest >= data$min)


###################################################
### code chunk number 159: NMOFman.Rnw:2911-2914
###################################################
algo$loopOF <- FALSE; algo$loopPen <- FALSE
algo$loopRepair <- FALSE; algo$loopChangeV <- FALSE
system.time(sol <- PSopt(OF = OF, algo = algo, data = data))


###################################################
### code chunk number 160: NMOFman.Rnw:2919-2935
###################################################
algo$printDetail <- FALSE
restartsPS <- restartOpt(fun = PSopt,
                         n = 20L,
                         OF = OF,
                         algo = algo, data = data,
                         method = "snow", cl = 2)

## extract best solution
OFvaluesPS <- sapply(restartsPS, `[[`, "OFvalue")
OFvaluesPS <- 16 * 100 * sqrt(OFvaluesPS)
par(bty = "n", las = 1,mar = c(3,4,0,0),
    ps = 8, tck = 0.001)
plot(sort(OFvaluesPS),
     (seq_len(length(OFvaluesPS))) / length(OFvaluesPS),
     type = "S", ylim = c(0, 1), xlab = "", ylab = "")
mtext("OF value",  side = 1, line = 2)


###################################################
### code chunk number 161: NMOFman.Rnw:2946-2969
###################################################
data$R <- R  ## not transposed any more

neighbourU <- function(sol, data){
    resample <- function(x, ...)
        x[sample.int(length(x), ...)]
    wn <- sol$w
    toSell <- wn > data$winf
    toBuy  <- wn < data$wsup
    i <- resample(which(toSell), size = 1L)
    j <- resample(which(toBuy), size = 1L)
    eps <- runif(1) * data$eps
    eps <- min(wn[i] - data$winf, data$wsup - wn[j], eps)
    wn[i] <- wn[i] - eps
    wn[j] <- wn[j] + eps
    Rw <- sol$Rw + data$R[,c(i,j)] %*% c(-eps,eps)
    list(w = wn, Rw = Rw)
}
OF <- function(x, data) {
    Rw <- x$Rw - data$theta
    Rw <- Rw - abs(Rw)
    sum(Rw*Rw) / (4*data$ns)
}



###################################################
### code chunk number 162: NMOFman.Rnw:2975-2988
###################################################
## a random initial weights
w0 <- runif(data$na); w0 <- w0/sum(w0)
x0 <- list(w = w0, Rw = R %*% w0)
algo <- list(x0 = x0,
             neighbour = neighbourU,
             nS = 2000L,
             nT = 10L,
             nD = 5000L,
             q = 0.20,
             printBar = FALSE,
             printDetail = FALSE)
system.time(sol2 <- TAopt(OF,algo,data))
16 * 100 * sqrt(sol2$OFvalue)


###################################################
### code chunk number 163: NMOFman.Rnw:2994-3016
###################################################
restartsTA <- restartOpt(fun = TAopt,
                         n = 20L,
                         OF = OF,
                         algo = algo,
                         data = data,
                         method = "snow",
                         cl = 2)

OFvaluesTA <- sapply(restartsTA, `[[`, "OFvalue") ## extract best solution
OFvaluesTA <- 16 * 100 * sqrt(OFvaluesTA)
weightsTA <- sapply(restartsTA, `[[`, "xbest")
par(bty = "n", las = 1,mar = c(3,4,0,0), ps = 8,
    tck = 0.001, mgp = c(3, 0.5, 0))

## blue: DE solution with nP = 200 and nG = 2000
xlims <- pretty(c(res2[,3], OFvaluesTA))
plot(ecdf(res2[,3]), col = "blue", cex = 0.4,
     main = "", ylab = "", xlab = "",
     xlim = c(min(xlims), max(xlims)) )

## black: TA
lines(ecdf(OFvaluesTA), cex = 0.4)


###################################################
### code chunk number 164: NMOFman.Rnw:3051-3058
###################################################
size <- 20L
x <- logical(size)
x[runif(size) > 0.5] <- TRUE

## store information
Data <- list()
Data$size <- size


###################################################
### code chunk number 165: NMOFman.Rnw:3062-3070
###################################################
compareLogicals <- function(x, y, ...) {
    argsL <- list(...)
    if (!("sep" %in% names(argsL))) argsL$sep <- ""
    do.call("cat",
            c(list("\n",as.integer(x), "\n", as.integer(y), "\n",
                   ifelse(x == y, " ", "^"), "\n"), argsL))
    message("The vectors differ in ", sum(x!=y), " places.")
}


###################################################
### code chunk number 166: NMOFman.Rnw:3074-3080
###################################################
## there should be no difference
compareLogicals(x, x)

## change the second element
z <- x; z[2L] <- !z[2L]
compareLogicals(x, z)


###################################################
### code chunk number 167: NMOFman.Rnw:3089-3096
###################################################
Data$n <- 5L  ## how many elements to change
neighbour <- function(x, Data) {
    ii <- sample.int(Data$size, Data$n)
    x[ii] <- !x[ii]
    x
}
compareLogicals(x, neighbour(x, Data))


###################################################
### code chunk number 168: NMOFman.Rnw:3106-3117
###################################################
neighbour <- function(x, Data) {
    ## required: x must have at least one TRUE and one FALSE
    Ts <- which(x)
    Fs <- which(!x)
    lenTs <- length(Ts)
    O <- sample.int(lenTs,  1L)
    I <- sample.int(Data$size - lenTs, 1L)
    x[c(Fs[I], Ts[O])] <- c(TRUE, FALSE)
    x
}
compareLogicals(x, neighbour(x, Data))


###################################################
### code chunk number 169: NMOFman.Rnw:3124-3142
###################################################
size <- 5L
x0 <- runif(size)
xTRUE <- runif(size)
data <- list(xTRUE = xTRUE,
             step = 0.02)
OF <- function(x, data)
    max(abs(x - data$xTRUE))
neighbour <- function(x, data)
    x + runif(length(data$xTRUE))*data$step - data$step/2

algo <- list(q = 0.05, nS = 1000L, nT = 10L,
             neighbour = neighbour, x0 = x0,
             printBar = FALSE,
             printDetail = FALSE,
             storeSolutions = TRUE,
             storeF = TRUE)
res <- TAopt(OF, algo = algo, data = data)
res$OFvalue < 0.005


###################################################
### code chunk number 170: NMOFman.Rnw:3163-3428
###################################################
## N1: This neighbour enforces a budget constraint, a minimum
## holding size (which need not be zero) and a maximum holding size

Data <- list(wmin = 0,     ## the minimal weight
             wmax = 0.22,  ## the maximal weight
             eps = 0.2/100,  ## the step size
             resample = function(x, ...)
                        x[sample.int(length(x), ...)],
             na = dim(fundData)[2L],
             R = fundData)

cat("The portfolio will consist of at least ",
    ceiling(1/Data$wmax), " assets.\n", sep = "")

neighbour1 <- function(w, Data){
    toSell <- w > Data$wmin
    toBuy  <- w < Data$wmax
    i <- Data$resample(which(toSell), size = 1L)
    j <- Data$resample(which(toBuy),  size = 1L)
    eps <- runif(1) * Data$eps
    eps <- min(w[i] - Data$wmin, Data$wmax - w[j], eps)
    w[i] <- w[i] - eps
    w[j] <- w[j] + eps
    w
}
neighbour1U <- function(x, Data){
    wn <- x$w
    toSell <- wn > Data$wmin
    toBuy  <- wn < Data$wmax
    i <- Data$resample(which(toSell), size = 1L)
    j <- Data$resample(which(toBuy),  size = 1L)
    eps <- runif(1) * Data$eps
    eps <- min(wn[i] - Data$wmin, Data$wmax - wn[j], eps)
    wn[i] <- wn[i] - eps
    wn[j] <- wn[j] + eps
    Rw <- x$Rw + Data$R[ ,c(i,j)] %*% c(-eps,eps)
    list(w = wn, Rw = Rw)
}

## create a random solution
makex <- function(Data) {
    w0 <- numeric(Data$na)
    nAssets <- Data$resample(ceiling(1/Data$wmax):Data$na, 1L)
    w0[sample(seq_len(Data$na), nAssets)] <- runif(nAssets)
    w0/sum(w0)
}

isOK <- function(w, Data) {
    tooBig   <- any(w > Data$wmax)
    tooSmall <- any(w < Data$wmin)
    sumToOne <- abs(sum(w)-1) < 1e-12
    if (!tooBig && !tooSmall && sumToOne)
        TRUE
    else
        FALSE
}
## TEST 1
w0 <- makex(Data)
x0 <- list(w = w0, Rw = fundData %*% w0)
isOK(w0, Data)
isOK(x0$w, Data)

set.seed(545)
w0 <- makex(Data)
nTests <- 1e3
for (i in seq(nTests)) {
    w1 <- neighbour1(w0,  Data)
    if (isOK(w1, Data))
        w0 <- w1
    else
        stop("error")
}
set.seed(545)
w0 <- makex(Data)
x0 <- list(w = w0, Rw = fundData %*% w0)
nTests <- 1e3
for (i in seq(nTests)) {
    x1 <- neighbour1U(x0,  Data)
    if (isOK(x1$w, Data))
        x0 <- x1
    else
        stop("error")
}
all.equal(fundData %*% w1, x1$Rw)


## TEST 2: reach a target solution
makeOF <- function(wt)
    function(w0, Data)
        sum(abs(wt - w0))
wt <- makex(Data)
OF <- makeOF(wt)
w0 <- makex(Data)
OF(w0, Data)
TAsettings <- list(neighbour = neighbour1,
                   x0 = w0, nS = 5000, q = 0.1,
                   printBar = FALSE)
res <- TAopt(OF, algo = TAsettings, Data)
round(head(sort(abs(res$xbest-wt), decreasing = TRUE),5),6)




## N2: This long-only neighbour enforces a budget constraint, a
## minimum holding size (which need not be zero), and a maximum holding
## size and a maximum cardinality.

Data <- list(wmax = 0.3, ## the maximal weight
             Kmax = 10,  ## max cardinality
             eps = 1/100, ## the step size
             resample = function(x, ...)
                            x[sample.int(length(x), ...)],
             na = dim(fundData)[2L],
             R = fundData)
cat("The portfolio will consist of at least ",
    ceiling(1/Data$wmax), " assets.\n", sep = "")

neighbour2 <- function(w, Data){
    tol <- 1e-12
    J <- sum(w > tol)
    if (J == Data$Kmax)
        toBuy <- w > tol & w < Data$wmax
    else
        toBuy <- w < Data$wmax
    toSell <- w > tol
    i <- Data$resample(which(toSell), size = 1L)
    j <- Data$resample(which(toBuy),  size = 1L)
    eps <- runif(1) * Data$eps
    eps <- min(w[i], Data$wmax - w[j], eps)
    w[i] <- w[i] - eps
    w[j] <- w[j] + eps
    w
}
neighbour2U <- function(x, Data){
    tol <- 1e-12
    w <- x$w
    J <- sum(w > tol)
    if (J == Data$Kmax)
        toBuy <- w > tol & w < Data$wmax
    else
        toBuy <- w < Data$wmax
    toSell <- w > tol
    i <- Data$resample(which(toSell), size = 1L)
    j <- Data$resample(which(toBuy),  size = 1L)
    eps <- runif(1) * Data$eps
    eps <- min(w[i], Data$wmax - w[j], eps)
    w[i] <- w[i] - eps
    w[j] <- w[j] + eps
    Rw <- x$Rw + Data$R[ ,c(i,j)] %*% c(-eps, eps)
    list(w = w, Rw = Rw)
}


makex <- function(Data) {
    w0 <- numeric(Data$na)
    nAssets <- sample(ceiling(1/Data$wmax):Data$Kmax, 1L)
    w0[sample(seq_len(Data$na), nAssets)] <- runif(nAssets)
    w0/sum(w0)
}

isOK <- function(w, Data) {
    tooBig   <- any(w > Data$wmax)
    tooMany <- sum(w > 1e-12) > Data$Kmax
    sumToOne <- abs(sum(w)-1) < 1e-12
    if (!tooBig && !tooMany && sumToOne)
        TRUE
    else
        FALSE
}

## TEST 1
w0 <- makex(Data)
x0 <- list(w = w0, Rw = fundData %*% w0)
isOK(w0, Data)
isOK(x0$w, Data)

set.seed(545)
w0 <- makex(Data)
nTests <- 1e3
for (i in seq(nTests)) {
    w1 <- neighbour2(w0,  Data)
    if (isOK(w1, Data))
        w0 <- w1
    else
        stop("error")
}
set.seed(545)
w0 <- makex(Data)
x0 <- list(w = w0, Rw = fundData %*% w0)
nTests <- 1e3
for (i in seq(nTests)) {
    x1 <- neighbour2U(x0,  Data)
    if (isOK(x1$w, Data))
        x0 <- x1
    else
        stop("error")
}

all.equal(fundData %*% w1, x1$Rw)

## TEST 2: reach a target solution
makeOF <- function(wt)
    function(w0, Data)
        sum(abs(wt - w0))
wt <- makex(Data)
OF <- makeOF(wt)
w0 <- makex(Data)
OF(w0, Data)
OF(wt, Data)

TAsettings <- list(neighbour = neighbour2,
                   x0 = w0, nS = 5000, q = 0.1,
                   printBar = FALSE)
res <- TAopt(OF, algo = TAsettings, Data)
isOK(res$xbest, Data)

df <- data.frame(target=wt, w0 = w0, wTAopt = res$xbest)
tmpfun <- function(x)
    !all(abs(x) < 1e-14)
df[apply(df,1,tmpfun),]
apply(df, 2, sum)

wt <- numeric(200)
wt[1:4] <- c(0.3,0.3,0.3,0.1)
OF <- makeOF(wt)

TAsettings <- list(neighbour = neighbour2,
                   x0 = w0, nS = 5000, q = 0.1,
                   printBar = FALSE)
res <- TAopt(OF, algo = TAsettings, Data)
isOK(res$xbest, Data)
df <- data.frame(target=wt, w0 = w0, wTAopt = res$xbest)
tmpfun <- function(x)
    !all(abs(x) < 1e-14)
df[apply(df,1,tmpfun),]
apply(df, 2, sum)


w0 <- makex(Data)
x0 <- list(w = w0, Rw = fundData %*% w0)

## the N is slower
system.time(for (i in 1:10000) neighbour2(w0, Data))
system.time(for (i in 1:10000) neighbour2U(x0, Data))


TAsettings2 <- list(neighbour = neighbour2,
                    x0 = w0, nS = 500, q = 0.1,
                    printBar = FALSE, printDetail = FALSE)
TAsettings2U <- list(neighbour = neighbour2U,
                     x0 = x0, nS = 500, q = 0.1,
                     printBar = FALSE, printDetail = FALSE)

ofun <- function(w, Data) {
    Rw <- Data$R %*% w
    crossprod(Rw)
}
ofunU <- function(sol, Data)
    crossprod(sol$Rw)
ign <- TAopt(ofun, TAsettings2, Data)
ign <- TAopt(ofunU, TAsettings2U, Data)

##benchmark(ign <- TAopt(ofun, TAsettings2, Data),
##          ign <- TAopt(ofunU, TAsettings2U, Data),
##          replications = 1, order = "relative")


###################################################
### code chunk number 171: NMOFman.Rnw:3472-3483
###################################################
makeCashFlows <- function(coupon, T) {
    t1 <- T - floor(T)                  ## time to first coupon
    tm <- seq(ifelse(t1 > 1e-5, t1, 1), ## 1e-5 is less than a calendar day
              T,
              by = 1)
    cf <- rep.int(coupon, length(tm))
    cf[length(cf)] <- cf[length(cf)] + 100
    list(cf = cf, tm = tm)
}

makeCashFlows(3, 10.2)


###################################################
### code chunk number 172: NMOFman.Rnw:3487-3507
###################################################
cf1 <- c(rep(5.75,  8), 105.75); tm1 <- 0:8 + 0.5
cf2 <- c(rep(4.25, 17), 104.25); tm2 <- 1:18
cf3 <- c(3.5, 103.5); tm3 <- 0:1 + 0.5
cf4 <- c(rep(3.00, 15), 103.00); tm4 <- 1:16
cf5 <- c(rep(3.25, 11), 103.25); tm5 <- 0:11 + 0.5
cf6 <- c(rep(5.75, 17), 105.75); tm6 <- 0:17 + 0.5
cf7 <- c(rep(3.50, 14), 103.50); tm7 <- 1:15
cf8 <- c(rep(5.00,  8), 105.00); tm8 <- 0:8 + 0.5
cf9 <- 105; tm9 <- 1
cf10 <- c(rep(3.00, 12), 103.00); tm10 <- 0:12 + 0.5
cf11 <- c(rep(2.50,  7), 102.50); tm11 <- 1:8
cf12 <- c(rep(4.00, 10), 104.00); tm12 <- 1:11
cf13 <- c(rep(3.75, 18), 103.75); tm13 <- 0:18 + 0.5
cf14 <- c(rep(4.00, 17), 104.00); tm14 <- 1:18
cf15 <- c(rep(2.25,  8), 102.25); tm15 <- 0:8 + 0.5
cf16 <- c(rep(4.00,  6), 104.00); tm16 <- 1:7
cf17 <- c(rep(2.25, 12), 102.25); tm17 <- 1:13
cf18 <- c(rep(4.50, 19), 104.50); tm18 <- 0:19 + 0.5
cf19 <- c(rep(2.25,  7), 102.25); tm19 <- 1:8
cf20 <- c(rep(3.00, 14), 103.00); tm20 <- 1:15


###################################################
### code chunk number 173: NMOFman.Rnw:3513-3528
###################################################
cfList <- list( cf1, cf2, cf3, cf4, cf5, cf6, cf7, cf8, cf9,cf10,
               cf11,cf12,cf13,cf14,cf15,cf16,cf17,cf18,cf19,cf20)
tmList <- list( tm1, tm2, tm3, tm4, tm5, tm6, tm7, tm8, tm9,tm10,
               tm11,tm12,tm13,tm14,tm15,tm16,tm17,tm18,tm19,tm20)
tm <- unlist(tmList, use.names = FALSE)
tm <- sort(unique(tm))
nR <- length(tm)
nC <- length(cfList)

cfMatrix <- array(0, dim = c(nR, nC))
for(j in seq(nC))
    cfMatrix[tm %in% tmList[[j]], j] <- cfList[[j]]
rownames(cfMatrix) <- tm

cfMatrix[1:10, 1:10]


###################################################
### code chunk number 174: NMOFman.Rnw:3535-3539
###################################################
betaTRUE <- c(5,-2,1,10,1,3)
yM <- NSS(betaTRUE,tm)
diFa <- 1 / ( (1 + yM/100)^tm )
bM <- diFa %*% cfMatrix


###################################################
### code chunk number 175: NMOFman.Rnw:3543-3547
###################################################
data <- list(bM = bM, tm = tm, cfMatrix = cfMatrix, model = NSS,
             ww = 1,
             min = c( 0,-15,-30,-30,0  ,2.5),
             max = c(15, 30, 30, 30,2.5,5  ))


###################################################
### code chunk number 176: NMOFman.Rnw:3557-3567
###################################################
OF2 <- function(param, data) {
  tm <- data$tm
  bM <- data$bM
  cfMatrix <- data$cfMatrix
  diFa  <- 1 / ((1 + data$model(param, tm)/100)^tm)
  b <- diFa %*% cfMatrix
  aux <- b - bM; aux <- max(abs(aux))
  if (is.na(aux)) aux <- 1e10
  aux
}


###################################################
### code chunk number 177: NMOFman.Rnw:3571-3586
###################################################
penalty <- function(mP, data) {
    minV <- data$min
    maxV <- data$max
    ww <- data$ww
    ## if larger than maxV, element in A is positiv
    A <- mP - as.vector(maxV)
    A <- A + abs(A)
    ## if smaller than minV, element in B is positiv
    B <- as.vector(minV) - mP
    B <- B + abs(B)
    ## beta 1 + beta2 > 0
    C <- ww*((mP[1L, ] + mP[2L, ]) - abs(mP[1L, ] + mP[2L, ]))
    A <- ww * colSums(A + B) - C
    A
}


###################################################
### code chunk number 178: NMOFman.Rnw:3591-3607
###################################################
algo <- list(nP  = 200L,
             nG  = 1000L,
             F   = 0.50,
             CR  = 0.99,
             min = c( 0,-15,-30,-30,0  ,2.5),
             max = c(15, 30, 30, 30,2.5,5  ),
             pen = penalty,
             repair = NULL,
             loopOF = TRUE,
             loopPen = FALSE,
             loopRepair = FALSE,
             printBar = FALSE,
             printDetail = FALSE,
             storeF = FALSE)

sol <- DEopt(OF = OF2, algo = algo, data = data)


###################################################
### code chunk number 179: NMOFman.Rnw:3614-3616
###################################################
max( abs(data$model(sol$xbest, tm) - data$model(betaTRUE, tm)))
sol$OFvalue


###################################################
### code chunk number 180: NMOFman.Rnw:3621-3638
###################################################
s0 <- algo$min + (algo$max - algo$min) * runif(length(algo$min))
system.time(sol2 <- nlminb(s0,OF2,data = data,
                           lower = data$min,
                           upper = data$max,
                           control = list(eval.max = 50000,
                           iter.max = 50000)))
max(abs(data$model(sol2$par,tm) - data$model(betaTRUE,tm)))
sol2$objective

par(ps = 8, bty = "n", las = 1, tck = 0.01,
    mgp = c(3, 0.5, 0), mar = c(4, 4, 1, 1))
plot(tm, yM, xlab = "maturities in years", ylab = "yields in %")
lines(tm,data$model(sol$xbest,tm), col = "blue")
lines(tm,data$model(sol2$par,tm), col = "darkgreen", lty = 2)
legend(x = "bottom", legend = c("true yields", "DE", "nlminb"),
       col = c("black", "blue", "darkgreen"),
       pch = c(1, NA, NA), lty = c(0, 1, 2))


###################################################
### code chunk number 181: NMOFman.Rnw:3643-3646
###################################################
diFa <- 1 / ((1 + NSS(sol$xbest,tm)/100)^tm)
b <- diFa %*% cfMatrix
b - bM


###################################################
### code chunk number 182: NMOFman.Rnw:3651-3655
###################################################
par(ps = 8, bty = "n", las = 1, tck = 0.01,
mgp = c(3, 0.5, 0), mar = c(4, 4, 1, 1))
plot(tm, NSS(sol$xbest,tm) - NSS(betaTRUE,tm),
xlab = "maturities in years", ylab = "yield error in %")


###################################################
### code chunk number 183: NMOFman.Rnw:3662-3666
###################################################
par(ps = 8, bty = "n", las = 1, tck = 0.01,
mgp = c(3, 0.5, 0), mar = c(4, 4, 1, 1))
plot(as.numeric(unlist(lapply(tmList, max))), as.vector(b - bM),
xlab = "maturities in years", ylab = "price error in %")


###################################################
### code chunk number 184: NMOFman.Rnw:3672-3676
###################################################
beta <- c(5,-2,1,10,1,3)
yM <- NSS(beta,tm)
diFa <- 1 / ( (1 + yM/100)^tm )
b <- diFa %*% cfMatrix


###################################################
### code chunk number 185: NMOFman.Rnw:3680-3686
###################################################
B <- cbind(c(5,-2,1,10,1,3), c(4,-2,1,10,1,3))
Y <- array(0, dim = c(length(tm), ncol(B)))
for (i in 1:ncol(Y))
    Y[ ,i] <- NSS(B[ ,i], tm)
D <- 1/((1+Y/100)^tm)
t(cfMatrix) %*% D - as.vector(b)


###################################################
### code chunk number 186: NMOFman.Rnw:3697-3716
###################################################
fy <- function(ytm, cf, tm)
    sum( cf / ( (1 + ytm)^tm ) )
compYield <- function(cf, tm, guess = NULL) {
    logik <- cf != 0
    cf <- cf[logik]
    tm <- tm[logik]
    if (is.null(guess)) {ytm <- 0.05} else {ytm <- guess}
    h <- 1e-8;	dF <- 1; ci <- 0
    while (abs(dF) > 1e-5) {
        ci <- ci + 1; if (ci > 5) break
        FF  <-  fy(ytm, cf, tm)
        dFF <- (fy(ytm + h, cf, tm) - FF) / h
        dF <- FF / dFF
        ytm <- ytm - dF
    }
    if (ytm < 0)
        ytm <- 0.99
    ytm
}


###################################################
### code chunk number 187: NMOFman.Rnw:3720-3746
###################################################
OF3 <- function(param, data) {
    tm <- data$tm
    rM <- data$rM
    cfMatrix<- data$cfMatrix
    nB <- dim(cfMatrix)[2L]
    zrates <- data$model(param,tm); aux <- 1e10
    if ( all(zrates > 0,
             !is.na(zrates))
        ) {
        diFa <- 1 / ((1 + zrates/100)^tm)
        b <- diFa %*% cfMatrix
        r <- numeric(nB)
        if ( all(!is.na(b),
                 diFa < 1,
                 diFa > 0,
                 b > 1)
            ) {
            for (bb in 1:nB) {
                r[bb] <- compYield(c(-b[bb], cfMatrix[ ,bb]), c(0,tm))
            }
            aux <- abs(r - rM)
            aux <- sum(aux)
        }
    }
    aux
}


###################################################
### code chunk number 188: NMOFman.Rnw:3756-3761
###################################################
betaTRUE <- c(5,-2,1,10,1,3)
yM <- NSS(betaTRUE, tm)
diFa <- 1 / ( (1 + yM/100)^tm )
bM <- diFa %*% cfMatrix
rM <- apply(rbind(-bM, cfMatrix), 2, compYield, c(0, tm))


###################################################
### code chunk number 189: NMOFman.Rnw:3766-3786
###################################################
data <- list(rM = rM, tm = tm,
             cfMatrix = cfMatrix,
             model = NSS,
             min = c( 0,-15,-30,-30,0  ,2.5),
             max = c(15, 30, 30, 30,2.5,5  ),
             ww = 0.1,
             fy = fy)
algo <- list(nP = 100L,
             nG = 1000L,
             F  = 0.50,
             CR = 0.99,
             min = c( 0,-15,-30,-30,0  ,2.5),
             max = c(15, 30, 30, 30,2.5,5  ),
             pen = penalty,
             repair = NULL,
             loopOF = TRUE,
             loopPen = FALSE,
             loopRepair = FALSE,
             printBar = FALSE,
             printDetail = FALSE)


###################################################
### code chunk number 190: NMOFman.Rnw:3789-3792
###################################################
sol <- DEopt(OF = OF3, algo = algo, data = data)
max(abs(data$model(sol$xbest,tm) - data$model(betaTRUE,tm)))
sol$OFvalue


###################################################
### code chunk number 191: NMOFman.Rnw:3797-3805
###################################################
s0 <- algo$min + (algo$max - algo$min) * runif(length(algo$min))
sol2 <- nlminb(s0, OF3, data = data,
               lower = algo$min,
               upper = algo$max,
               control = list(eval.max = 50000L,
               iter.max = 50000L))
max(abs(data$model(sol2$par,tm) - data$model(betaTRUE,tm)))
sol2$objective


###################################################
### code chunk number 192: NMOFman.Rnw:3808-3817
###################################################
par(ps = 8, bty = "n", las = 1, tck = 0.01,
    mgp = c(3, 0.5, 0), mar = c(4, 4, 1, 1))
plot(tm, yM, xlab = "maturities in years", ylab = "yields in %")
lines(tm,data$model(sol$xbest,tm), col = "blue")
lines(tm,data$model(sol2$par,tm), col = "darkgreen", lty = 2)

legend(x = "bottom", legend = c("true yields","DE","nlminb"),
       col = c("black", "blue", "darkgreen"),
       pch = c(1, NA, NA), lty = c(0,1,2))


###################################################
### code chunk number 193: NMOFman.Rnw:3821-3823
###################################################
betaTRUE
round(sol$xbest,3)


###################################################
### code chunk number 194: NMOFman.Rnw:3852-3880
###################################################
randomData <- function(p = 200L,      ## number of available regressors
                       n = 200L,      ## number of observations
                       maxReg = 10L,  ## max. number of included regressors
                       s = 1,         ## standard deviation of residuals
                       constant = TRUE ) {

    X <- rnorm(n * p); dim(X) <- c(n, p)  ## regressor matrix X
    if (constant) X[ ,1L] <- 1

    k <- sample.int(maxReg, 1L)   ## the number of true regressors
    K <- sort(sample.int(p, k))   ## the set of true regressors
    betatrue <- rnorm(k)          ## the true coefficients

    ## the response variable y
    y <- X[ ,K] %*% as.matrix(betatrue) + rnorm(n, sd = s)

    list(X = X, y = y, betatrue = betatrue, K = K, n = n, p = p)
}

rD <- randomData(p = 100L, n = 200L, s = 1,
                 constant = TRUE, maxReg = 15L)

data <- list(X = rD$X,
             y = rD$y,
             n = rD$n,
             p = rD$p,
             maxk  = 30L,  ## maximum number of regressors included in model
             lognn = log(rD$n)/rD$n)


###################################################
### code chunk number 195: NMOFman.Rnw:3888-3892
###################################################
x0 <- logical(data$p)
temp <- sample.int(data$maxk, 1L)
temp <- sample.int(data$p, temp)
x0[temp] <- TRUE


###################################################
### code chunk number 196: NMOFman.Rnw:3903-3914
###################################################
require("rbenchmark")
## benchmark(lm(data$y ~ -1 + data$X[ ,x0]),
##           qr.solve(data$X[ ,x0], data$y),
##           columns = c("test", "elapsed", "relative"),
##           order = "test",
##           replications = 1000L)

## ... should give the same coefficients
ignore1 <- lm(data$y ~ -1 + data$X[ ,x0])
ignore2 <- qr.solve(data$X[ ,x0], data$y)
all.equal(as.numeric(coef(ignore1)), as.numeric(ignore2))


###################################################
### code chunk number 197: NMOFman.Rnw:3929-3935
###################################################
OF <- function(x, data) {
    q <- qr(data$X[ ,x])
    e <- qr.resid(q, data$y)
    log(crossprod(e)/data$n) + sum(x) * data$lognn
}
OF(x0, data)


###################################################
### code chunk number 198: NMOFman.Rnw:3942-3951
###################################################
neighbour <- function(xc, data) {
    xn <- xc
    ex <- sample.int(data$p, 1L)
    xn[ex] <- !xn[ex]
    sumx <- sum(xn)
    if ( sumx < 1L || (sumx > data$maxk) )
        xc else xn
}
neighbour(x0, data)[1:10]


###################################################
### code chunk number 199: NMOFman.Rnw:3957-3964
###################################################
algo <- list(nT = 10L,    ## number of thresholds
             nS = 200L,   ## number of steps per threshold
             nD = 1000L,  ## number of random steps to compute thresholds
             neighbour = neighbour,
             x0 = x0,
             printBar = FALSE)
system.time(sol1 <- TAopt(OF, algo = algo, data = data))


###################################################
### code chunk number 200: NMOFman.Rnw:3970-3973
###################################################
sol1$OFvalue
which(sol1$xbest)  ## the selected regressors
rD$K               ## the true regressors


###################################################
### code chunk number 201: NMOFman.Rnw:3980-3984
###################################################
xtrue <- logical(data$p)
xtrue[rD$K] <- TRUE
OF(sol1$xbest, data)
OF(xtrue, data)


###################################################
### code chunk number 202: NMOFman.Rnw:3992-4001
###################################################
restarts <- 50L
algo$printDetail <- FALSE
res <- restartOpt(TAopt, n = restarts,
                  OF = OF, algo = algo, data = data,
                  method = "snow", cl = 2)
par(bty = "n", las = 1,mar = c(3,4,0,0),
    ps = 8, tck = 0.001, mgp = c(3, 0.5, 0))
plot(ecdf(sapply(res, `[[`, "OFvalue")),  ## extract solution quality
     cex = 0.4, main = "", ylab = "", xlab = "")


###################################################
### code chunk number 203: NMOFman.Rnw:4007-4013
###################################################
xbestAll <- sapply(res, `[[`, "xbest")    ## extract all solutions
inclReg  <- which(rowSums(xbestAll) > 0L) ## get included regressors
inclReg  <- sort(union(rD$K, inclReg))
data.frame(regressor = inclReg,
           times_included = paste(rowSums(xbestAll)[inclReg], "/", restarts, sep = ""),
           true_regressor = inclReg %in% rD$K)


###################################################
### code chunk number 204: NMOFman.Rnw:4142-4161
###################################################
S <- 100    ## spot
X <- 100    ## strike
tau <- 1    ## time-to-maturity
r <- 0.02   ## interest rate
q <- 0.02   ## dividend rate
v <- 0.2    ## volatility

## the closed-form solution
callBSM <- function(S,X,tau,r,q,v) {
    d1 <- (log(S/X) + (r - q + v^2 / 2)*tau) / (v*sqrt(tau))
    d2 <- d1 - v*sqrt(tau)
    S * exp(-q * tau) * pnorm(d1) -  X * exp(-r * tau) * pnorm(d2)
}
callBSM(S,X,tau,r,q,v)

## with the characteristic function
callCF(cf = cfBSM, S = S, X = X, tau = tau, r = r, q = q,
       v = v^2,  ## variance, not vol
       implVol = TRUE)


###################################################
### code chunk number 205: NMOFman.Rnw:4165-4168
###################################################
Xvec <- 80:120
tauvec <- c(c(3, 6, 9)/12,  ## 3, 6, 9 months
            1, 2, 3, 4, 5)  ## 1..5 years


###################################################
### code chunk number 206: NMOFman.Rnw:4172-4179
###################################################
fun1 <- function() {
    callprices <- array(NA, dim = c(length(Xvec), length(tauvec)))
    for (X in Xvec)
        for (tau in tauvec)
            callprices[X == Xvec, tau == tauvec] <- callBSM(S,X,tau,r,q,v)
    callprices
}


###################################################
### code chunk number 207: NMOFman.Rnw:4183-4189
###################################################
fun2 <- function() {
    tmp <- expand.grid(Xvec,tauvec)
    callprices <- callBSM(S, tmp[[1]], tmp[[2]], r, q, v)
    dim(callprices) <- c(length(Xvec), length(tauvec))
    callprices
}


###################################################
### code chunk number 208: NMOFman.Rnw:4192-4196
###################################################
callprices1 <- fun1()
callprices2 <- fun2()

all.equal(callprices1, callprices2)


###################################################
### code chunk number 209: NMOFman.Rnw:4200-4206
###################################################
system.time(
    for (i in 1:100)
        ignore <- fun1() )
system.time(
    for (i in 1:100)
        ignore <- fun2() )


###################################################
### code chunk number 210: NMOFman.Rnw:4211-4259
###################################################
priceMatrix <- function(cf, S, Xvec, tauvec, r, q = 0, ...,
                        nodes = NULL, weights = NULL, n = 200) {

    if (is.null(nodes)) {
        tmp <- xwGauss(n)
        tmp <- changeInterval(tmp$nodes, tmp$weights,
                              oldmin = -1, oldmax = 1,
                              newmin =  0, newmax = 200)
        nodes <- tmp$nodes
        weights <- tmp$weights
    }

    callprices <- array(NA, dim = c(length(Xvec), length(tauvec)))
    tmpmat <- array(NA, dim = c(length(Xvec), length(weights)))

    inodes <- 1i * nodes
    itau <- 0L
    for (tau in tauvec) {
        itau <- itau + 1L
        cfi <- S * exp((r - q) * tau)
        cf1 <- cf(nodes - 1i, S, tau, r, q, ...)/inodes/cfi
        cf2 <- cf(nodes,      S, tau, r, q, ...)/inodes
        iX <- 0L
        for (X in Xvec) {
            iX <- iX + 1L
            if (itau == 1L)
                tmpmat[iX, ] <- exp(-inodes * log(X))
            P1 <- 0.5 + weights %*% Re(tmpmat[iX, ] * cf1)/pi
            P2 <- 0.5 + weights %*% Re(tmpmat[iX, ] * cf2)/pi
            callprices[iX, itau] <-
                exp(-q*tau) * S * P1 - exp(-r*tau) * X * P2
        }
   }
    callprices
}
v <- .2

system.time(
    for (i in 1:100)
        ignore <- priceMatrix(cf = cfBSM, S, Xvec, tauvec, r, q = q,
                   v = 0.2^2, n = 50) )
require("compiler")
priceMatrix2 <- cmpfun(priceMatrix)
system.time(
    for (i in 1:100)
        ignore <- priceMatrix2(cf = cfBSM, S, Xvec, tauvec, r, q = q,
                   v = 0.2^2, n = 50) )



###################################################
### code chunk number 211: NMOFman.Rnw:4262-4292
###################################################
cfp <- priceMatrix(cf = cfBSM, S, Xvec, tauvec, r, q = q,
                   v = 0.2^2, n = 100)

callprices1[1:5, 1:5]
callprices2[1:5, 1:5]
cfp[1:5, 1:5]
all.equal(callprices1, cfp)
system.time(
    for (i in 1:100)
        ignore <- fun1() )
system.time(
    for (i in 1:100)
        ignore <- fun2() )
system.time(
    for (i in 1:100)
        ignore <- priceMatrix(cf = cfBSM, S, Xvec, tauvec, r, q = q,
                   v = 0.2^2, n = 50) )

tmp <- xwGauss(50)
tmp <- changeInterval(tmp$nodes, tmp$weights,
                      oldmin = -1, oldmax = 1,
                      newmin =  0, newmax = 200)
nodes <- tmp$nodes
weights <- tmp$weights
system.time(
    for (i in 1:100)
        ignore <- priceMatrix(cf = cfBSM, S, Xvec, tauvec, r, q = q,
                              v = 0.2^2,
                              nodes = nodes, weights = weights) )



###################################################
### code chunk number 212: NMOFman.Rnw:4333-4334
###################################################
require("adagio")


###################################################
### code chunk number 213: NMOFman.Rnw:4339-4344
###################################################
## Example 1
p <- c(15, 100, 90, 60, 40, 15, 10,  1)
w <- c( 2,  20, 20, 30, 40, 30, 60, 10)
cap <- 102
(is <- knapsack(w, p, cap))


###################################################
### code chunk number 214: NMOFman.Rnw:4348-4349
###################################################
Data <- list(p = p, w = w, n = length(p), cap = cap)


###################################################
### code chunk number 215: NMOFman.Rnw:4354-4366
###################################################
OF <- function(x, Data)
    -sum(x * Data$p)

neighbour <- function(x, Data) {
    xn <- x
    p <- sample.int(Data$n, size = 1L)
    xn[p] <- !xn[p]
    if (sum(Data$w*xn) > Data$cap)
        x
    else
        xn
}


###################################################
### code chunk number 216: NMOFman.Rnw:4370-4376
###################################################
algo <- list(x0 = logical(Data$n),  ## a random start
             printDetail = TRUE, printBar = FALSE,
             q = 0.99, neighbour = neighbour,
             nS = 50)
system.time(sol <- TAopt(OF, algo = algo, Data))
OF(sol$xbest,Data)


###################################################
### code chunk number 217: NMOFman.Rnw:4380-4385
###################################################
## Example 1
## [1] 1 2 3 4 6 , capacity 102 and total profit 280
xHWB <- logical(Data$n)
xHWB[c(1:4,6)] <- TRUE
OF(xHWB, Data)


###################################################
### code chunk number 218: NMOFman.Rnw:4389-4395
###################################################
## Example 2
## [1] 1 4 , capacity 50 and total profit 107
p <- c(70, 20, 39, 37, 7, 5, 10)
w <- c(31, 10, 20, 19, 4, 3,  6)
cap <- 50
(is <- knapsack(w, p, cap))


###################################################
### code chunk number 219: NMOFman.Rnw:4399-4405
###################################################
Data <- list(p = p, w = w, n = length(p), cap = cap)
algo <- list(x0 = logical(Data$n),  ## a random start
             printDetail = TRUE, printBar = FALSE,
             q = 0.99, neighbour = neighbour, nS = 50)
system.time(sol <- TAopt(OF, algo = algo, Data))
OF(sol$xbest, Data)


###################################################
### code chunk number 220: NMOFman.Rnw:4417-4428
###################################################
set.seed(8232)
X <- runif(100L)

## Find subset that sums up close to 2.0 !
i <- sort(c(84,54,11,53,88,12,26,45,25,62,96,23,78,77,66,1))
sum(X[i])
## --> should be 2.000451

xHWB <- logical(100L)
xHWB[i] <- TRUE
sum(X[xHWB]) ## check


###################################################
### code chunk number 221: NMOFman.Rnw:4436-4459
###################################################
## try with optim/SANN
makeN <- function(X, size = 1L) {
    function(x) {
        x <- x > 0L
        p <- sample.int(100, size = size)
        x[p] <- !x[p]
        x
    }
}
makeF <- function(X) {
    function(x) {
        x <- x > 0L
        abs(sum(X[x]) - 2)
    }
}
F <- makeF(X)
N <- makeN(X)
x0 <- runif(100)>0.5
F(x0) ## initial solution
result <- optim(par = x0, fn = F,  N, method = "SANN",
                control = list(maxit = 20000,
                temp = 1))
F(as.logical(result$par)) ## final solution


###################################################
### code chunk number 222: NMOFman.Rnw:4469-4557
###################################################
require("quadprog")

na <- 50L   ## number of assets
ns <- 100L  ## number of scenarios
R  <- array(rnorm(ns*na, mean = 0.005, sd = 0.015),
            dim = c(ns, na))
mu <- colMeans(R)
rf <- 0.0001
mu2 <- mu - rf

## TEST 1: minimum-variance portfolio (long/short)
wsup <- 0.05
winf <- -0.05
Q <- 2*cov(R)
A <- array( 1, dim = c(1,na)); a <- 1
B <- rbind(-diag(na),diag(na))
b <- rbind(array(-wsup, dim = c(na,1)),
           array( winf, dim = c(na,1)))
result <- solve.QP(Dmat = Q, dvec = rep(0, na),
                   Amat = t(rbind(A,B)), bvec = rbind(a, b),
                   meq = 1)
wqp <- result$solution

resample <- function(x, ...)
    x[sample.int(length(x), ...)]
data <- list(RR = cov(R), na = na, ns = ns,
             eps = 0.10/100, winf = winf, wsup = wsup,
             resample = resample)
neighbour <- function(w, data){
    toSell <- w > data$winf
    toBuy  <- w < data$wsup
    i <- resample(which(toSell), size = 1L)
    j <- resample(which(toBuy),  size = 1L)
    eps <- runif(1L) * data$eps
    eps <- min(w[i] - data$winf, data$wsup - w[j], eps)
    w[i] <- w[i] - eps
    w[j] <- w[j] + eps
    w
}
OF <- function(w, data) {
    aux <- crossprod(data$RR,w)
    crossprod(w,aux)
}
w0 <- runif(na)
w0 <- w0/sum(w0)
algo <- list(x0 = w0, neighbour = neighbour,
             nS = 5000L, nT = 10L, nD = 2000L, q = 0.02,
             printBar = FALSE, printDetail = FALSE)
res <- TAopt(OF,algo,data)
as.numeric(16 * 100 *sqrt(res$OFvalue)) -
    as.numeric(16 * 100 *sqrt(result$value))


## check constraints
wSummary <- function(w)
    cat("min weight: ", min(w), "\n",
        "max weight: ", max(w), "\n",
        "sum of weights: ", sum(w), "\n",
        "no. of assets: ",  sum(w > 1e-12), "\n", sep ="")
wSummary(res$xbest)
wSummary(wqp)

cat("Compare results: \n",
    "QP:", 100 * sqrt( crossprod(R %*% wqp)/data$ns ),"\n",
    "TA:", 100 * sqrt( crossprod(R %*% res$xbest)/data$ns ) ,"\n")

## TEST 2: tangency portfolio with non-negative weights
winf <- 0; Q <- cov(R)
A <- array(mu2, dim = c(1L, na)); a <- 1
B <- diag(na); b <- array( winf, dim = c(na,1L))
result <- solve.QP(Dmat = Q, dvec = rep(0,na),
                   Amat = t(rbind(A,B)), bvec = rbind(a,b),
                   meq = 1)
w <- as.matrix(result$solution/sum(result$solution))
SR <- t(w) %*% mu2 / sqrt(t(w) %*% Q %*% w)
OF2 <- function(w, data) {
    aux <- crossprod(data$RR,w)
    sqrt(crossprod(w,aux)) / t(w) %*% data$mu2
}
w0 <- runif(na); w0 <- w0/sum(w0)
data <- list(RR = cov(R), na = na, ns = ns, mu2 = mu2,
             eps = 0.10/100, winf = winf, wsup = 1)
res <- TAopt(OF2,algo,data)
wSummary(res$xbest)
wSummary(w)

## check difference between Sharpe ratios
1/res$OFvalue - as.numeric(SR)


###################################################
### code chunk number 223: NMOFman.Rnw:4579-4581
###################################################
options(digits = 7)
require("Rsolnp")


###################################################
### code chunk number 224: NMOFman.Rnw:4585-4657
###################################################
## run the benchmark case
bmResults <- benchmark(id = "RachevRatio")
strwrap(attr(bmResults, "description"), 0.75 * getOption("width"))

##
OF <- function(sol, data) {
    alpha <- data$alpha
    Rw <- sol$Rw
    VaRp <- quantile( Rw, probs = alpha, type = 1)
    VaRm <- quantile(-Rw, probs = alpha, type = 1)
    CVaRp <- VaRp - 0.5 * mean(((VaRp - Rw) + abs(VaRp - Rw))) / alpha
    CVaRm <- VaRm - 0.5 * mean(((VaRm + Rw) + abs(VaRm + Rw))) / alpha
    -CVaRm/CVaRp
}


neighbourU <- function(sol, data){
    wn <- sol$w
    toSell <- wn > data$winf
    toBuy  <- wn < data$wsup
    i <- data$resample(which(toSell), size = 1L)
    j <- data$resample(which(toBuy),  size = 1L)
    eps <- data$eps * runif(1)
    eps <- min(wn[i] - data$winf, data$wsup - wn[j], eps)
    wn[i] <- wn[i] - eps
    wn[j] <- wn[j] + eps
    Rw <- sol$Rw + data$R[,c(i,j)] %*% c(-eps,eps)
    list(w = wn, Rw = Rw)
}


## prepare data
rets <- as.matrix(dji30ret)
data <- list(R = rets,
             na = dim(rets)[2L],
             ns = dim(rets)[1L],
             eps = 2/100,
             wsup = 0.1,
             winf = 0,
             alpha = 0.05,
             resample = resample)


## check the objective function:
## (1) get benchmark weights
solnp <- bmResults[grep("par", attr(bmResults, "row.names")), "solnp"]
snopt <- bmResults[grep("par", attr(bmResults, "row.names")), "snopt"]

## (2) setup solutions
sol.solnp <- list(w = solnp, Rw = rets %*% solnp)
sol.snopt <- list(w = snopt, Rw = rets %*% snopt)

## (3) compare objective function values benchmark/OF
bmResults[grep("func", attr(bmResults, "row.names")), ]
data.frame(solnp = OF(sol.solnp, data),
           snopt = OF(sol.snopt, data))


## run TAopt
## random initial solution
w0 <- runif(data$na); w0 <- w0/sum(w0)
sol0 <- list(w = w0, Rw = data$R %*% w0)

algo <- list(x0 = sol0, neighbour = neighbourU,
             nS = 3000L, nT = 10L, nD = 1000L,
             q = 0.9,
             printBar = FALSE, printDetail = 2500L)
res <- TAopt(OF,algo,data)
##plot(cummin(res$Fmat[ ,2L]), type = 'l')
##lines(res$Fmat[ ,1L], type = 'l')
res$OFvalue
wSummary(res$xbest$w)


###################################################
### code chunk number 225: NMOFman.Rnw:4673-4675 (eval = FALSE)
###################################################
## install.packages("NMOF") ## CRAN
## install.packages("NMOF", repos = "http://R-Forge.R-project.org")


###################################################
### code chunk number 226: NMOFman.Rnw:4683-4685 (eval = FALSE)
###################################################
## require("NMOF")
## showExample("exampleOF.R")


###################################################
### code chunk number 227: NMOFman.Rnw:4711-4712
###################################################
toLatex(sessionInfo())


