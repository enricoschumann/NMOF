### R code from vignette source 'NMOFex.Rnw'

###################################################
### code chunk number 1: NMOFex.Rnw:74-76
###################################################
## version 2011-10-15
options(continue = " ", digits = 3, width = 70)


###################################################
### code chunk number 2: NMOFex.Rnw:90-91 (eval = FALSE)
###################################################
## install.packages("NMOF", repos = "http://R-Forge.R-project.org")


###################################################
### code chunk number 3: NMOFex.Rnw:94-96
###################################################
require("NMOF")
set.seed(1112233344)


###################################################
### code chunk number 4: NMOFex.Rnw:102-104 (eval = FALSE)
###################################################
## whereToLook <- system.file("NMOFex/NMOFex.R", package = "NMOF")
## file.show(whereToLook, title = "NMOF examples")


###################################################
### code chunk number 5: NMOFex.Rnw:123-137
###################################################
## using several cores
testFun  <- function(x) {
        Sys.sleep(0.1)  ## wasting time :)
        x[1L] + x[2L]^2
}
lower <- 1:3; upper <- 5; n <- 5L
system.time(sol1 <- gridSearch(fun = testFun,
                               lower = lower, upper = upper,
                               n = n, printDetail = TRUE))
system.time(sol2 <- gridSearch(fun = testFun,
                               lower = lower, upper = upper,
                               n = n, printDetail = FALSE,
                               method = "snow", cl = 4L))
all.equal(sol1, sol2)


###################################################
### code chunk number 6: NMOFex.Rnw:153-160
###################################################
## create random data with vols between minVol and maxVol
## and pairwise correlation of 0.6
na <- 500L
C <- array(0.6, dim = c(na,na)); diag(C) <- 1
minVol <- 0.20; maxVol <- 0.40
Vols <- (maxVol - minVol) * runif(na) + minVol
Sigma <- outer(Vols,Vols) * C


###################################################
### code chunk number 7: NMOFex.Rnw:172-190
###################################################
## objective function for LS/TA
OF <- function(x, data) {
    sx <- sum(x)
    w <- rep.int(1/sx, sx)
    res <- crossprod(w, data$Sigma[x, x])
    tcrossprod(w, res)
}

## neighbourhood function for LS/TA
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
### code chunk number 8: NMOFex.Rnw:197-203
###################################################
## data
data <- list(Sigma = Sigma,   ### cov-matrix
              Kinf = 30L,      ### min cardinality
              Ksup = 60L,      ### max cardinality
              na = na,         ### number of assets
              nn = 1L)         ### how many assets to change per iteration


###################################################
### code chunk number 9: NMOFex.Rnw:207-212
###################################################
## a random solution x0
card0 <- sample(data$Kinf:data$Ksup, 1L, replace = FALSE)
assets <- sample.int(na, card0, replace = FALSE)
x0 <- logical(na)
x0[assets] <- TRUE


###################################################
### code chunk number 10: NMOFex.Rnw:218-226
###################################################
## *Local Search*
algo <- list(x0 = x0, neighbour = neighbour, nS = 5000L,
             printDetail = FALSE, printBar = FALSE)
system.time(solLS <- LSopt(OF, algo = algo, data = data))

## *Threshold Accepting*
algo$nT <- 10L; algo$nS <- trunc(algo$nS/algo$nT); algo$q <- 0.2
system.time(solTA <- TAopt(OF, algo = algo, data = data))


###################################################
### code chunk number 11: NMOFex.Rnw:308-316
###################################################
## *Genetic Algorithm*
OF2 <- function(x, data) {
    res <- colSums(data$Sigma %*% x * x)
    n <- colSums(x); res <- res / n^2
    ## penalise
    p <- pmax(data$Kinf - n, 0) + pmax(n - data$Ksup, 0)
    res + p
}


###################################################
### code chunk number 12: NMOFex.Rnw:322-325
###################################################
algo <- list(nB = na, nP = 100L, nG = 500L, prob = 0.002,
             printBar = FALSE, loopOF = FALSE)
system.time(solGA <- GAopt(OF = OF2, algo = algo, data = data))


###################################################
### code chunk number 13: NMOFex.Rnw:328-333
###################################################
cat(
    "Local Search        ", format(sqrt(solLS$OFvalue), digits = 4), "\n",
    "Threshold Accepting ", format(sqrt(solTA$OFvalue), digits = 4), "\n",
    "Genetic Algorithm   ", format(sqrt(solGA$OFvalue), digits = 4), "\n",
    sep = "")


###################################################
### code chunk number 14: NMOFex.Rnw:357-365
###################################################
na <- 100L                                 ### number of assets
ns <- 200L                                 ### number of scenarios
vols <- runif(na, min = 0.2, max = 0.4)    ### marginal vols
C <- matrix(0.6, na, na); diag(C) <- 1     ### correlation matrix
R <- rnorm(ns * na)/16                     ### random returns
dim(R) <- c(ns, na)
R <- R %*% chol(C)
R <- R %*% diag(vols)


###################################################
### code chunk number 15: NMOFex.Rnw:386-397
###################################################
data <- list(R = t(R),              ### scenarios
              theta = 0.005,         ### return threshold
              na = na,               ### number of assets
              ns = ns,               ### number of scenarios
              max = rep( 0.05, na),  ### DE: vector of max. weight
              min = rep(-0.05, na),  ### DE: vector of min. weight
              wsup =  0.05,          ### TA: max weight
              winf = -0.05,          ### TA: min weight
              eps = 0.5/100,         ### TA: step size
              w = 1                  ### penalty weight
             )


###################################################
### code chunk number 16: NMOFex.Rnw:402-405
###################################################
x0 <- data$min + runif(data$na)*(data$max - data$min)
x0[1:5]
sum(x0)


###################################################
### code chunk number 17: NMOFex.Rnw:408-412
###################################################
temp <- R %*% x0             ### compute portfolio returns
temp <- temp - data$theta
temp <- (temp[temp < 0])^2
sum(temp)/ns                 ### semivariance


###################################################
### code chunk number 18: NMOFex.Rnw:416-423
###################################################
OF <- function(x, data) {
    Rx <- crossprod(data$R, x)
    Rx <- Rx - data$theta
    Rx <- Rx - abs(Rx)
    Rx <- Rx * Rx
    colSums(Rx) /(4*data$ns)
}


###################################################
### code chunk number 19: NMOFex.Rnw:431-433
###################################################
OF(x0, data)
OF(cbind(x0, x0), data)


###################################################
### code chunk number 20: NMOFex.Rnw:440-450
###################################################
repair <- function(x, data) {
    myFun <- function(x) x/sum(x)
    if (is.null(dim(x)[2L]))
        myFun(x) else apply(x, 2L, myFun)
}
repair2 <- function(x, data) {
    myFun <- function(x) x + (1 - sum(x))/data$na
    if (is.null(dim(x)[2L]))
        myFun(x) else apply(x, 2L, myFun)
}


###################################################
### code chunk number 21: NMOFex.Rnw:454-459
###################################################
sum(x0)
sum(repair(x0, data))

colSums(repair( cbind(x0, x0), data))
colSums(repair2(cbind(x0, x0), data))


###################################################
### code chunk number 22: NMOFex.Rnw:462-473
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
### code chunk number 23: NMOFex.Rnw:479-485
###################################################
x0[1L] <- 0.30
penalty(x0, data)
penalty(cbind(x0, x0), data)
x0[1L] <- 0
penalty(x0, data)
penalty(cbind(x0, x0), data)


###################################################
### code chunk number 24: NMOFex.Rnw:489-499
###################################################
algo <- list(nP = 100,        ### population size
             nG = 1000,       ### number of generations
             F = 0.25,        ### step size
             CR = 0.9,
             min = data$min,
             max = data$max,
             repair = repair,
             pen = penalty,
             printBar = FALSE, printDetail = TRUE,
             loopOF = TRUE, loopPen = TRUE, loopRepair = TRUE)


###################################################
### code chunk number 25: NMOFex.Rnw:504-511
###################################################
system.time(sol <- DEopt(OF = OF,algo = algo,data = data))
16 * 100 * sqrt(sol$OFvalue)   ### solution quality

## check constraints
all(all.equal(sum(sol$xbest), 1),  ### budget constraint
    sol$xbest <= data$max,         ### holding size constraints
    sol$xbest >= data$min)


###################################################
### code chunk number 26: NMOFex.Rnw:520-527
###################################################
## looping over the population
algo$loopOF <- TRUE; algo$loopPen <- TRUE; algo$loopRepair <- TRUE
system.time(sol <- DEopt(OF = OF,algo = algo, data = data))

## evaluating the population in one step
algo$loopOF <- FALSE; algo$loopPen <- FALSE; algo$loopRepair <- FALSE
system.time(sol <- DEopt(OF = OF,algo = algo, data = data))


###################################################
### code chunk number 27: NMOFex.Rnw:536-549
###################################################
algo$printDetail <- FALSE
restartsDE <- restartOpt(fun = DEopt,      ### what function
                         n = 20L,          ### how many restarts
                         OF = OF,
                         algo = algo,
                         data = data,
                         method = "snow",  ### using package snow
                         cl = 4)           ### 4 cores

## extract best solution
OFvaluesDE <- sapply(restartsDE, `[[`, "OFvalue")
OFvaluesDE <- 16 * 100 * sqrt(OFvaluesDE)
weightsDE <- sapply(restartsDE, `[[`, "xbest")


###################################################
### code chunk number 28: NMOFex.Rnw:552-557
###################################################
par(bty = "n", las = 1, mar = c(3, 4, 0, 0),
    ps = 8, tck = 0.001, mgp = c(3, 0.2, 0))
plot(sort(OFvaluesDE), (seq_len(length(OFvaluesDE))) / length(OFvaluesDE),
     type = "S", ylim = c(0, 1), xlab = "", ylab = "")
mtext("OF value",  side = 1, line = 1)


###################################################
### code chunk number 29: NMOFex.Rnw:561-567
###################################################
par(bty = "n", las = 1, mar = c(3, 4, 0, 0),
    ps = 8, tck = 0.001, mgp = c(3, 0.2, 0))
boxplot(t(weightsDE),
        outline = FALSE, boxwex = 0.4, ylim = c(-0.06,0.06))
mtext("assets",  side = 1, line = 1)
mtext("weights", side = 2, line = 1.3, las = 1, padj = -5)


###################################################
### code chunk number 30: NMOFex.Rnw:579-617
###################################################
algo$printDetail <- FALSE;  algo$nP <- 200L; restarts <- 20L
nGs <- c(500L, 1000L, 2500L)
lstOFvaluesDE <- list()
for (i in 1:3) {
    algo$nG <- nGs[i]
    restartsDE <- restartOpt(fun = DEopt,
                             n = restarts,
                             OF = OF,  algo = algo, data = data,
                             method = "snow", cl = 8)
    OFvaluesDE <- sapply(restartsDE, `[[`, "OFvalue") ### extract best solution
    OFvaluesDE <- 16 * 100 * sqrt(OFvaluesDE)
    lstOFvaluesDE[[i]] <- OFvaluesDE
}
res <- simplify2array(lstOFvaluesDE)

## now with 'repair2'
algo$repair <- repair2
lstOFvaluesDE <- list()
for (i in 1:3) {
    algo$nG <- nGs[i]
    restartsDE <- restartOpt(fun = DEopt,
                             n = restarts,
                             OF = OF,  algo = algo, data = data,
                             method = "snow", cl = 8)
    OFvaluesDE <- sapply(restartsDE, `[[`, "OFvalue") ### extract best solution
    OFvaluesDE <- 16 * 100 * sqrt(OFvaluesDE)
    lstOFvaluesDE[[i]] <- OFvaluesDE
}
res2 <- simplify2array(lstOFvaluesDE)

## plot results
allres <- as.vector(rbind(res,res2))
xlims <- pretty(allres); xlims <- c(min(xlims), max(xlims))
par(bty = "n", las = 1, mar = c(3, 4, 0, 0),
    ps = 8, tck = 0.001, mgp = c(3, 0.2, 0))
plot(ecdf(res[ ,3L]), xlim = xlims, cex = 0.4, main = "", ylab = "", xlab = "")
for (i in 1:2) lines(ecdf(res[  ,i]), cex = 0.4)
for (i in 1:3) lines(ecdf(res2[ ,i]), col = "blue", cex = 0.4)


###################################################
### code chunk number 31: NMOFex.Rnw:626-633
###################################################
weightsDE <- sapply(restartsDE, `[[`, "xbest")
par(bty = "n", las = 1, mar = c(3, 4, 0, 0),
    ps = 8, tck = 0.001, mgp = c(3, 0.2, 0))
boxplot(t(weightsDE),
        outline = FALSE, boxwex = 0.4, ylim = c(-0.06, 0.06))
mtext("assets",  side = 1, line = 1)
mtext("weights", side = 2, line = 1.3, las = 1, padj = -5)


###################################################
### code chunk number 32: NMOFex.Rnw:639-656
###################################################
algo <- list(nP = 100L,        ### population size
    nG = 1000L,                ### number of generations
    c1 = 0.5,                  ### weight for individually best solution
    c2 = 1.5,                  ### weight for overall best solution
    min = data$min,
    max = data$max,
    repair = repair, pen = penalty,
    iner = 0.7, initV = 1, maxV = 0.2,
    printBar = FALSE, printDetail = TRUE)

system.time(sol <- PSopt(OF = OF,algo = algo,data = data))
16 * 100 * sqrt(sol$OFvalue)      ### solution quality

## check constraints
all(all.equal(sum(sol$xbest),1),  ### budget constraint
    sol$xbest <= data$max,
    sol$xbest >= data$min)


###################################################
### code chunk number 33: NMOFex.Rnw:662-694
###################################################
## adjusting velocity
changeV <- function(x, data) {
    myFun <- function(x) x - (sum(x))/data$na
    if (is.null(dim(x)[2L]))
        myFun(x) else apply(x, 2L, myFun)
}
sum(changeV(x0, data))
colSums(changeV(cbind(x0, x0), data))

## initial population that meets budget constraint
initP <- data$min + diag(data$max - data$min) %*%
         array(runif(length(data$min) * algo$nP),
               dim = c(length(data$min),  algo$nP))
colSums(initP <- repair(initP,data))[1:10]

## add to 'algo'
algo$changeV <- changeV        ### function to adjust velocity
algo$initP <- initP            ### initial population
algo$repair <- NULL            ### not needed anymore

system.time(sol <- PSopt(OF = OF,algo = algo, data = data))
16 * 100 * sqrt(sol$OFvalue)   ### solution quality

## check constraints
all(all.equal(sum(sol$xbest), 1), ### budget constraint
    sol$xbest <= data$max,
    sol$xbest >= data$min)

## vectorised
algo$loopOF <- FALSE; algo$loopPen <- FALSE
algo$loopRepair <- FALSE; algo$loopChangeV <- FALSE
system.time(sol <- PSopt(OF = OF, algo = algo, data = data))


###################################################
### code chunk number 34: NMOFex.Rnw:697-712
###################################################
algo$printDetail <- FALSE
restartsPS <- restartOpt(fun = PSopt,
                         n = 20L,
                         OF = OF,
                         algo = algo, data = data,
                         method = "snow", cl = 8)

## extract best solution
OFvaluesPS <- sapply(restartsPS, `[[`, "OFvalue")
OFvaluesPS <- 16 * 100 * sqrt(OFvaluesPS)
par(bty = "n", las = 1,mar = c(3,4,0,0),
    ps = 8, tck = 0.001, mgp = c(3, 0.2, 0))
plot(sort(OFvaluesPS), (seq_len(length(OFvaluesPS))) / length(OFvaluesPS),
     type = "S", ylim = c(0, 1), xlab = "", ylab = "")
mtext("OF value",  side = 1, line = 1)


###################################################
### code chunk number 35: NMOFex.Rnw:720-743
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
### code chunk number 36: NMOFex.Rnw:746-759
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
### code chunk number 37: NMOFex.Rnw:763-783
###################################################
restartsTA <- restartOpt(fun = TAopt,
                         n = 20L,
                         OF = OF,
                         algo = algo, data = data,
                         method = "snow", cl = 8)

OFvaluesTA <- sapply(restartsTA, `[[`, "OFvalue") # extract best solution
OFvaluesTA <- 16 * 100 * sqrt(OFvaluesTA)
weightsTA <- sapply(restartsTA, `[[`, "xbest")
par(bty = "n", las = 1,mar = c(3,4,0,0), ps = 8,
    tck = 0.001, mgp = c(3, 0.2, 0))

## blue: DE solution with nP = 200 and nG = 2000
xlims <- pretty(c(res2[,3], OFvaluesTA))
plot(ecdf(res2[,3]), col = "blue", cex = 0.4,
     main = "", ylab = "", xlab = "",
xlim = c(min(xlims), max(xlims)) )

## black: TA
lines(ecdf(OFvaluesTA), cex = 0.4)


###################################################
### code chunk number 38: NMOFex.Rnw:805-834
###################################################
randomData <- function(
    p = 200L,      ### number of available regressors
    n = 200L,      ### number of observations
    maxReg = 10L,  ### max. number of included regressors
    s = 1,         ### standard deviation of residuals
    constant = TRUE ) {

    X <- rnorm(n * p); dim(X) <- c(n, p)  # regressor matrix X
    if (constant) X[ ,1L] <- 1

    k <- sample.int(maxReg, 1L)   ### the number of true regressors
    K <- sort(sample.int(p, k))   ### the set of true regressors
    betatrue <- rnorm(k)          ### the true coefficients

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
             maxk  = 30L,  ### maximum number of regressors included in model
             lognn = log(rD$n)/rD$n)


###################################################
### code chunk number 39: NMOFex.Rnw:840-844
###################################################
x0 <- logical(data$p)
temp <- sample.int(data$maxk, 1L)
temp <- sample.int(data$p, temp)
x0[temp] <- TRUE


###################################################
### code chunk number 40: NMOFex.Rnw:853-864
###################################################
require(rbenchmark)
benchmark(lm(data$y ~ -1 + data$X[ ,x0]),
          qr.solve(data$X[ ,x0], data$y),
          columns = c("test", "elapsed", "relative"),
          order = "test",
          replications = 1000L)

## ... should give the same coefficients
ignore1 <- lm(data$y ~ -1 + data$X[ ,x0])
ignore2 <- qr.solve(data$X[ ,x0], data$y)
all.equal(as.numeric(coef(ignore1)), as.numeric(ignore2))


###################################################
### code chunk number 41: NMOFex.Rnw:873-879
###################################################
OF <- function(x, data) {
    q <- qr(data$X[ ,x])
    e <- qr.resid(q, data$y)
    log(crossprod(e)/data$n) + sum(x) * data$lognn
}
OF(x0, data)


###################################################
### code chunk number 42: NMOFex.Rnw:884-893
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
### code chunk number 43: NMOFex.Rnw:898-906
###################################################
algo <- list(
    nT = 10L,    ### number of thresholds
    nS = 200L,   ### number of steps per threshold
    nD = 1000L,  ### number of random steps to compute thresholds
    neighbour = neighbour,
    x0 = x0,
    printBar = FALSE)
system.time(sol1 <- TAopt(OF, algo = algo, data = data))


###################################################
### code chunk number 44: NMOFex.Rnw:911-914
###################################################
sol1$OFvalue
which(sol1$xbest)  ### the selected regressors
rD$K               ### the true regressors


###################################################
### code chunk number 45: NMOFex.Rnw:921-925
###################################################
xtrue <- logical(data$p)
xtrue[rD$K] <- TRUE
OF(sol1$xbest, data)
OF(xtrue, data)


###################################################
### code chunk number 46: NMOFex.Rnw:932-941
###################################################
restarts <- 50L
algo$printDetail <- FALSE
res <- restartOpt(TAopt, n = restarts,
                  OF = OF, algo = algo, data = data,
                  method = "snow", cl = 8)
par(bty = "n", las = 1,mar = c(3,4,0,0),
    ps = 8, tck = 0.001, mgp = c(3, 0.2, 0))
plot(ecdf(sapply(res, `[[`, "OFvalue")),  ### extract solution quality
     cex = 0.4, main = "", ylab = "", xlab = "")


###################################################
### code chunk number 47: NMOFex.Rnw:946-954
###################################################
xbestAll <- sapply(res, `[[`, "xbest")    ### extract all solutions
inclReg  <- which(rowSums(xbestAll) > 0L) ### get included regressors
inclReg  <- sort(union(rD$K, inclReg))
data.frame(
    regressor = inclReg,
    times_included = paste(rowSums(xbestAll)[inclReg], "/",
                           restarts, sep = ""),
    true_regressor = inclReg %in% rD$K)


###################################################
### code chunk number 48: NMOFex.Rnw:1048-1055
###################################################
OF <- tfTrefethen
n <- 100L
surf <- matrix(NA, n, n)
x1 <- seq(from = -10, to = 10, length.out = n)
for (i in seq_len(n))
    for (j in seq_len(n))
         surf[i, j] <- tfTrefethen(c(x1[i], x1[j]))


###################################################
### code chunk number 49: NMOFex.Rnw:1062-1068
###################################################
par(bty = "n", las = 1, mar = c(3,4,0,0),
    ps = 8, tck = 0.001, mgp = c(3, 0.2, 0))
contour(x1, x1, surf, nlevels=5, col = grey(0.6))

## the actual minimum
abline(v = -0.02440308, h = 0.21061243, col = grey(0.6))


###################################################
### code chunk number 50: NMOFex.Rnw:1073-1079
###################################################
algo <- list(nP = 50L, nG = 300L,
             F = 0.6, CR = 0.9,
             min = c(-10,-10), max = c(10,10),
             printDetail = FALSE, printBar = FALSE,
             storeF = TRUE, storeSolutions = TRUE)
sol <- DEopt(OF = OF, algo = algo)


###################################################
### code chunk number 51: NMOFex.Rnw:1082-1088
###################################################
names(sol)
sd(sol$popF)
ts.plot(sol$Fmat, xlab = "generations", ylab = "OF")

length(sol$xlist)
xlist <- sol$xlist[[1L]]


###################################################
### code chunk number 52: NMOFex.Rnw:1096-1111
###################################################
## show solution 1 (column 1) in population over time
xlist[[  1L]][ ,1L]  ### at the end of generation 1
## ...
xlist[[ 10L]][ ,1L]  ### at the end of generation 10
## ...
xlist[[300L]][ ,1L]  ### at the end of generation 300

res  <- sapply(xlist, `[`, 1:2, 1)  ### get row 1 and 2 from column 1
res2 <- sapply(xlist, `[`, TRUE, 1) ### simpler
all.equal(res, res2)

dim(res)
res[ ,1L]
res[ ,2L]
res[ ,300L]


###################################################
### code chunk number 53: NMOFex.Rnw:1115-1129
###################################################
## show parameter 2 (row 2) in population over time
xlist[[  1L]][2L, ]  ### at the end of generation 1
## ...
xlist[[ 10L]][2L, ]  ### at the end of generation 10
## ...
xlist[[300L]][2L, ]  ### at the end of generation 300

res <- sapply(xlist, `[`, 2, 1:50)
res <- sapply(xlist, `[`, 2, TRUE)  ### simpler

dim(res)
res[ ,1L]
res[ ,2L]
res[ ,300L]


###################################################
### code chunk number 54: NMOFex.Rnw:1134-1143 (eval = FALSE)
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
### code chunk number 55: NMOFex.Rnw:1146-1174
###################################################
setEPS()
postscript(file = "figures/c1.eps", width = 2, height = 2)
par(bty="n", las = 1,mar = c(2,2,0,0),
     ps = 8, tck = 0.001, mgp = c(3, 0.2, 0))
contour(x1, x1, surf, nlevels=5, col = grey(0.6))
abline(v = -0.02440308, h = 0.21061243, col = grey(0.6))
points(t(xlist[[1L]]), pch = 21, bg=grey(0.9), col = grey(.2))
invisible(dev.off())

setEPS()
postscript(file = "figures/c2.eps", width = 2, height = 2)
par(bty="n", las = 1,mar = c(2,2,0,0),
     ps = 8, tck = 0.001, mgp = c(3, 0.2, 0))
contour(x1, x1, surf, nlevels=5, col = grey(0.6))
abline(v = -0.02440308, h = 0.21061243, col = grey(0.6))
points(t(xlist[[100L]]), pch = 21, bg=grey(0.9), col = grey(.2))
invisible(dev.off())

setEPS()
postscript(file = "figures/c3.eps", width = 2, height = 2)
par(bty="n", las = 1,mar = c(2,2,0,0),
     ps = 8, tck = 0.001, mgp = c(3, 0.2, 0))
contour(x1, x1, surf, nlevels=5, col = grey(0.6))
abline(v = -0.02440308, h = 0.21061243, col = grey(0.6))
points(t(xlist[[300L]]), pch = 21, bg=grey(0.9), col = grey(.2))
invisible(dev.off())
cat("\\includegraphics{figures/c1.eps}\\includegraphics{figures/c2.eps}\\includegraphics{figures/c3.eps}",
sep = "")


###################################################
### code chunk number 56: NMOFex.Rnw:1185-1200
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
### code chunk number 57: NMOFex.Rnw:1205-1222
###################################################
algo <- list(nP = 200L, nG = 100L,
             F = 0.50, CR = 0.99,
             min = c( 0,-10,-10,  0),
             max = c( 1, 10, 10, 10),
             storeSolutions = TRUE, printBar = FALSE)


## set up yield curve and put information in Data
tm <- 1:20                ### times to maturity
parTRUE <- c(5, 3, 2, 1)  ### true parameters
yM <- NS(parTRUE, tm)     ### true market yields
Data <- list(yM = yM, tm = tm, model = NS, ww = 0.1, maxb1 = 4)

## solve with DEopt
sol <- DEopt(OF = OF, algo = algo, Data = Data)
P <- sol$xlist[[1L]] ### all population matrices
p1 <- sapply(P, `[`, 1L, TRUE)


###################################################
### code chunk number 58: NMOFex.Rnw:1229-1238
###################################################
par(bty = "n", las = 1, mar = c(4,4,0,0),
    ps = 8, tck = 0.001, mgp = c(3, 0.2, 0))
plot(jitter(rep(seq_len(algo$nG), each = algo$nP), factor = 5),
     p1,
     pch = 21, cex = 0.01, ylim = c(-5,10),
     xlab = "", ylab = "")

mtext("generation", 1, line = 2)
mtext("parameter\nvalue", 2, line = 1)


###################################################
### code chunk number 59: NMOFex.Rnw:1245-1280
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
    ps = 8, tck = 0.001, mgp = c(3, 0.2, 0))
plot(jitter(rep(seq_len(algo$nG), each = algo$nP), factor = 5),
     p1,
     pch = 21, cex = 0.01, ylim = c(-5,10),
     xlab = "", ylab = "")
abline(h = 4, col=grey(0.5))
mtext("generation", 1, line = 2)
mtext("parameter\nvalue", 2, line = 1)


###################################################
### code chunk number 60: NMOFex.Rnw:1292-1302
###################################################
testFun <- function(x) {
    Sys.sleep(0.1) ## wasting time :)
    cos(1/x^2)
}
system.time(sol1 <- bracketing(testFun, interval = c(0.3, 0.9),
                           n = 100L))
system.time(sol2 <- bracketing(testFun, interval = c(0.3, 0.9),
                           n = 100L, method = "snow", cl = 4))

all.equal(sol1, sol2)


###################################################
### code chunk number 61: NMOFex.Rnw:1344-1345
###################################################
toLatex(sessionInfo())


