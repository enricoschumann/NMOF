###################################################
### chunk number 1: 
###################################################
#line 72 "NMOFex.Rnw"
# version 2011-07-24
options(continue = " ")
options(digits = 3)


###################################################
### chunk number 2:  eval=FALSE
###################################################
## #line 85 "NMOFex.Rnw"
## install.packages("NMOF", repos = "http://R-Forge.R-project.org")


###################################################
### chunk number 3: 
###################################################
#line 89 "NMOFex.Rnw"
require("NMOF")
set.seed(124689)


###################################################
### chunk number 4:  eval=FALSE
###################################################
## #line 95 "NMOFex.Rnw"
## whereToLook <- system.file("NMOFex/NMOFex.R", package = "NMOF")
## file.show(whereToLook, title = "NMOF examples")


###################################################
### chunk number 5: 
###################################################
#line 119 "NMOFex.Rnw"
# create random data with vols between minVol and maxVol 
# and pairwise correlation of 0.6
na <- 500L
C <- array(0.5, dim = c(na,na)); diag(C) <- 1
minVol <- 0.20; maxVol <- 0.40
Vols <- (maxVol - minVol) * runif(na) + minVol
Sigma <- outer(Vols,Vols) * C


###################################################
### chunk number 6: 
###################################################
#line 134 "NMOFex.Rnw"
# objective function for LS/TA
OF <- function(x, data) {
    w <- x/sum(x)
    res <- crossprod(w[x], data$Sigma[x,x])
    res <- tcrossprod(w[x], res)
    res
}

# neighbourhood function for LS/TA
neighbour <- function(xc, data) {
    xn <- xc
    p <- sample.int(data$na, data$nn, replace = FALSE)
    xn[p] <- !xn[p]
    # reject infeasible solution
    sumx <- sum(xn)
    if ( (sumx > data$Ksup) || (sumx < data$Kinf) )
        xc else xn
}


###################################################
### chunk number 7: 
###################################################
#line 157 "NMOFex.Rnw"
# data 
data <- list(Sigma = Sigma, # cov-matrix
              Kinf = 30L,   # min cardinality
              Ksup = 60L,   # max cardinality
                na = na,    # number of assets
                nn = 1L)    # assets to change per iteration


###################################################
### chunk number 8: 
###################################################
#line 167 "NMOFex.Rnw"
# a random solution x0
card0 <- sample(data$Kinf:data$Ksup, 1L, replace = FALSE) 
assets <- sample.int(na, card0, replace = FALSE)
x0 <- logical(na)
x0[assets] <- TRUE


###################################################
### chunk number 9: 
###################################################
#line 177 "NMOFex.Rnw"
# *Local Search*
algo <- list(x0 = x0, neighbour = neighbour, nS = 5000L, 
    printDetail = FALSE, printBar = FALSE)
system.time(solLS <- LSopt(OF, algo = algo, data = data))

# *Threshold Accepting*
algo$nT <- 10; algo$nS <- trunc(algo$nS/algo$nT); algo$q <- 0.2
system.time(solTA <- TAopt(OF, algo = algo, data = data))


###################################################
### chunk number 10: 
###################################################
#line 191 "NMOFex.Rnw"
# *Genetic Algorithm*
OF2 <- function(x, data) {
    res <- colSums(data$Sigma %*% x * x)
    n <- colSums(x); res <- res / n^2
    # penalise
    p <- pmax(data$Kinf - n, 0) + pmax(n - data$Ksup, 0)
    res + p
}
algo <- list(nB = na, nP = 100L, nG = 500L, prob = 0.002, 
printBar = FALSE, loopOF = FALSE)
system.time(solGA <- GAopt(OF = OF2, algo = algo, data = data))


###################################################
### chunk number 11: 
###################################################
#line 209 "NMOFex.Rnw"
cat(
"Local Search        ", format(sqrt(solLS$OFvalue), digits = 4), "\n", 
"Threshold Accepting ", format(sqrt(solTA$OFvalue), digits = 4), "\n", 
"Genetic Algorithm   ", format(sqrt(solGA$OFvalue), digits = 4), "\n", 
sep = "")


###################################################
### chunk number 12: 
###################################################
#line 228 "NMOFex.Rnw"
na <- 100   # number of assets
ns <- 200   # number of scenarios
vols <- runif(na, min = 0.2, max = 0.4)
C <- matrix(0.6, na, na); diag(C) <- 1
R <- rnorm(ns * na)/16  # random returns, 
dim(R) <- c(ns, na)     
R <- R %*% chol(C)    
R <- R %*% diag(vols) 


###################################################
### chunk number 13: 
###################################################
#line 254 "NMOFex.Rnw"
data <- list(
    R = t(R),              # scenarios
    theta = 0.005,         # return threshold
    na = na,               # number of assets 
    ns = ns,               # number of scenarios
    max = rep( 0.05, na),  # DE: vector of max. weight
    min = rep(-0.05, na),  # DE: vector of min. weight
    wsup =  0.05,          # TA: max weight
    winf = -0.05,          # TA: min weight
    eps = 0.5/100,         # TA: step size
    w = 1                  # penalty weight
)


###################################################
### chunk number 14: 
###################################################
#line 270 "NMOFex.Rnw"
x0 <- data$min + runif(data$na)*(data$max - data$min)
x0[1:5]
sum(x0)


###################################################
### chunk number 15: 
###################################################
#line 276 "NMOFex.Rnw"
temp <- R %*% x0             # compute portfolio returns
temp <- temp - data$theta    
temp <- (temp[temp < 0])^2
sum(temp)/ns                 # semivariance


###################################################
### chunk number 16: 
###################################################
#line 283 "NMOFex.Rnw"
OF <- function(x, data) {
    Rx <- crossprod(data$R, x)
    Rx <- Rx - data$theta
    Rx <- Rx - abs(Rx)
    Rx <- Rx * Rx
    colSums(Rx) /(4*data$ns)     
}


###################################################
### chunk number 17: 
###################################################
#line 295 "NMOFex.Rnw"
OF(x0, data)
OF(cbind(x0, x0), data)


###################################################
### chunk number 18: 
###################################################
#line 301 "NMOFex.Rnw"
repair <- function(x, data) {
    myFun <- function(x) x/sum(x)
    if (is.null(dim(x)[2L]))
        myFun(x) else apply(x,2,myFun)
}
repair2 <- function(x, data) {
    myFun <- function(x) x + (1 - sum(x))/data$na
    if (is.null(dim(x)[2L]))
        myFun(x) else apply(x,2,myFun)
}


###################################################
### chunk number 19: 
###################################################
#line 314 "NMOFex.Rnw"
sum(x0)
sum(repair(x0, data))

colSums(repair(cbind(x0,x0),data))
colSums(repair2(cbind(x0,x0),data))


###################################################
### chunk number 20: 
###################################################
#line 322 "NMOFex.Rnw"
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
### chunk number 21: 
###################################################
#line 337 "NMOFex.Rnw"
x0[1L] <- 0.30
penalty(x0, data)
penalty(cbind(x0, x0), data)
x0[1L] <- 0
penalty(x0, data)
penalty(cbind(x0, x0), data)


###################################################
### chunk number 22: 
###################################################
#line 346 "NMOFex.Rnw"
algo <- list(
    nP = 100,        # population size
    nG = 1000,       # number of generations
    F = 0.25,        # step size
    CR = 0.9, 
    min = data$min, 
    max = data$max,
    repair = repair, 
    pen = penalty,
    printBar = FALSE, printDetail = TRUE,
    loopOF = TRUE, loopPen = TRUE, loopRepair = TRUE)


###################################################
### chunk number 23: 
###################################################
#line 360 "NMOFex.Rnw"
system.time(sol <- DEopt(OF = OF,algo = algo,data = data))
16 * 100 * sqrt(sol$OFvalue)      # solution quality

# check constraints
all(all.equal(sum(sol$xbest),1),  # budget constraint
    sol$xbest <= data$max,
    sol$xbest >= data$min)


###################################################
### chunk number 24: 
###################################################
#line 372 "NMOFex.Rnw"
# looping over the population
algo$loopOF <- TRUE; algo$loopPen <- TRUE; algo$loopRepair <- TRUE
system.time(sol <- DEopt(OF = OF,algo = algo, data = data))

# evaluating the population in one step
algo$loopOF <- FALSE; algo$loopPen <- FALSE; algo$loopRepair <- FALSE
system.time(sol <- DEopt(OF = OF,algo = algo, data = data))


###################################################
### chunk number 25: 
###################################################
#line 383 "NMOFex.Rnw"
algo$printDetail <- FALSE  
restartsDE <- restartOpt(fun = DEopt,   
    n = 20L, OF = OF,  algo = algo, data = data) 
OFvaluesDE <- sapply(restartsDE, `[[`, "OFvalue")  # extract best solution
OFvaluesDE <- 16 * 100 * sqrt(OFvaluesDE)
weightsDE <- sapply(restartsDE, `[[`, "xbest")


###################################################
### chunk number 26: 
###################################################
#line 392 "NMOFex.Rnw"
par(bty="n", las = 1,mar = c(3,4,0,0), 
     ps = 8, tck = 0.001, mgp = c(3, 0.2, 0))
plot(sort(OFvaluesDE), (1:length(OFvaluesDE)) / length(OFvaluesDE), 
    type="S", ylim = c(0,1), xlab = "", ylab = "")
mtext("OF value",  side = 1, line = 1)


###################################################
### chunk number 27: 
###################################################
#line 401 "NMOFex.Rnw"
par(bty="n", las = 1,mar = c(3,4,0,0), 
     ps = 8, tck = 0.001, mgp = c(3, 0.2, 0))
boxplot(t(weightsDE), 
    outline = FALSE, boxwex = 0.4, ylim = c(-0.06,0.06))
mtext("assets",  side = 1, line = 1)
mtext("weights", side = 2, line = 1.3, las = 1, padj = -5)


###################################################
### chunk number 28: 
###################################################
#line 416 "NMOFex.Rnw"
algo$printDetail <- FALSE;  algo$nP <- 200L; restarts <- 20L
nGs <- c(500L, 1000L, 2000L)
lstOFvaluesDE <- list()
for (i in 1:3) {
    algo$nG <- nGs[i]
    restartsDE <- restartOpt(fun = DEopt,   
        n = restarts, OF = OF,  algo = algo, data = data) 
    OFvaluesDE <- sapply(restartsDE, `[[`, "OFvalue")  # extract best solution
    OFvaluesDE <- 16 * 100 * sqrt(OFvaluesDE)
    lstOFvaluesDE[[i]] <- OFvaluesDE
}
res <- simplify2array(lstOFvaluesDE)
# now with repair2 
algo$repair <- repair2
lstOFvaluesDE <- list()
for (i in 1:3) {
    algo$nG <- nGs[i]
    restartsDE <- restartOpt(fun = DEopt,   
        n = restarts, OF = OF,  algo = algo, data = data) 
    OFvaluesDE <- sapply(restartsDE, `[[`, "OFvalue")  # extract best solution
    OFvaluesDE <- 16 * 100 * sqrt(OFvaluesDE)
    lstOFvaluesDE[[i]] <- OFvaluesDE
}
res2 <- simplify2array(lstOFvaluesDE)
# plot results
allres <- as.vector(rbind(res,res2))
xlims <- pretty(allres); xlims <- c(min(xlims), max(xlims))
par(bty = "n", las = 1,mar = c(3,4,0,0), 
    ps = 8, tck = 0.001, mgp = c(3, 0.2, 0))
plot(ecdf(res[,3]), xlim = xlims, cex = 0.4, main = "", ylab = "", xlab = "")
for (i in 1:2) lines(ecdf(res[,i]), cex = 0.4)
for (i in 1:3) lines(ecdf(res2[,i]), col = "blue", cex = 0.4)


###################################################
### chunk number 29: 
###################################################
#line 454 "NMOFex.Rnw"
weightsDE <- sapply(restartsDE, `[[`, "xbest")
par(bty="n", las = 1,mar = c(3,4,0,0), 
     ps = 8, tck = 0.001, mgp = c(3, 0.2, 0))
boxplot(t(weightsDE), 
    outline = FALSE, boxwex = 0.4, ylim = c(-0.06,0.06))
mtext("assets",  side = 1, line = 1)
mtext("weights", side = 2, line = 1.3, las = 1, padj = -5)


###################################################
### chunk number 30: 
###################################################
#line 467 "NMOFex.Rnw"
data$R <- R  # not transposed any more

neighbourU <- function(sol, data){
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
resample <- function(x, ...)  # see ?sample
    x[sample.int(length(x), ...)]


###################################################
### chunk number 31: 
###################################################
#line 492 "NMOFex.Rnw"
# a random initial weights
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
16*100*sqrt(sol2$OFvalue)


###################################################
### chunk number 32: 
###################################################
#line 508 "NMOFex.Rnw"
restartsTA <- restartOpt(fun = TAopt, n = 20L, 
    OF = OF, algo = algo, data = data)

OFvaluesTA <- sapply(restartsTA, `[[`, "OFvalue") # extract best solution
OFvaluesTA <- 16 * 100 * sqrt(OFvaluesTA)
weightsTA <- sapply(restartsTA, `[[`, "xbest")
par(bty="n", las = 1,mar = c(3,4,0,0), ps = 8, tck = 0.001, mgp = c(3, 0.2, 0))
# blue: DE solution with nP = 200 and nG = 2000
xlims <- pretty(c(res2[,3], OFvaluesTA))
plot(ecdf(res2[,3]), col = "blue", cex = 0.4, main = "", ylab = "", xlab = "",
xlim = c(min(xlims), max(xlims)) ) 
# black: TA
lines(ecdf(OFvaluesTA), cex = 0.4) 


###################################################
### chunk number 33: 
###################################################
#line 537 "NMOFex.Rnw"
randomData <- function(
    p = 200L,      # number of available regressors
    n = 200L,      # number of observations
    maxReg = 10L,  # max. number of included regressors 
    s = 1,         # standard deviation of residuals
    constant = TRUE ) {
    
    X <- rnorm(n * p); dim(X) <- c(n, p)  # regressor matrix X
    if (constant) X[ ,1L] <- 1
    
    k <- sample.int(maxReg, 1L)     # the number of true regressors
    K <- sort(sample.int(p, k))     # the set of true regressors
    betatrue <- rnorm(k)            # the true coefficients
    
    # the response variable y
    y <- X[ ,K] %*% as.matrix(betatrue) + rnorm(n, sd = s)
    
    list(X = X, y = y, betatrue = betatrue, K = K, n = n, p = p)
}

rD <- randomData(p = 100L, n = 200L, s = 1, 
    constant = TRUE, maxReg = 15L)

data <- list(
    X = rD$X, 
    y = rD$y, 
    n = rD$n,
    p = rD$p,
    maxk = 30L,  # maximum number of regressors included in model 
    lognn = log(rD$n)/rD$n 
)


###################################################
### chunk number 34: 
###################################################
#line 572 "NMOFex.Rnw"
x0 <- logical(data$p)
temp <- sample.int(data$maxk, 1L)
temp <- sample.int(data$p, temp)
x0[temp] <- TRUE


###################################################
### chunk number 35: 
###################################################
#line 582 "NMOFex.Rnw"
system.time({
        for (i in seq_len(1000)) ignore1 <- lm(data$y ~ -1 + data$X[ ,x0])
    })

system.time({
        for (i in seq_len(1000)) ignore2 <- qr.solve(data$X[ ,x0], data$y)
    })

# ... should give the same coefficients
all.equal(as.numeric(coef(ignore1)), as.numeric(ignore2))


###################################################
### chunk number 36: 
###################################################
#line 599 "NMOFex.Rnw"
OF <- function(x, data) {
    q <- qr(data$X[ ,x])
    e <- qr.resid(q, data$y)
    log(crossprod(e)/data$n) + sum(x) * data$lognn
}
OF(x0, data)


###################################################
### chunk number 37: 
###################################################
#line 609 "NMOFex.Rnw"
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
### chunk number 38: 
###################################################
#line 624 "NMOFex.Rnw"
algo <- list(
    nT = 10L,    # number of thresholds
    nS = 200L,   # number of steps per threshold 
    nD = 1000L,  # number of random steps to compute thresholds 
    neighbour = neighbour, 
    x0 = x0, 
    printBar = FALSE)
system.time(sol1 <- TAopt(OF, algo = algo, data = data))
sol1$OFvalue
which(sol1$xbest)  # the selected regressors
rD$K               # the true regressors


###################################################
### chunk number 39: 
###################################################
#line 640 "NMOFex.Rnw"
xtrue <- logical(data$p)
xtrue[rD$K] <- TRUE
OF(sol1$xbest, data)
OF(xtrue, data)


###################################################
### chunk number 40: 
###################################################
#line 649 "NMOFex.Rnw"
restarts <- 50L
algo$printDetail <- FALSE
res <- restartOpt(TAopt, n = restarts, OF = OF, algo = algo, data = data)
par(bty = "n", las = 1,mar = c(3,4,0,0), 
    ps = 8, tck = 0.001, mgp = c(3, 0.2, 0))
plot(ecdf(sapply(res, `[[`, "OFvalue")),   # extract solution quality
    cex = 0.4, main = "", ylab = "", xlab = "")
    


###################################################
### chunk number 41: 
###################################################
#line 661 "NMOFex.Rnw"
xbestAll <- sapply(res, `[[`, "xbest")    # extract all solutions
inclReg <- which(rowSums(xbestAll) > 0L)  # get included regressors
inclReg <- sort(union(rD$K, inclReg))
data.frame(
    regressor = inclReg, 
    times_included = paste(rowSums(xbestAll)[inclReg], "/", 
                           restarts, sep = ""),
    true_regressor = inclReg %in% rD$K)


