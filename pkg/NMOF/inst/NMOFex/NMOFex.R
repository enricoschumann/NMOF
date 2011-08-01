### R code from vignette source 'NMOFex.Rnw'

###################################################
### code chunk number 1: NMOFex.Rnw:72-75
###################################################
# version 2011-08-01
options(continue = " ")
options(digits = 3)


###################################################
### code chunk number 2: NMOFex.Rnw:85-86 (eval = FALSE)
###################################################
## install.packages("NMOF", repos = "http://R-Forge.R-project.org")


###################################################
### code chunk number 3: NMOFex.Rnw:89-91
###################################################
require("NMOF")
set.seed(124689)


###################################################
### code chunk number 4: NMOFex.Rnw:95-97 (eval = FALSE)
###################################################
## whereToLook <- system.file("NMOFex/NMOFex.R", package = "NMOF")
## file.show(whereToLook, title = "NMOF examples")


###################################################
### code chunk number 5: NMOFex.Rnw:119-126
###################################################
# create random data with vols between minVol and maxVol 
# and pairwise correlation of 0.6
na <- 500L
C <- array(0.5, dim = c(na,na)); diag(C) <- 1
minVol <- 0.20; maxVol <- 0.40
Vols <- (maxVol - minVol) * runif(na) + minVol
Sigma <- outer(Vols,Vols) * C


###################################################
### code chunk number 6: NMOFex.Rnw:134-152
###################################################
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
### code chunk number 7: NMOFex.Rnw:157-163
###################################################
# data 
data <- list(Sigma = Sigma, # cov-matrix
              Kinf = 30L,   # min cardinality
              Ksup = 60L,   # max cardinality
                na = na,    # number of assets
                nn = 1L)    # assets to change per iteration


###################################################
### code chunk number 8: NMOFex.Rnw:167-172
###################################################
# a random solution x0
card0 <- sample(data$Kinf:data$Ksup, 1L, replace = FALSE) 
assets <- sample.int(na, card0, replace = FALSE)
x0 <- logical(na)
x0[assets] <- TRUE


###################################################
### code chunk number 9: NMOFex.Rnw:177-185
###################################################
# *Local Search*
algo <- list(x0 = x0, neighbour = neighbour, nS = 5000L, 
    printDetail = FALSE, printBar = FALSE)
system.time(solLS <- LSopt(OF, algo = algo, data = data))

# *Threshold Accepting*
algo$nT <- 10; algo$nS <- trunc(algo$nS/algo$nT); algo$q <- 0.2
system.time(solTA <- TAopt(OF, algo = algo, data = data))


###################################################
### code chunk number 10: NMOFex.Rnw:191-202
###################################################
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
### code chunk number 11: NMOFex.Rnw:209-214
###################################################
cat(
"Local Search        ", format(sqrt(solLS$OFvalue), digits = 4), "\n", 
"Threshold Accepting ", format(sqrt(solTA$OFvalue), digits = 4), "\n", 
"Genetic Algorithm   ", format(sqrt(solGA$OFvalue), digits = 4), "\n", 
sep = "")


###################################################
### code chunk number 12: NMOFex.Rnw:229-237
###################################################
na <- 100   # number of assets
ns <- 200   # number of scenarios
vols <- runif(na, min = 0.2, max = 0.4)
C <- matrix(0.6, na, na); diag(C) <- 1
R <- rnorm(ns * na)/16  # random returns, 
dim(R) <- c(ns, na)     
R <- R %*% chol(C)    
R <- R %*% diag(vols) 


###################################################
### code chunk number 13: NMOFex.Rnw:255-267
###################################################
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
### code chunk number 14: NMOFex.Rnw:271-274
###################################################
x0 <- data$min + runif(data$na)*(data$max - data$min)
x0[1:5]
sum(x0)


###################################################
### code chunk number 15: NMOFex.Rnw:277-281
###################################################
temp <- R %*% x0             # compute portfolio returns
temp <- temp - data$theta    
temp <- (temp[temp < 0])^2
sum(temp)/ns                 # semivariance


###################################################
### code chunk number 16: NMOFex.Rnw:284-291
###################################################
OF <- function(x, data) {
    Rx <- crossprod(data$R, x)
    Rx <- Rx - data$theta
    Rx <- Rx - abs(Rx)
    Rx <- Rx * Rx
    colSums(Rx) /(4*data$ns)     
}


###################################################
### code chunk number 17: NMOFex.Rnw:296-298
###################################################
OF(x0, data)
OF(cbind(x0, x0), data)


###################################################
### code chunk number 18: NMOFex.Rnw:302-312
###################################################
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
### code chunk number 19: NMOFex.Rnw:315-320
###################################################
sum(x0)
sum(repair(x0, data))

colSums(repair(cbind(x0,x0),data))
colSums(repair2(cbind(x0,x0),data))


###################################################
### code chunk number 20: NMOFex.Rnw:323-334
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
### code chunk number 21: NMOFex.Rnw:338-344
###################################################
x0[1L] <- 0.30
penalty(x0, data)
penalty(cbind(x0, x0), data)
x0[1L] <- 0
penalty(x0, data)
penalty(cbind(x0, x0), data)


###################################################
### code chunk number 22: NMOFex.Rnw:347-358
###################################################
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
### code chunk number 23: NMOFex.Rnw:361-368
###################################################
system.time(sol <- DEopt(OF = OF,algo = algo,data = data))
16 * 100 * sqrt(sol$OFvalue)      # solution quality

# check constraints
all(all.equal(sum(sol$xbest),1),  # budget constraint
    sol$xbest <= data$max,
    sol$xbest >= data$min)


###################################################
### code chunk number 24: NMOFex.Rnw:373-380
###################################################
# looping over the population
algo$loopOF <- TRUE; algo$loopPen <- TRUE; algo$loopRepair <- TRUE
system.time(sol <- DEopt(OF = OF,algo = algo, data = data))

# evaluating the population in one step
algo$loopOF <- FALSE; algo$loopPen <- FALSE; algo$loopRepair <- FALSE
system.time(sol <- DEopt(OF = OF,algo = algo, data = data))


###################################################
### code chunk number 25: NMOFex.Rnw:384-390
###################################################
algo$printDetail <- FALSE  
restartsDE <- restartOpt(fun = DEopt,   
    n = 20L, OF = OF,  algo = algo, data = data) 
OFvaluesDE <- sapply(restartsDE, `[[`, "OFvalue")  # extract best solution
OFvaluesDE <- 16 * 100 * sqrt(OFvaluesDE)
weightsDE <- sapply(restartsDE, `[[`, "xbest")


###################################################
### code chunk number 26: NMOFex.Rnw:393-398
###################################################
par(bty="n", las = 1,mar = c(3,4,0,0), 
     ps = 8, tck = 0.001, mgp = c(3, 0.2, 0))
plot(sort(OFvaluesDE), (1:length(OFvaluesDE)) / length(OFvaluesDE), 
    type="S", ylim = c(0,1), xlab = "", ylab = "")
mtext("OF value",  side = 1, line = 1)


###################################################
### code chunk number 27: NMOFex.Rnw:402-408
###################################################
par(bty="n", las = 1,mar = c(3,4,0,0), 
     ps = 8, tck = 0.001, mgp = c(3, 0.2, 0))
boxplot(t(weightsDE), 
    outline = FALSE, boxwex = 0.4, ylim = c(-0.06,0.06))
mtext("assets",  side = 1, line = 1)
mtext("weights", side = 2, line = 1.3, las = 1, padj = -5)


###################################################
### code chunk number 28: NMOFex.Rnw:417-449
###################################################
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
### code chunk number 29: NMOFex.Rnw:455-462
###################################################
weightsDE <- sapply(restartsDE, `[[`, "xbest")
par(bty="n", las = 1,mar = c(3,4,0,0), 
     ps = 8, tck = 0.001, mgp = c(3, 0.2, 0))
boxplot(t(weightsDE), 
    outline = FALSE, boxwex = 0.4, ylim = c(-0.06,0.06))
mtext("assets",  side = 1, line = 1)
mtext("weights", side = 2, line = 1.3, las = 1, padj = -5)


###################################################
### code chunk number 30: NMOFex.Rnw:467-485
###################################################
algo <- list(
    nP = 100,        # population size
    nG = 1000,       # number of generations
    c1 = 0.5,        # weight for individually best solution
    c2 = 1.5,        # weight for overall best solution
    min = data$min, 
    max = data$max,
    repair = repair, pen = penalty,
    iner = 0.7, initV = 1, maxV = 0.2,
    printBar = FALSE, printDetail = TRUE)

system.time(sol <- PSopt(OF = OF,algo = algo,data = data))
16 * 100 * sqrt(sol$OFvalue)      # solution quality

# check constraints
all(all.equal(sum(sol$xbest),1),  # budget constraint
    sol$xbest <= data$max,
    sol$xbest >= data$min)


###################################################
### code chunk number 31: NMOFex.Rnw:489-520
###################################################
# adjusting velocity
changeV <- function(x, data) {
    myFun <- function(x) x - (sum(x))/data$na
    if (is.null(dim(x)[2L]))
        myFun(x) else apply(x,2,myFun)
}
sum(changeV(x0, data))
colSums(changeV(cbind(x0, x0), data))

# initial population that meets budget constraint 
initP <- data$min + diag(data$max - data$min) %*% 
    array(runif(length(data$min) * algo$nP), 
        dim = c(length(data$min),  algo$nP))
colSums(initP <- repair(initP,data))[1:10]

# add to 'algo'
algo$changeV <- changeV  # function to adjust velocity
algo$initP <- initP      # initial population
algo$repair <- NULL      # not needed anymore
system.time(sol <- PSopt(OF = OF,algo = algo, data = data))
16 * 100 * sqrt(sol$OFvalue)      # solution quality

# check constraints
all(all.equal(sum(sol$xbest),1),  # budget constraint
    sol$xbest <= data$max,
    sol$xbest >= data$min)

# vectorised
algo$loopOF <- FALSE; algo$loopPen <- FALSE
algo$loopRepair <- FALSE; algo$loopChangeV <- FALSE
system.time(sol <- PSopt(OF = OF,algo = algo, data = data))


###################################################
### code chunk number 32: NMOFex.Rnw:523-533
###################################################
algo$printDetail <- FALSE  
restartsPS <- restartOpt(fun = PSopt,   
    n = 20L, OF = OF,  algo = algo, data = data) 
OFvaluesPS <- sapply(restartsPS, `[[`, "OFvalue")  # extract best solution
OFvaluesPS <- 16 * 100 * sqrt(OFvaluesPS)
par(bty="n", las = 1,mar = c(3,4,0,0), 
    ps = 8, tck = 0.001, mgp = c(3, 0.2, 0))
plot(sort(OFvaluesPS), (1:length(OFvaluesPS)) / length(OFvaluesPS), 
    type="S", ylim = c(0,1), xlab = "", ylab = "")
mtext("OF value",  side = 1, line = 1)


###################################################
### code chunk number 33: NMOFex.Rnw:539-561
###################################################
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
### code chunk number 34: NMOFex.Rnw:564-577
###################################################
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
### code chunk number 35: NMOFex.Rnw:580-593
###################################################
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
### code chunk number 36: NMOFex.Rnw:609-640
###################################################
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
### code chunk number 37: NMOFex.Rnw:644-648
###################################################
x0 <- logical(data$p)
temp <- sample.int(data$maxk, 1L)
temp <- sample.int(data$p, temp)
x0[temp] <- TRUE


###################################################
### code chunk number 38: NMOFex.Rnw:654-664
###################################################
system.time({
        for (i in seq_len(1000)) ignore1 <- lm(data$y ~ -1 + data$X[ ,x0])
    })

system.time({
        for (i in seq_len(1000)) ignore2 <- qr.solve(data$X[ ,x0], data$y)
    })

# ... should give the same coefficients
all.equal(as.numeric(coef(ignore1)), as.numeric(ignore2))


###################################################
### code chunk number 39: NMOFex.Rnw:671-677
###################################################
OF <- function(x, data) {
    q <- qr(data$X[ ,x])
    e <- qr.resid(q, data$y)
    log(crossprod(e)/data$n) + sum(x) * data$lognn
}
OF(x0, data)


###################################################
### code chunk number 40: NMOFex.Rnw:681-691
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
### code chunk number 41: NMOFex.Rnw:696-707
###################################################
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
### code chunk number 42: NMOFex.Rnw:712-716
###################################################
xtrue <- logical(data$p)
xtrue[rD$K] <- TRUE
OF(sol1$xbest, data)
OF(xtrue, data)


###################################################
### code chunk number 43: NMOFex.Rnw:721-729
###################################################
restarts <- 50L
algo$printDetail <- FALSE
res <- restartOpt(TAopt, n = restarts, OF = OF, algo = algo, data = data)
par(bty = "n", las = 1,mar = c(3,4,0,0), 
    ps = 8, tck = 0.001, mgp = c(3, 0.2, 0))
plot(ecdf(sapply(res, `[[`, "OFvalue")),   # extract solution quality
    cex = 0.4, main = "", ylab = "", xlab = "")
    


###################################################
### code chunk number 44: NMOFex.Rnw:733-741
###################################################
xbestAll <- sapply(res, `[[`, "xbest")    # extract all solutions
inclReg <- which(rowSums(xbestAll) > 0L)  # get included regressors
inclReg <- sort(union(rD$K, inclReg))
data.frame(
    regressor = inclReg, 
    times_included = paste(rowSums(xbestAll)[inclReg], "/", 
                           restarts, sep = ""),
    true_regressor = inclReg %in% rD$K)


