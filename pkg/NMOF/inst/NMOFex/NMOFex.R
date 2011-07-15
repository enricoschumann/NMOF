###################################################
### chunk number 1: 
###################################################
#line 72 "NMOFex.Rnw"
# version 2011-07-15
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


###################################################
### chunk number 4:  eval=FALSE
###################################################
## #line 94 "NMOFex.Rnw"
## whereToLook <- system.file("NMOFex/NMOFex.R", package = "NMOF")
## file.show(whereToLook, title = "NMOF examples")


###################################################
### chunk number 5: 
###################################################
#line 118 "NMOFex.Rnw"
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
#line 133 "NMOFex.Rnw"
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
#line 156 "NMOFex.Rnw"
# data 
data <- list(Sigma = Sigma, # cov-matrix
              Kinf = 30L,   # min cardinality
              Ksup = 60L,   # max cardinality
                na = na,    # number of assets
                nn = 1L)    # assets to change per iteration


###################################################
### chunk number 8: 
###################################################
#line 166 "NMOFex.Rnw"
# a random solution x0
card0 <- sample(data$Kinf:data$Ksup, 1L, replace = FALSE) 
assets <- sample.int(na, card0, replace = FALSE)
x0 <- logical(na)
x0[assets] <- TRUE


###################################################
### chunk number 9: 
###################################################
#line 176 "NMOFex.Rnw"
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
#line 190 "NMOFex.Rnw"
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
#line 208 "NMOFex.Rnw"
cat(
"Local Search        ", format(sqrt(solLS$OFvalue), digits = 4), "\n", 
"Threshold Accepting ", format(sqrt(solTA$OFvalue), digits = 4), "\n", 
"Genetic Algorithm   ", format(sqrt(solGA$OFvalue), digits = 4), "\n", 
sep = "")


###################################################
### chunk number 12: 
###################################################
#line 227 "NMOFex.Rnw"
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
#line 253 "NMOFex.Rnw"
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
#line 269 "NMOFex.Rnw"
x0 <- data$min + runif(data$na)*(data$max - data$min)
x0[1:5]
sum(x0)


###################################################
### chunk number 15: 
###################################################
#line 275 "NMOFex.Rnw"
temp <- R %*% x0             # compute portfolio returns
temp <- temp - data$theta    
temp <- (temp[temp < 0])^2
sum(temp)/ns                 # semivariance


###################################################
### chunk number 16: 
###################################################
#line 282 "NMOFex.Rnw"
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
#line 294 "NMOFex.Rnw"
OF(x0, data)
OF(cbind(x0, x0), data)


###################################################
### chunk number 18: 
###################################################
#line 300 "NMOFex.Rnw"
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
#line 313 "NMOFex.Rnw"
sum(x0)
sum(repair(x0, data))

colSums(repair(cbind(x0,x0),data))
colSums(repair2(cbind(x0,x0),data))


###################################################
### chunk number 20: 
###################################################
#line 321 "NMOFex.Rnw"
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
#line 336 "NMOFex.Rnw"
x0[1L] <- 0.30
penalty(x0, data)
penalty(cbind(x0, x0), data)
x0[1L] <- 0
penalty(x0, data)
penalty(cbind(x0, x0), data)


###################################################
### chunk number 22: 
###################################################
#line 345 "NMOFex.Rnw"
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
#line 359 "NMOFex.Rnw"
system.time(sol <- DEopt(OF = OF,algo = algo,data = data))
16 * 100 * sqrt(sol$OFvalue)      # solution quality

# check constraints
all(all.equal(sum(sol$xbest),1),  # budget constraint
    sol$xbest <= data$max,
    sol$xbest >= data$min)


###################################################
### chunk number 24: 
###################################################
#line 371 "NMOFex.Rnw"
# looping over the population
algo$loopOF <- TRUE; algo$loopPen <- TRUE; algo$loopRepair <- TRUE
system.time(sol <- DEopt(OF = OF,algo = algo, data = data))

# evaluating the population in one step
algo$loopOF <- FALSE; algo$loopPen <- FALSE; algo$loopRepair <- FALSE
system.time(sol <- DEopt(OF = OF,algo = algo, data = data))


###################################################
### chunk number 25: 
###################################################
#line 382 "NMOFex.Rnw"
algo$printDetail <- FALSE  
restartsDE <- restartOpt(fun = DEopt,   
    n = 20L, OF = OF,  algo = algo, data = data) 
OFvaluesDE <- sapply(restartsDE, `[[`, "OFvalue")  # extract best solution
OFvaluesDE <- 16 * 100 * sqrt(OFvaluesDE)
weightsDE <- sapply(restartsDE, `[[`, "xbest")


###################################################
### chunk number 26: 
###################################################
#line 391 "NMOFex.Rnw"
par(bty="n", las = 1,mar = c(3,4,0,0), 
     ps = 8, tck = 0.001, mgp = c(3, 0.2, 0))
plot(sort(OFvaluesDE), (1:length(OFvaluesDE)) / length(OFvaluesDE), 
    type="S", ylim = c(0,1), xlab = "", ylab = "")
mtext("OF value",  side = 1, line = 1)


###################################################
### chunk number 27: 
###################################################
#line 400 "NMOFex.Rnw"
par(bty="n", las = 1,mar = c(3,4,0,0), 
     ps = 8, tck = 0.001, mgp = c(3, 0.2, 0))
boxplot(t(weightsDE), 
    outline = FALSE, boxwex = 0.4, ylim = c(-0.06,0.06))
mtext("assets",  side = 1, line = 1)
mtext("weights", side = 2, line = 1.3, las = 1, padj = -5)


###################################################
### chunk number 28: 
###################################################
#line 415 "NMOFex.Rnw"
algo$printDetail <- FALSE;  algo$nP <- 200L; restarts <- 20L
nGs <- c(500L, 1500L, 3000L)
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
par(bty="n", las = 1,mar = c(3,4,0,0), 
    ps = 8, tck = 0.001, mgp = c(3, 0.2, 0))
plot(ecdf(res[,3]), xlim = xlims, cex = 0.4, main = "", ylab = "", xlab = "")
for (i in 1:2) lines(ecdf(res[,i]), cex = 0.4)
for (i in 1:3) lines(ecdf(res2[,i]), col = "blue", cex = 0.4)


###################################################
### chunk number 29: 
###################################################
#line 453 "NMOFex.Rnw"
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
#line 466 "NMOFex.Rnw"
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
#line 491 "NMOFex.Rnw"
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
#line 507 "NMOFex.Rnw"
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


