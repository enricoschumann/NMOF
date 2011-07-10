###################################################
### chunk number 1: 
###################################################
#line 69 "NMOFex.Rnw"
# version 2011-07-10
options(continue = " ")
options(digits = 3)


###################################################
### chunk number 2:  eval=FALSE
###################################################
## #line 83 "NMOFex.Rnw"
## install.packages("NMOF", repos = "http://R-Forge.R-project.org")


###################################################
### chunk number 3: 
###################################################
#line 87 "NMOFex.Rnw"
require("NMOF")


###################################################
### chunk number 4: 
###################################################
#line 107 "NMOFex.Rnw"
na <- 100   # number of assets
ns <- 300   # number of scenarios
vols <- runif(na, min = 0.2, max = 0.4)
C <- matrix(0.6, na, na); diag(C) <- 1
R <- rnorm(ns * na)/16  # random returns, 
dim(R) <- c(ns, na)     #   scaled so as to resemble
R <- R %*% chol(C)      #   daily returns with a 
R <- R %*% diag(vols)   #   constant correlation


###################################################
### chunk number 5: 
###################################################
#line 129 "NMOFex.Rnw"
data <- list(
    R = t(R),              # scenarios
    theta = 0.005,         # MAR
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
### chunk number 6: 
###################################################
#line 145 "NMOFex.Rnw"
x0 <- data$min + runif(data$na)*(data$max - data$min)
x0[1:5]
sum(x0)


###################################################
### chunk number 7: 
###################################################
#line 151 "NMOFex.Rnw"
OF <- function(x, data) {
    Rw <- crossprod(data$R, x)
    Rw <- Rw - data$theta
    Rw <- Rw - abs(Rw)
    Rw <- Rw * Rw
    colSums(Rw) /(4*data$ns)     
}


###################################################
### chunk number 8: 
###################################################
#line 161 "NMOFex.Rnw"
temp <- R %*% x0             # compute portfolio returns
temp <- temp - data$theta    
temp <- (temp[temp < 0])^2
sum(temp)/ns                 # semivariance


###################################################
### chunk number 9: 
###################################################
#line 169 "NMOFex.Rnw"
OF(x0, data)
OF(cbind(x0, x0), data)


###################################################
### chunk number 10: 
###################################################
#line 174 "NMOFex.Rnw"
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
### chunk number 11: 
###################################################
#line 190 "NMOFex.Rnw"
sum(x0)
sum(repair(x0, data))

colSums(repair(cbind(x0,x0),data))
colSums(repair2(cbind(x0,x0),data))


###################################################
### chunk number 12: 
###################################################
#line 198 "NMOFex.Rnw"
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
### chunk number 13: 
###################################################
#line 211 "NMOFex.Rnw"
x0[1L] <- 0.30
penalty(x0, data)
penalty(cbind(x0, x0), data)
x0[1L] <- 0
penalty(x0, data)
penalty(cbind(x0, x0), data)


###################################################
### chunk number 14: 
###################################################
#line 220 "NMOFex.Rnw"
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
### chunk number 15: 
###################################################
#line 234 "NMOFex.Rnw"
sol <- DEopt(OF = OF,algo = algo,data = data)
16 * 100 * sqrt(sol$OFvalue)      # solution quality

# check constraints
all(all.equal(sum(sol$xbest),1),  # budget constraint
    sol$xbest <= data$max,
    sol$xbest >= data$min)


###################################################
### chunk number 16: 
###################################################
#line 244 "NMOFex.Rnw"
# looping over the population
algo$loopOF <- TRUE; algo$loopPen <- TRUE; algo$loopRepair <- TRUE
system.time(sol <- DEopt(OF = OF,algo = algo, data = data))

# evaluating the population in one step
algo$loopOF <- FALSE; algo$loopPen <- FALSE; algo$loopRepair <- FALSE
system.time(sol <- DEopt(OF = OF,algo = algo, data = data))


###################################################
### chunk number 17: 
###################################################
#line 255 "NMOFex.Rnw"
algo$printDetail <- FALSE  
restartsDE <- restartOpt(fun = DEopt,   
    n = 100L, OF = OF,  algo = algo, data = data) 
OFvaluesDE <- sapply(restartsDE, `[[`, "OFvalue")  # extract best solution
OFvaluesDE <- 16 * 100 * sqrt(OFvaluesDE)
weightsDE <- sapply(restartsDE, `[[`, "xbest")


###################################################
### chunk number 18: 
###################################################
#line 265 "NMOFex.Rnw"
par(bty="n", las = 1,mar = c(3,4,0,0), 
     ps = 8, tck = 0.001, mgp = c(3, 0.2, 0))
boxplot(t(weightsDE), 
    outline = FALSE, boxwex = 0.4, ylim = c(-0.06,0.06))
mtext("assets",  side = 1, line = 1)
mtext("weights", side = 2, line = 1.3, las = 1, padj = -5)


###################################################
### chunk number 19: 
###################################################
#line 274 "NMOFex.Rnw"
par(bty="n", las = 1,mar = c(3,4,0,0), 
     ps = 8, tck = 0.001, mgp = c(3, 0.2, 0))
plot(sort(OFvaluesDE), (1:length(OFvaluesDE)) / length(OFvaluesDE), 
    type="S", ylim = c(0,1), xlab = "", ylab = "")
mtext("OF value",  side = 1, line = 1)


###################################################
### chunk number 20: 
###################################################
#line 281 "NMOFex.Rnw"
algo$printDetail <- FALSE;  algo$nP <- 200
nGs <- c(200, 500, 1000, 2000)
lstOFvaluesDE <- list()
for (i in 1:4) {
    algo$nG <- nGs[i]
    restartsDE <- restartOpt(fun = DEopt,   
        n = 10L, OF = OF,  algo = algo, data = data) 
    OFvaluesDE <- sapply(restartsDE, `[[`, "OFvalue")  # extract best solution
    OFvaluesDE <- 16 * 100 * sqrt(OFvaluesDE)
    lstOFvaluesDE[[i]] <- OFvaluesDE
}
res <- simplify2array(lstOFvaluesDE)


algo$repair <- repair2
lstOFvaluesDE <- list()
for (i in 1:4) {
    algo$nG <- nGs[i]
    restartsDE <- restartOpt(fun = DEopt,   
        n = 10L, OF = OF,  algo = algo, data = data) 
    OFvaluesDE <- sapply(restartsDE, `[[`, "OFvalue")  # extract best solution
    OFvaluesDE <- 16 * 100 * sqrt(OFvaluesDE)
    lstOFvaluesDE[[i]] <- OFvaluesDE
}
res2 <- simplify2array(lstOFvaluesDE)


allres <- as.vector(rbind(res,res2))
xlims <- pretty(allres); xlims <- c(min(xlims), max(xlims))
par(bty="n", las = 1,mar = c(3,4,0,0), 
    ps = 8, tck = 0.001, mgp = c(3, 0.2, 0))
plot(ecdf(res[,4]), xlim = xlims)
for (i in 1:3) lines(ecdf(res[,i]))
for (i in 1:4) lines(ecdf(res2[,i]), col = "blue")


###################################################
### chunk number 21: 
###################################################
#line 319 "NMOFex.Rnw"
data$R <- R

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
    Rw <- x$Rw
    Rw <- Rw - data$theta
    Rw <- Rw - abs(Rw)
    Rw <- Rw * Rw
    colSums(Rw) /(4*data$ns)     
}
resample <- function(x, ...) x[sample.int(length(x), ...)]


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
sol2 <- TAopt(OF,algo,data)
16*100*sqrt(sol2$OFvalue)
sum(sol2$xbest$w)

summary(sol$xbest)
sum(sol$xbest)
sum(abs(sol$xbest))
summary(sol2$xbest$w)
sum(sol2$xbest$w)
sum(abs(sol2$xbest$w))


