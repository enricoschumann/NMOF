### R code from vignette source 'NMOFman.Rnw'

###################################################
### code chunk number 1: NMOFman.Rnw:3-7
###################################################
version <- as.Date("2012-04-18")
options(continue = " ", digits = 3, width = 70)
require("cacheSweave")
setCacheDir("./cache")


###################################################
### code chunk number 2: NMOFman.Rnw:128-130 (eval = FALSE)
###################################################
## whereToLook <- system.file("NMOFex/NMOFman.R", package = "NMOF")
## file.show(whereToLook, title = "NMOF manual")


###################################################
### code chunk number 3: NMOFman.Rnw:142-143
###################################################
require("NMOF")


###################################################
### code chunk number 4: NMOFman.Rnw:151-154
###################################################
require("rbenchmark")
resample <- function(x, ...)
    x[sample.int(length(x), ...)]


###################################################
### code chunk number 5: NMOFman.Rnw:161-162
###################################################
set.seed(123321)


###################################################
### code chunk number 6: NMOFman.Rnw:181-201
###################################################
## set number of rows
n <- 100L

## set a correlation between x and y
rho <- 0.7

## create correlation matrix
C <- array(rho, dim = c(2L, 2L))
diag(C) <- 1

## make columns of X correlated
ab <- array(rnorm(n * 2), dim = c(n, 2L)) %*% chol(C)
colnames(ab) <- c("a", "b")

par(mfrow = c(1L, 3L), bty = "n", mar = c(4, 4, 0, 0),
    tck = 0.01, las = 1, ps = 9, mgp = c(3, 1, 0.5))
plot(ab, pch = 19, cex = 0.5,
     xaxt = "n", yaxt = "n",
     xlab = "a", ylab = "b")
axis(1); axis(2)


###################################################
### code chunk number 7: NMOFman.Rnw:209-228
###################################################
par(mfrow = c(1L, 3L), bty = "n", mar = c(4, 4, 0, 0),
    tck = 0.01, las = 1, ps = 9, mgp = c(3,1,0.5))

## determine axis limits
prx <- pretty(ab)
lims <- c(min(prx), max(pretty(prx)))

## plot
plot(ab[ 1: 50, ], xlim = lims, ylim = lims, xlab = "a", ylab = "b",
col = grey(0.2), pch = 19, cex = 0.5, xaxt = "n", yaxt = "n")
axis(1); axis(2)
lines(ab[51:100, ], col = grey(0.75), pch = 19, type = "p", cex = 0.5)
axis(2)
plot(ab[ 1: 50, ], xlim=lims, ylim = lims, xlab = "a", ylab = "",
col = grey(0.2), pch = 19, cex = 0.5, xaxt = "n", yaxt = "n")
axis(1)
plot(ab[51:100, ], xlim=lims, ylim = lims, xlab = "a", ylab = "",
col = grey(0.75), pch = 19, cex = 0.5, xaxt = "n", yaxt = "n")
axis(1)


###################################################
### code chunk number 8: NMOFman.Rnw:234-236
###################################################
cor(ab[ 1: 50, ])
cor(ab[51:100, ])


###################################################
### code chunk number 9: NMOFman.Rnw:255-256
###################################################
x0 <- rep(c(TRUE, FALSE), each = 50L)


###################################################
### code chunk number 10: NMOFman.Rnw:261-262
###################################################
x0 <- runif(n) > 0.5


###################################################
### code chunk number 11: NMOFman.Rnw:273-275
###################################################
OF <- function(x, ab)
    -abs(cor(ab[x, ])[1L, 2L] - cor(ab[!x, ])[1L, 2L])


###################################################
### code chunk number 12: NMOFman.Rnw:281-284
###################################################
x0 <- rep(c(TRUE, FALSE), each = 50L)
OF( x0, ab)
OF(!x0, ab) ## should give the same result


###################################################
### code chunk number 13: NMOFman.Rnw:302-316
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
### code chunk number 14: NMOFman.Rnw:321-324
###################################################
summary(OFvalues)
xbest <- which.min(OFvalues)
OFvalues[xbest]


###################################################
### code chunk number 15: NMOFman.Rnw:353-358
###################################################
subset1 <- ab[ ,1L] * ab[ ,2L] >  0
subset2 <- ab[ ,1L] * ab[ ,2L] <= 0
c1 <- cor(ab[subset1, ])[1L, 2L]
c2 <- cor(ab[subset2, ])[1L, 2L]
OF(subset1, ab)


###################################################
### code chunk number 16: NMOFman.Rnw:362-364
###################################################
sum(subset1)
sum(subset2)


###################################################
### code chunk number 17: NMOFman.Rnw:371-378
###################################################
cr <- order(ab[ ,1L] * ab[ ,2L])
OFvalues <- numeric(n)
for (i in 20:80) {
  x0 <- logical(n)
  x0[cr[seq_len(i)]] <- TRUE
  OFvalues[i] <- OF(x0, ab)
}


###################################################
### code chunk number 18: NMOFman.Rnw:383-388
###################################################
cutoff <- which.min(OFvalues)
subset1 <- logical(n)
subset1[cr[seq_len(n) <= cutoff]] <- TRUE
subset2 <- !subset1
OF(subset1, ab)


###################################################
### code chunk number 19: NMOFman.Rnw:395-396
###################################################
xConstr <- subset1


###################################################
### code chunk number 20: NMOFman.Rnw:400-414
###################################################
par(mfrow = c(1L, 3L), bty="n", mar=c(4, 4, 0, 0),
tck = 0.01, las = 1, ps = 9)
prx <- pretty(ab); lims <- c(min(prx), max(pretty(prx)))
plot(ab[subset1, ], xlim = lims, ylim = lims, xlab = "a", ylab = "b",
col = grey(0.2), pch = 19, cex = 0.5, xaxt = "n", yaxt = "n")
axis(1); axis(2)
lines(ab[subset2, ], col = grey(0.75), pch = 19, type = "p", cex = 0.5)
axis(2)
plot(ab[subset1, ], xlim=lims, ylim = lims, xlab = "a", ylab = "",
col = grey(0.2), pch = 19, cex = 0.5, xaxt = "n", yaxt = "n")
axis(1)
plot(ab[subset2, ], xlim=lims, ylim = lims, xlab = "a", ylab = "",
col = grey(0.75), pch = 19, cex = 0.5, xaxt = "n", yaxt = "n")
axis(1)


###################################################
### code chunk number 21: NMOFman.Rnw:429-466
###################################################
x0 <- rep(c(TRUE, FALSE), each = 50L)
OF(x0, ab)

done <- FALSE
xbest <- xc <- x0
xbestF <- xcF <- OF(xbest, ab)
ic <- 0

while (!done) {
    if (ic > 200L)
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
        enough <- sxn >= 20
        notTooMany <- sxn <= 80
        if (enough && notTooMany) {

            xnF <- OF(xn, ab)
            if (xnF < xbestF) {
                xbest <- xn
                xbestF <- xnF
                done <- FALSE
            }
        }
    }
}
OF(xbest, ab)


###################################################
### code chunk number 22: NMOFman.Rnw:478-479
###################################################
otherData <- list(ab = ab, n = n, nmin = 20L)


###################################################
### code chunk number 23: NMOFman.Rnw:484-485
###################################################
x0 <- runif(n) > 0.5


###################################################
### code chunk number 24: NMOFman.Rnw:489-507
###################################################
neighbour <- function(xc, otherData) {
    xn <- xc

    ## choose which elements to change
    p <- sample.int(otherData$n, size = 1L)
    xn[p] <- !xn[p]

    ## check constraints
    sxn <- sum(xn)
    enough <- sxn >= otherData$nmin
    notTooMany <- sxn <= (otherData$n - otherData$nmin)

    ## if constraints are ok: return new solution, else old solution
    if (enough && notTooMany)
        xn
    else
        xc
}


###################################################
### code chunk number 25: NMOFman.Rnw:513-514
###################################################
table(x0 == neighbour(x0, otherData))


###################################################
### code chunk number 26: NMOFman.Rnw:518-525
###################################################
OF <- function(x, otherData)
    -abs(cor(otherData$ab[x, ])[1L, 2L] - cor(otherData$ab[!x, ])[1L, 2L])

## check
OF(x0, otherData)
## check
OF(neighbour(x0, otherData), otherData)


###################################################
### code chunk number 27: NMOFman.Rnw:529-535
###################################################
algo <- list(nS = 3000L,             ## number of steps to make
             neighbour = neighbour,  ## neighbourhood function
             x0 = x0,                ## initial solution
             printBar = FALSE)
sol1 <- LSopt(OF, algo = algo, otherData=otherData)
sol1$OFvalue


###################################################
### code chunk number 28: NMOFman.Rnw:542-546
###################################################
subset1 <-  sol1$xbest
subset2 <- !sol1$xbest
c1 <- cor(ab[subset1, ])[1L, 2L]
c2 <- cor(ab[subset2, ])[1L, 2L]


###################################################
### code chunk number 29: NMOFman.Rnw:550-566
###################################################
par(mfrow = c(1L, 3L), bty="n", mar=c(4, 4, 0, 0),
    tck = 0.01, las = 1, ps = 9)
prx <- pretty(ab)
lims <- c(min(prx), max(pretty(prx)))
plot(ab[subset1, ], xlim = lims, ylim = lims, xlab = "a", ylab = "b",
     col = grey(0.2), pch = 19, cex = 0.5, xaxt = "n", yaxt = "n")
axis(1)
axis(2)
lines(ab[subset2, ], col = grey(0.75), pch = 19, type = "p", cex = 0.5)
axis(2)
plot(ab[subset1, ], xlim=lims, ylim = lims, xlab = "a", ylab = "",
     col = grey(0.2), pch = 19, cex = 0.5, xaxt = "n", yaxt = "n")
axis(1)
plot(ab[subset2, ], xlim=lims, ylim = lims, xlab = "a", ylab = "",
     col = grey(0.75), pch = 19, cex = 0.5, xaxt = "n", yaxt = "n")
axis(1)


###################################################
### code chunk number 30: NMOFman.Rnw:578-584
###################################################
makex0 <- function() {
    x <- logical(100) ## solution size is not passed!
    while (sum(x) > 80 || sum(x) < 20)
        x <- runif(100) > 0.5
    x
}


###################################################
### code chunk number 31: NMOFman.Rnw:587-598
###################################################
algo <- list(nS = 4000L,             ## number of steps to make
             neighbour = neighbour,  ## neighbourhood function
             x0 = makex0,            ## initial solution
             printBar = FALSE,
             printDetail = FALSE)
restarts1 <- restartOpt(LSopt, 100, OF = OF, algo = algo, otherData)
restarts1OFvalues <- sapply(restarts1, `[[`, "OFvalue")

algo$nS <- 8000L
restarts2 <- restartOpt(LSopt, 100, OF = OF, algo = algo, otherData)
restarts2OFvalues <- sapply(restarts2, `[[`, "OFvalue")


###################################################
### code chunk number 32: NMOFman.Rnw:604-610
###################################################
par(bty = "n", las = 1, mar = c(3, 4, 0, 0), ps = 8, tck = 0.001)
plot( ecdf(restarts1OFvalues), main = "", ylab = "", xlab = "",
     cex = 0.4, pch = 19, col = grey(.2))
lines(ecdf(restarts2OFvalues),
     cex = 0.4, pch = 19, col = grey(.6))
abline(v = OF(xConstr, otherData))


###################################################
### code chunk number 33: NMOFman.Rnw:618-623
###################################################
algo$q <- 0.9
algo$nT <- 10
algo$nS <- 400
sol2 <- TAopt(OF, algo = algo, otherData = otherData)
sol2$OFvalue


###################################################
### code chunk number 34: NMOFman.Rnw:627-632
###################################################
subset1 <-  sol2$xbest
subset2 <- !sol2$xbest
c1 <- cor(ab[subset1, ])[1L, 2L]
c2 <- cor(ab[subset2, ])[1L, 2L]
OF(sol2$xbest, otherData)


###################################################
### code chunk number 35: NMOFman.Rnw:637-653
###################################################
par(mfrow = c(1L, 3L), bty="n", mar=c(4, 4, 0, 0),
    tck = 0.01, las = 1, ps = 9)
prx <- pretty(ab)
lims <- c(min(prx), max(pretty(prx)))
plot(ab[subset1, ], xlim = lims, ylim = lims, xlab = "a", ylab = "b",
     col = grey(0.2), pch = 19, cex = 0.5, xaxt = "n", yaxt = "n")
axis(1)
axis(2)
lines(ab[subset2, ], col = grey(0.75), pch = 19, type = "p", cex = 0.5)
axis(2)
plot(ab[subset1, ], xlim=lims, ylim = lims, xlab = "a", ylab = "",
     col = grey(0.2), pch = 19, cex = 0.5, xaxt = "n", yaxt = "n")
axis(1)
plot(ab[subset2, ], xlim=lims, ylim = lims, xlab = "a", ylab = "",
     col = grey(0.75), pch = 19, cex = 0.5, xaxt = "n", yaxt = "n")
axis(1)


###################################################
### code chunk number 36: NMOFman.Rnw:658-666
###################################################
algo$printBar <- FALSE
algo$printDetail <- FALSE
restarts3 <- restartOpt(TAopt, 100, OF = OF, algo = algo, otherData)
restarts3OFvalues <- sapply(restarts3, `[[`, "OFvalue")

algo$nS <- 1500
restarts4 <- restartOpt(TAopt, 100, OF = OF, algo = algo, otherData)
restarts4OFvalues <- sapply(restarts4, `[[`, "OFvalue")


###################################################
### code chunk number 37: NMOFman.Rnw:669-679
###################################################
par(bty = "n", las = 1, mar = c(3, 4, 0, 0), ps = 8, tck = 0.001)
plot( ecdf(restarts1OFvalues), main = "", ylab = "", xlab = "",
     cex = 0.4, pch = 19, col = grey(.2))
lines(ecdf(restarts2OFvalues),
     cex = 0.4, pch = 19, col = grey(.6))
lines(ecdf(restarts3OFvalues),
     cex = 0.4, pch = 19, col = grey(.2), lty=2)
lines(ecdf(restarts4OFvalues),
     cex = 0.4, pch = 19, col = grey(.6), lty=2)
abline(v = OF(xConstr, otherData))


###################################################
### code chunk number 38: NMOFman.Rnw:775-782
###################################################
OF <- tfTrefethen
n <- 100L
surf <- matrix(NA, n, n)
x1 <- seq(from = -10, to = 10, length.out = n)
for (i in seq_len(n))
    for (j in seq_len(n))
        surf[i, j] <- tfTrefethen(c(x1[i], x1[j]))


###################################################
### code chunk number 39: NMOFman.Rnw:790-796
###################################################
par(bty = "n", las = 1, mar = c(3,4,0,0),
    ps = 8, tck = 0.001, mgp = c(3, 0.5, 0))
contour(x1, x1, surf, nlevels=5, col = grey(0.6))

## the actual minimum
abline(v = -0.02440308, h = 0.21061243, col = grey(0.6))


###################################################
### code chunk number 40: NMOFman.Rnw:802-808
###################################################
algo <- list(nP = 50L, nG = 300L,
             F = 0.6, CR = 0.9,
             min = c(-10,-10), max = c(10,10),
             printDetail = FALSE, printBar = FALSE,
             storeF = TRUE, storeSolutions = TRUE)
sol <- DEopt(OF = OF, algo = algo)


###################################################
### code chunk number 41: NMOFman.Rnw:813-819
###################################################
names(sol)
sd(sol$popF)
ts.plot(sol$Fmat, xlab = "generations", ylab = "OF")

length(sol$xlist)
xlist <- sol$xlist[[1L]]


###################################################
### code chunk number 42: NMOFman.Rnw:828-843
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
### code chunk number 43: NMOFman.Rnw:849-863
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
### code chunk number 44: NMOFman.Rnw:869-878 (eval = FALSE)
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
### code chunk number 45: NMOFman.Rnw:882-911
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
### code chunk number 46: NMOFman.Rnw:925-940
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
### code chunk number 47: NMOFman.Rnw:947-964
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
### code chunk number 48: NMOFman.Rnw:971-980
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
### code chunk number 49: NMOFman.Rnw:988-1023
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
### code chunk number 50: NMOFman.Rnw:1109-1110
###################################################
tfRosenbrock


###################################################
### code chunk number 51: NMOFman.Rnw:1117-1121
###################################################
OF <- tfRosenbrock     ## see ?testFunctions
size <- 5L             ## set dimension
x <- rep.int(1, size)  ## the known solution ...
OF(x)                  ## ... should give zero


###################################################
### code chunk number 52: NMOFman.Rnw:1129-1136
###################################################
algo <- list(printBar = FALSE,
             nP = 50L,
             nG = 500L,
             F = 0.6,
             CR = 0.9,
             min = rep(-100, size),
             max = rep( 100, size))


###################################################
### code chunk number 53: NMOFman.Rnw:1143-1149
###################################################
## a vectorised OF: works only with *matrix* x
OF2 <- function(x) {
    n <- dim(x)[1L]
    xi <- x[1L:(n - 1L), ]
    colSums(100 * (x[2L:n, ] - xi * xi)^2 + (1 - xi)^2)
}


###################################################
### code chunk number 54: NMOFman.Rnw:1154-1158
###################################################
x <- matrix(rnorm(size * algo$nP), size, algo$nP)
c(OF(x[ ,1L]), OF(x[ ,2L]), OF(x[ ,3L]))
OF2(x)[1L:3L]  ## should give the same result
all.equal(OF2(x)[1L:3L], c(OF(x[ ,1L]), OF(x[ ,2L]), OF(x[ ,3L])))


###################################################
### code chunk number 55: NMOFman.Rnw:1169-1175
###################################################
set.seed(1223445)
(t1 <- system.time(sol <- DEopt(OF = OF, algo = algo)))

algo$loopOF <- FALSE
set.seed(1223445)
(t2 <- system.time(sol2 <- DEopt(OF = OF2, algo = algo)))


###################################################
### code chunk number 56: NMOFman.Rnw:1180-1183
###################################################
sol$OFvalue    ## both should be zero (with luck)
sol2$OFvalue
t1[[3L]]/t2[[3L]]  ## speedup


###################################################
### code chunk number 57: NMOFman.Rnw:1215-1226
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
### code chunk number 58: NMOFman.Rnw:1230-1250
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
### code chunk number 59: NMOFman.Rnw:1255-1257
###################################################
all.equal(res1,res2)
all.equal(res2,res3)


###################################################
### code chunk number 60: NMOFman.Rnw:1262-1265
###################################################
t1  ##  speedup for variant 1
t2  ##  speedup for variant 2
t3  ##  speedup for variant 3


###################################################
### code chunk number 61: NMOFman.Rnw:1289-1303
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
### code chunk number 62: NMOFman.Rnw:1307-1316
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
### code chunk number 63: NMOFman.Rnw:1323-1324
###################################################
all.equal(R1, R2)  ## ... should be TRUE


###################################################
### code chunk number 64: NMOFman.Rnw:1339-1349
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
### code chunk number 65: NMOFman.Rnw:1376-1380
###################################################
testFun  <- function(x) {
  Sys.sleep(0.1) ## just wasting time :-)
  x[1L] + x[2L]^2
}


###################################################
### code chunk number 66: NMOFman.Rnw:1385-1389
###################################################
lower <- c(1, 3); upper <- 5; n <- 5L
system.time(sol1 <- gridSearch(fun = testFun,
lower = lower, upper = upper,
n = n, printDetail = TRUE))


###################################################
### code chunk number 67: NMOFman.Rnw:1394-1396
###################################################
seq(from = 1, to = 5, length.out= n)  ## x_1
seq(from = 3, to = 5, length.out= n)  ## x_2


###################################################
### code chunk number 68: NMOFman.Rnw:1401-1403
###################################################
sol1$minfun
sol1$minlevels


###################################################
### code chunk number 69: NMOFman.Rnw:1408-1415
###################################################
system.time(sol2 <- gridSearch(fun = testFun,
lower = lower,
upper = upper,
n = n, printDetail = FALSE,
method = "snow",  ### use 'snow' ...
cl = 2L))         ### ... with 2 cores
all.equal(sol1, sol2)


###################################################
### code chunk number 70: NMOFman.Rnw:1434-1435
###################################################
cfBSM


###################################################
### code chunk number 71: NMOFman.Rnw:1439-1458
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
### code chunk number 72: NMOFman.Rnw:1469-1472
###################################################
v1 <- rnorm(200)
v2 <- runif(500)
v3 <- rbinom(100, size = 50, prob = 0.4)


###################################################
### code chunk number 73: NMOFman.Rnw:1476-1478
###################################################
cormat <- array(0.5, dim = c(3, 3))
diag(cormat) <- 1


###################################################
### code chunk number 74: NMOFman.Rnw:1482-1485
###################################################
results <- resampleC(v1 = v1, v2 = v2, v3 = v3,
size = 50, cormat = cormat)
cor(results, method = "spearman")


###################################################
### code chunk number 75: NMOFman.Rnw:1488-1502
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

pairs(results,
diag.panel = panel.hist,
gap = 0, pch = 21, cex = 0.5)



###################################################
### code chunk number 76: NMOFman.Rnw:1523-1533
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
### code chunk number 77: NMOFman.Rnw:1545-1551
###################################################
OF <- function(x, data) {
  sx <- sum(x)
  w <- rep.int(1/sx, sx)
  res <- crossprod(w, data$Sigma[x, x])
  tcrossprod(w, res)
}


###################################################
### code chunk number 78: NMOFman.Rnw:1555-1564
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
### code chunk number 79: NMOFman.Rnw:1571-1576
###################################################
data <- list(Sigma = Sigma,   ## cov-matrix
Kinf  = 30L,     ## min cardinality
Ksup  = 60L,     ## max cardinality
na    = na,      ## number of assets
nn    = 1L)      ## how many assets to change per iteration


###################################################
### code chunk number 80: NMOFman.Rnw:1580-1584
###################################################
card0 <- sample(data$Kinf:data$Ksup, 1L, replace = FALSE)
assets <- sample.int(na, card0, replace = FALSE)
x0 <- logical(na)
x0[assets] <- TRUE


###################################################
### code chunk number 81: NMOFman.Rnw:1590-1598
###################################################
## Local Search
algo <- list(x0 = x0, neighbour = neighbour, nS = 5000L,
             printDetail = FALSE, printBar = FALSE)
system.time(solLS <- LSopt(OF, algo = algo, data = data))

## Threshold Accepting
algo$nT <- 10L; algo$nS <- trunc(algo$nS/algo$nT); algo$q <- 0.2
system.time(solTA <- TAopt(OF, algo = algo, data = data))


###################################################
### code chunk number 82: NMOFman.Rnw:1680-1687
###################################################
OF2 <- function(x, data) {
    res <- colSums(data$Sigma %*% x * x)
    n <- colSums(x); res <- res / n^2
    ## penalise
    p <- pmax(data$Kinf - n, 0) + pmax(n - data$Ksup, 0)
    res + p
}


###################################################
### code chunk number 83: NMOFman.Rnw:1694-1697
###################################################
algo <- list(nB = na, nP = 100L, nG = 500L, prob = 0.002,
             printBar = FALSE, loopOF = FALSE)
system.time(solGA <- GAopt(OF = OF2, algo = algo, data = data))


###################################################
### code chunk number 84: NMOFman.Rnw:1702-1706
###################################################
cat("Local Search        ", format(sqrt(solLS$OFvalue), digits = 4), "\n",
    "Threshold Accepting ", format(sqrt(solTA$OFvalue), digits = 4), "\n",
    "Genetic Algorithm   ", format(sqrt(solGA$OFvalue), digits = 4), "\n",
    sep = "")


###################################################
### code chunk number 85: NMOFman.Rnw:1734-1742
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
### code chunk number 86: NMOFman.Rnw:1767-1777
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
### code chunk number 87: NMOFman.Rnw:1784-1787
###################################################
x0 <- data$min + runif(data$na)*(data$max - data$min)
x0[1:5]
sum(x0)


###################################################
### code chunk number 88: NMOFman.Rnw:1793-1797
###################################################
temp <- R %*% x0             ## compute portfolio returns
temp <- temp - data$theta    ## subtract return threshold
temp <- (temp[temp < 0])^2   ## select elements below threshold
sum(temp)/ns                 ## compute semivariance


###################################################
### code chunk number 89: NMOFman.Rnw:1803-1810
###################################################
OF <- function(x, data) {
  Rx <- crossprod(data$R, x)
  Rx <- Rx - data$theta
  Rx <- Rx - abs(Rx)
  Rx <- Rx * Rx
  colSums(Rx) /(4*data$ns)
}


###################################################
### code chunk number 90: NMOFman.Rnw:1819-1821
###################################################
OF(x0, data)
OF(cbind(x0, x0, x0), data)


###################################################
### code chunk number 91: NMOFman.Rnw:1830-1843
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
### code chunk number 92: NMOFman.Rnw:1849-1855
###################################################
sum(x0)
sum(repair(x0, data))
sum(repair2(x0, data))

colSums(repair( cbind(x0, x0, x0), data))
colSums(repair2(cbind(x0, x0, x0), data))


###################################################
### code chunk number 93: NMOFman.Rnw:1861-1863
###################################################
summary(repair (x0, data)-x0)
summary(repair2(x0, data)-x0)


###################################################
### code chunk number 94: NMOFman.Rnw:1868-1879
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
### code chunk number 95: NMOFman.Rnw:1887-1893
###################################################
x0[1L] <- 0.30
penalty(x0, data)
penalty(cbind(x0, x0, x0), data)
x0[1L] <- 0
penalty(x0, data)
penalty(cbind(x0, x0, x0), data)


###################################################
### code chunk number 96: NMOFman.Rnw:1898-1911
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
### code chunk number 97: NMOFman.Rnw:1917-1924
###################################################
system.time(sol <- DEopt(OF = OF,algo = algo,data = data))
16 * 100 * sqrt(sol$OFvalue)   ## solution quality

## check constraints
all(all.equal(sum(sol$xbest), 1),  ## budget constraint
    sol$xbest <= data$max,         ## holding size constraints
    sol$xbest >= data$min)


###################################################
### code chunk number 98: NMOFman.Rnw:1934-1944
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
### code chunk number 99: NMOFman.Rnw:1955-1968
###################################################
algo$printDetail <- FALSE
restartsDE <- restartOpt(fun = DEopt,      ### what function
                         n = 20L,          ### how many restarts
                         OF = OF,
                         algo = algo,
                         data = data,
                         method = "snow",  ### using package snow
                         cl = 2)           ### 2 cores

## extract best solution
OFvaluesDE <- sapply(restartsDE, `[[`, "OFvalue")
OFvaluesDE <- 16 * 100 * sqrt(OFvaluesDE)
weightsDE  <- sapply(restartsDE, `[[`, "xbest")


###################################################
### code chunk number 100: NMOFman.Rnw:1973-1978
###################################################
par(bty = "n", las = 1, mar = c(3, 4, 0, 0),
    ps = 8, tck = 0.001)
plot(sort(OFvaluesDE), (seq_len(length(OFvaluesDE))) / length(OFvaluesDE),
     type = "S", ylim = c(0, 1), xlab = "", ylab = "")
mtext("OF value",  side = 1, line = 2)


###################################################
### code chunk number 101: NMOFman.Rnw:1982-1988
###################################################
par(bty = "n", las = 1, mar = c(3, 4, 0, 0),
    ps = 8, tck = 0.001)
boxplot(t(weightsDE),
        outline = FALSE, boxwex = 0.4, ylim = c(-0.06,0.06))
mtext("assets",  side = 1, line = 2)
mtext("weights", side = 2, line = 1.3, las = 1, padj = -4)


###################################################
### code chunk number 102: NMOFman.Rnw:2001-2016
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
### code chunk number 103: NMOFman.Rnw:2021-2035
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
### code chunk number 104: NMOFman.Rnw:2040-2050
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
### code chunk number 105: NMOFman.Rnw:2060-2067
###################################################
weightsDE <- sapply(restartsDE, `[[`, "xbest")
par(bty = "n", las = 1, mar = c(3, 4, 0, 0),
ps = 8, tck = 0.001)
boxplot(t(weightsDE),
outline = FALSE, boxwex = 0.4, ylim = c(-0.06, 0.06))
mtext("assets",  side = 1, line = 2)
mtext("weights", side = 2, line = 1.3, las = 1, padj = -4)


###################################################
### code chunk number 106: NMOFman.Rnw:2074-2091
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
### code chunk number 107: NMOFman.Rnw:2099-2106
###################################################
changeV <- function(x, data) {
  myFun <- function(x) x - (sum(x))/data$na
  if (is.null(dim(x)[2L]))
  myFun(x) else apply(x, 2L, myFun)
}
sum(changeV(x0, data))
colSums(changeV(cbind(x0, x0, x0), data))


###################################################
### code chunk number 108: NMOFman.Rnw:2111-2115
###################################################
initP <- data$min + diag(data$max - data$min) %*%
array(runif(length(data$min) * algo$nP),
dim = c(length(data$min),  algo$nP))
colSums(initP <- repair(initP,data))[1:10] ## check


###################################################
### code chunk number 109: NMOFman.Rnw:2121-2127
###################################################
algo$changeV <- changeV        ## function to adjust velocity
algo$initP <- initP            ## initial population
algo$repair <- NULL            ## not needed anymore

system.time(sol <- PSopt(OF = OF,algo = algo, data = data))
16 * 100 * sqrt(sol$OFvalue)   ## solution quality


###################################################
### code chunk number 110: NMOFman.Rnw:2132-2135
###################################################
all(all.equal(sum(sol$xbest), 1), ## budget constraint
sol$xbest <= data$max,
sol$xbest >= data$min)


###################################################
### code chunk number 111: NMOFman.Rnw:2138-2141
###################################################
algo$loopOF <- FALSE; algo$loopPen <- FALSE
algo$loopRepair <- FALSE; algo$loopChangeV <- FALSE
system.time(sol <- PSopt(OF = OF, algo = algo, data = data))


###################################################
### code chunk number 112: NMOFman.Rnw:2146-2162
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
### code chunk number 113: NMOFman.Rnw:2173-2196
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
### code chunk number 114: NMOFman.Rnw:2202-2215
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
### code chunk number 115: NMOFman.Rnw:2221-2243
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
### code chunk number 116: NMOFman.Rnw:2293-2300
###################################################
size <- 20L
x <- logical(size)
x[runif(size) > 0.5] <- TRUE

## store information
Data <- list()
Data$size <- size


###################################################
### code chunk number 117: NMOFman.Rnw:2304-2312
###################################################
compareLogicals <- function(x, y, ...) {
  argsL <- list(...)
  if (!("sep" %in% names(argsL))) argsL$sep <- ""
  do.call("cat",
  c(list("\n",as.integer(x), "\n", as.integer(y), "\n",
  ifelse(x == y, " ", "^"), "\n"), argsL)
  )
}


###################################################
### code chunk number 118: NMOFman.Rnw:2316-2322
###################################################
## there should be no difference
compareLogicals(x, x)

## change the second element
z <- x; z[2L] <- !z[2L]
compareLogicals(x, z)


###################################################
### code chunk number 119: NMOFman.Rnw:2331-2338
###################################################
Data$n <- 5L  ## how many elements to change
neighbour <- function(x, Data) {
    ii <- sample.int(Data$size, Data$n)
    x[ii] <- !x[ii]
    x
}
compareLogicals(x, neighbour(x, Data))


###################################################
### code chunk number 120: NMOFman.Rnw:2348-2359
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
### code chunk number 121: NMOFman.Rnw:2364-2384
###################################################

## TA should come close to the minimum
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
### code chunk number 122: NMOFman.Rnw:2436-2447
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
### code chunk number 123: NMOFman.Rnw:2451-2471
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
### code chunk number 124: NMOFman.Rnw:2477-2492
###################################################
cfList <- list(cf1,cf2,cf3,cf4,cf5,cf6,cf7,cf8,cf9,cf10,
cf11,cf12,cf13,cf14,cf15,cf16,cf17,cf18,cf19,cf20)
tmList <- list(tm1,tm2,tm3,tm4,tm5,tm6,tm7,tm8,tm9,tm10,
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
### code chunk number 125: NMOFman.Rnw:2499-2503
###################################################
betaTRUE <- c(5,-2,1,10,1,3)
yM <- NSS(betaTRUE,tm)
diFa <- 1 / ( (1 + yM/100)^tm )
bM <- diFa %*% cfMatrix


###################################################
### code chunk number 126: NMOFman.Rnw:2507-2511
###################################################
data <- list(bM = bM, tm = tm, cfMatrix = cfMatrix, model = NSS,
ww = 1,
min = c( 0,-15,-30,-30,0  ,2.5),
max = c(15, 30, 30, 30,2.5,5  ))


###################################################
### code chunk number 127: NMOFman.Rnw:2521-2531
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
### code chunk number 128: NMOFman.Rnw:2535-2550
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
### code chunk number 129: NMOFman.Rnw:2555-2571
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
### code chunk number 130: NMOFman.Rnw:2578-2580
###################################################
max( abs(data$model(sol$xbest, tm) - data$model(betaTRUE, tm)))
sol$OFvalue


###################################################
### code chunk number 131: NMOFman.Rnw:2585-2602
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
### code chunk number 132: NMOFman.Rnw:2607-2610
###################################################
diFa <- 1 / ((1 + NSS(sol$xbest,tm)/100)^tm)
b <- diFa %*% cfMatrix
b - bM


###################################################
### code chunk number 133: NMOFman.Rnw:2615-2619
###################################################
par(ps = 8, bty = "n", las = 1, tck = 0.01,
mgp = c(3, 0.5, 0), mar = c(4, 4, 1, 1))
plot(tm, NSS(sol$xbest,tm) - NSS(betaTRUE,tm),
xlab = "maturities in years", ylab = "yield error in %")


###################################################
### code chunk number 134: NMOFman.Rnw:2626-2630
###################################################
par(ps = 8, bty = "n", las = 1, tck = 0.01,
mgp = c(3, 0.5, 0), mar = c(4, 4, 1, 1))
plot(as.numeric(unlist(lapply(tmList, max))), as.vector(b - bM),
xlab = "maturities in years", ylab = "price error in %")


###################################################
### code chunk number 135: NMOFman.Rnw:2643-2662
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
### code chunk number 136: NMOFman.Rnw:2667-2693
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
### code chunk number 137: NMOFman.Rnw:2703-2708
###################################################
betaTRUE <- c(5,-2,1,10,1,3)
yM <- NSS(betaTRUE, tm)
diFa <- 1 / ( (1 + yM/100)^tm )
bM <- diFa %*% cfMatrix
rM <- apply(rbind(-bM, cfMatrix), 2, compYield, c(0, tm))


###################################################
### code chunk number 138: NMOFman.Rnw:2713-2733
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
### code chunk number 139: NMOFman.Rnw:2736-2739
###################################################
sol <- DEopt(OF = OF3, algo = algo, data = data)
max(abs(data$model(sol$xbest,tm) - data$model(betaTRUE,tm)))
sol$OFvalue


###################################################
### code chunk number 140: NMOFman.Rnw:2744-2752
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
### code chunk number 141: NMOFman.Rnw:2755-2764
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
### code chunk number 142: NMOFman.Rnw:2769-2771
###################################################
betaTRUE
round(sol$xbest,3)


###################################################
### code chunk number 143: NMOFman.Rnw:2798-2827
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
### code chunk number 144: NMOFman.Rnw:2835-2839
###################################################
x0 <- logical(data$p)
temp <- sample.int(data$maxk, 1L)
temp <- sample.int(data$p, temp)
x0[temp] <- TRUE


###################################################
### code chunk number 145: NMOFman.Rnw:2850-2861
###################################################
require("rbenchmark")
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
### code chunk number 146: NMOFman.Rnw:2876-2882
###################################################
OF <- function(x, data) {
    q <- qr(data$X[ ,x])
    e <- qr.resid(q, data$y)
    log(crossprod(e)/data$n) + sum(x) * data$lognn
}
OF(x0, data)


###################################################
### code chunk number 147: NMOFman.Rnw:2889-2898
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
### code chunk number 148: NMOFman.Rnw:2904-2912
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
### code chunk number 149: NMOFman.Rnw:2918-2921
###################################################
sol1$OFvalue
which(sol1$xbest)  ### the selected regressors
rD$K               ### the true regressors


###################################################
### code chunk number 150: NMOFman.Rnw:2928-2932
###################################################
xtrue <- logical(data$p)
xtrue[rD$K] <- TRUE
OF(sol1$xbest, data)
OF(xtrue, data)


###################################################
### code chunk number 151: NMOFman.Rnw:2941-2950
###################################################
restarts <- 50L
algo$printDetail <- FALSE
res <- restartOpt(TAopt, n = restarts,
                  OF = OF, algo = algo, data = data,
                  method = "snow", cl = 2)
par(bty = "n", las = 1,mar = c(3,4,0,0),
    ps = 8, tck = 0.001, mgp = c(3, 0.5, 0))
plot(ecdf(sapply(res, `[[`, "OFvalue")),  ### extract solution quality
     cex = 0.4, main = "", ylab = "", xlab = "")


###################################################
### code chunk number 152: NMOFman.Rnw:2956-2962
###################################################
xbestAll <- sapply(res, `[[`, "xbest")    ### extract all solutions
inclReg  <- which(rowSums(xbestAll) > 0L) ### get included regressors
inclReg  <- sort(union(rD$K, inclReg))
data.frame(regressor = inclReg,
           times_included = paste(rowSums(xbestAll)[inclReg], "/", restarts, sep = ""),
           true_regressor = inclReg %in% rD$K)


###################################################
### code chunk number 153: NMOFman.Rnw:3019-3020
###################################################
require("adagio")


###################################################
### code chunk number 154: NMOFman.Rnw:3024-3056
###################################################
## Example 1
myList <- list()
myList$p <- c(15, 100, 90, 60, 40, 15, 10,  1)
myList$w <- c( 2,  20, 20, 30, 40, 30, 60, 10)
myList$n <- length(myList$p)
myList$cap <- 102

OF <- function(x, myList)
-sum(x*myList$p)

neighbour <- function(x, myList) {
  xn <- x
  p <- sample.int(myList$n, size = 1L)
  xn[p] <- !xn[p]
  if (sum(myList$w*xn) > myList$cap)
  x else xn
}

algo <- list(x0 = logical(myList$n),  ## a random start
printDetail = 100,
printBar = FALSE,
q = 0.99,
neighbour = neighbour, nS = 50)
system.time(sol <- TAopt(OF, algo = algo, myList))
OF(sol$xbest,myList)

(is <- knapsack(myList$w, myList$p, myList$cap))
xHWB <- logical(myList$n)
xHWB[c(1:4,6)] <- TRUE
OF(xHWB, myList)
## [1] 1 2 3 4 6 , capacity 102 and total profit 280



###################################################
### code chunk number 155: NMOFman.Rnw:3060-3076
###################################################
## Example 2
myList$p <- c(70, 20, 39, 37, 7, 5, 10)
myList$w <- c(31, 10, 20, 19, 4, 3,  6)
myList$cap <- 50
myList$n <- length(myList$p)

## adagio solution
(is <- knapsack(myList$w, myList$p, myList$cap))
## [1] 1 4 , capacity 50 and total profit 107
algo <- list(x0 = logical(myList$n),  ## a random start
printDetail = 100,
printBar = FALSE,
q = 0.99,
neighbour = neighbour, nS = 50)
system.time(sol <- TAopt(OF, algo = algo, myList))
OF(sol$xbest, myList)


###################################################
### code chunk number 156: NMOFman.Rnw:3085-3142
###################################################
set.seed(8232)
X <- runif(100)

## Find subset that sums up close to 2.0 !
i <- sort(c(84,54,11,53,88,12,26,45,25,62,96,23,78,77,66,1))
sum(X[i])
##  2.000451

x_HWB <- logical(100L)
x_HWB[i] <- TRUE
sum(X[x_HWB]) ## check

neighbour <- function(x, X) {
  p <- sample.int(100, size = 1L)
  x[p] <- !x[p]
  x
}

OF <- function(x, X)
abs(sum(X[x]) - 2)
OF(x_HWB, X)

algo <- list(x0=runif(100)>0.5,printDetail=FALSE,
printBar = FALSE,q=0.2,
neighbour=neighbour,nS = 5000)
system.time(sol <- TAopt(OF, algo = algo, X = X))
OF(sol$xbest,X) < OF(x_HWB,X)

manySols <- restartOpt(TAopt, n = 3, OF = OF, algo = algo, X = X)
plot(ecdf(sapply(manySols, `[[`, "OFvalue")), xlim = c(0, 5e-4))
abline(v = OF(x_HWB, X))


##
xrand <- runif(100) > 0.5
x0 <- list(x = xrand,
sumX = sum(X[xrand]))
x_HWB <- list(x = x_HWB,
sumX = sum(X[i]))

OF <- function(x, X)
abs(x$sumX - 2)
OF(x0, X)
OF(x_HWB, X)

neighbour <- function(x, X) {
  p <- sample.int(100, size = 1L)
  x$x[p] <- !x$x[p]
  x$sumX <- x$sumX + X[p] * (x$x[p]*2-1)
  x
}
neighbour(x_HWB, X)

algo <- list(x0 = x0, printDetail=FALSE,
printBar = FALSE,q=0.2,
neighbour=neighbour,nS = 5000)
system.time(sol <- TAopt(OF, algo = algo, X = X))


###################################################
### code chunk number 157: NMOFman.Rnw:3152-3240
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
### code chunk number 158: NMOFman.Rnw:3262-3264
###################################################
options(digits = 7)
require("Rsolnp")


###################################################
### code chunk number 159: NMOFman.Rnw:3268-3340
###################################################
## run the benchmark case
bmResults <- benchmark(id = "RachevRatio")
cat(attr(bmResults, "description"))

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
### code chunk number 160: NMOFman.Rnw:3353-3355 (eval = FALSE)
###################################################
## install.packages("NMOF") ## CRAN
## install.packages("NMOF", repos = "http://R-Forge.R-project.org")


###################################################
### code chunk number 161: NMOFman.Rnw:3363-3365 (eval = FALSE)
###################################################
## require("NMOF")
## showExample("exampleOF.R")


###################################################
### code chunk number 162: NMOFman.Rnw:3391-3392
###################################################
toLatex(sessionInfo())


