### R code from vignette source 'NMOFdist.Rnw'

###################################################
### code chunk number 1: NMOFdist.Rnw:72-74
###################################################
## version 2011-12-11
options(continue = " ", digits = 3, width = 70)


###################################################
### code chunk number 2: NMOFdist.Rnw:94-96 (eval = FALSE)
###################################################
## install.packages("NMOF") ## CRAN
## install.packages("NMOF", repos = "http://R-Forge.R-project.org")


###################################################
### code chunk number 3: NMOFdist.Rnw:99-102
###################################################
require("NMOF")
set.seed(1112233344)
nC <- 2L ## the number of cores to be used


###################################################
### code chunk number 4: NMOFdist.Rnw:105-117
###################################################
require("RUnit")
require("rbenchmark")
require("snow")

## 'multicore' is not available on Windows
if (!require("multicore")) {
    print("package 'multicore' not available")
    mclapply <- lapply  ## use lapply
}
## packages for parallel random numbers
require("rlecuyer")
require("rsprng")


###################################################
### code chunk number 5: NMOFdist.Rnw:123-125 (eval = FALSE)
###################################################
## whereToLook <- system.file("NMOFex/NMOFdist.R", package = "NMOF")
## file.show(whereToLook, title = "NMOF examples")


###################################################
### code chunk number 6: NMOFdist.Rnw:136-155
###################################################
if (!require("multicore"))
    print("... multicore not available")

testFun <- function(ignore, delay) {
    Sys.sleep(delay)
    1
}
delay <- 0.01     ## running time of function
n <- 8            ## how many calls per lapply
repl <- 10        ## how many restarts
sq <- seq_len(n)

cl <- makeCluster(c(rep("localhost", nC)), type = "SOCK")
benchmark(lapply(sq, testFun, delay),
          mclapply(sq, testFun, delay),
          clusterApply(cl, sq, testFun, delay),
          columns = c("test", "elapsed", "relative"),
          order = "relative", replications = repl)
stopCluster(cl)


###################################################
### code chunk number 7: NMOFdist.Rnw:160-185
###################################################
testFun <- function(x) {
    Sys.sleep(0.1)
    cos(1/x^2)
}
with_loop <- expression(
    sol1 <- bracketing(testFun,
                       interval = c(0.3, 0.9),
                       n = 100L)
    )
with_multicore <- expression(
    sol2 <- bracketing(testFun,
                       interval = c(0.3, 0.9),
                       n = 100L,
                       method = "multicore")
    )
with_snow  <- expression(
    sol3 <- bracketing(testFun,
                       interval = c(0.3, 0.9),
                       n = 100L, method = "snow", cl = nC)
    )
benchmark(with_loop, with_multicore, with_snow,
          columns = c("test", "elapsed", "relative"),
          order = "relative", replications = 1)
checkEquals(sol1, sol2)
checkEquals(sol1, sol3)


###################################################
### code chunk number 8: NMOFdist.Rnw:193-214
###################################################
OF <- function(x, y) {
    Sys.sleep(0.005)
    sum(x != y)
}
size <- 20L            ## the length of the string
y <- runif(size) > 0.5 ## the true solution
with_loop <- list(nB = size, nP = 20L, nG = 100L, prob = 0.002,
                  printBar = FALSE, printDetail = FALSE,
                  methodOF = "loop")
with_snow <- list(nB = size, nP = 20L, nG = 100L, prob = 0.002,
                  printBar = FALSE, printDetail = FALSE,
                  methodOF = "snow", cl = nC)
with_multicore <- list(nB = size, nP = 20L, nG = 100L, prob = 0.002,
                       printBar = FALSE, printDetail = FALSE,
                       methodOF = "multicore")

benchmark(GAopt(OF, algo = with_loop, y = y),
          GAopt(OF, algo = with_snow, y = y),
          GAopt(OF, algo = with_multicore, y = y),
          columns = c("test", "elapsed", "relative"),
          order = "relative", replications = 1)


###################################################
### code chunk number 9: NMOFdist.Rnw:221-223
###################################################
with_multicore$mc.control <- list(mc.cores = 1L)
system.time(GAopt(OF, algo = with_multicore, y = y))


###################################################
### code chunk number 10: NMOFdist.Rnw:227-243
###################################################
OF <- function(x, y) {
    Sys.sleep(0.01)
    sum(x != y)
}
size <- 10L; y <- runif(size) > 0.5
algo <- list(nB = size, nP = 20L, nG = 100L, prob = 0.002,
             printBar = FALSE, methodOF = "loop")
t1 <- system.time(sol <- GAopt(OF, algo = algo, y = y))
checkEquals(sol$xbest, y)
checkEquals(sol$OFvalue, 0)

algo <- list(nB = size, nP = 20L, nG = 100L, prob = 0.002,
             printBar = FALSE, methodOF = "snow", cl = nC)
t2 <- system.time(sol <- GAopt(OF, algo = algo, y = y))
checkEquals(sol$xbest,y)
checkEquals(sol$OFvalue, 0)


###################################################
### code chunk number 11: NMOFdist.Rnw:246-248
###################################################
t1[[3L]]/t2[[3L]]
##checkTrue(t1[[3L]] > t2[[3L]])


###################################################
### code chunk number 12: NMOFdist.Rnw:252-271
###################################################
OF <- function(x, y, k) {
    Sys.sleep(0.01)
    sum(x != y)+k
}
size <- 10L; y <- runif(size) > 0.5; k <- 10
algo <- list(nB = size, nP = 20L, nG = 100L, prob = 0.002,
             printBar = FALSE, printDetail = FALSE,
             methodOF = "loop")
t1 <- system.time(sol <- GAopt(OF, algo = algo, y = y, k = k))
checkEquals(sol$xbest, y)
checkEquals(sol$OFvalue, k)

algo <- list(nB = size, nP = 20L, nG = 100L, prob = 0.002,
             printBar = FALSE, printDetail = FALSE,
             methodOF = "snow", cl = nC)
t2 <- system.time(sol <- GAopt(OF, algo = algo, y = y, k = k))
checkEquals(sol$xbest,y)
##checkTrue(t1[[3L]]>t2[[3L]])
checkEquals(sol$OFvalue, k)


###################################################
### code chunk number 13: NMOFdist.Rnw:276-306
###################################################
testFun  <- function(x) {
    Sys.sleep(0.1)
    x[1L] + x[2L]^2
}
lower <- 1:2; upper <- 5; n <- 10
with_loop <- expression(
    sol1 <- gridSearch(fun = testFun,
                       lower = lower, upper = upper,
                       n = n, printDetail = FALSE)
                )
with_multicore <- expression(
    sol2 <- gridSearch(fun = testFun,
                       lower = lower, upper = upper,
                       n = n, printDetail = FALSE,
                       method = "multicore")
    )
with_snow <- expression(
    sol3 <- gridSearch(fun = testFun,
                       lower = lower, upper = upper,
                       n = n, printDetail = FALSE,
                       method = "snow",
                       cl = nC)
    )

benchmark(with_loop, with_multicore, with_snow,
          columns = c("test", "elapsed", "relative"),
          order = "relative", replications = 1)
checkEquals(sol1, sol2)
checkEquals(sol1, sol3)
checkEquals(sol3$minlevels, 1:2)


###################################################
### code chunk number 14: NMOFdist.Rnw:311-330
###################################################
testFun  <- function(x, k) {
    Sys.sleep(0.1)
    x[1L] + x[2L]^2 + k
}
lower <- 1:2; upper <- 5; n <- 5; k <- 1
sol1 <- gridSearch(fun = testFun, k = k,
                   lower = lower, upper = upper,
                   n = n, printDetail = FALSE)
sol2 <- gridSearch(fun = testFun,k = k,
                   lower = lower, upper = upper,
                   n = n, printDetail = FALSE,
                   method = "multicore")
sol3 <- gridSearch(fun = testFun,k = k,
                   lower = lower, upper = upper,
                   n = n, printDetail = FALSE,
                   method = "snow", cl = nC)
checkEquals(sol1, sol2)
checkEquals(sol1, sol3)
checkEquals(sol3$minlevels, 1:2)


###################################################
### code chunk number 15: NMOFdist.Rnw:336-355
###################################################
testFun  <- function(x) {
    Sys.sleep(0.1)
    x[1L] + x[2L] + runif(1)
}
lower <- 1:2; upper <- 5; n <- 3
set.seed(5)
sol2 <- gridSearch(fun = testFun,
                   lower = lower, upper = upper,
                   n = n, printDetail = FALSE,
                   method = "multicore",
                   mc.control = list(mc.set.seed = FALSE))
temp <- sol2$values
set.seed(5)
sol2 <- gridSearch(fun = testFun,
                   lower = lower, upper = upper,
                   n = n, printDetail = FALSE,
                   method = "multicore",
                   mc.control = list(mc.set.seed = FALSE))
checkEquals(sol2$values, temp)


###################################################
### code chunk number 16: NMOFdist.Rnw:360-378
###################################################
cl <- makeCluster(c(rep("localhost", nC)), type = "SOCK")
clusterSetupSPRNG(cl, seed = rep.int(12345, nC))
sol3 <- gridSearch(fun = testFun,
                   lower = lower, upper = upper,
                   n = n, printDetail = FALSE,
                   method = "snow", cl = cl)
stopCluster(cl)
temp <- sol3$values

## ... and again
cl <- makeCluster(c(rep("localhost", nC)), type = "SOCK")
clusterSetupSPRNG(cl, seed = rep.int(12345, nC))
sol3 <- gridSearch(fun = testFun,
                   lower = lower, upper = upper,
                   n = n, printDetail = FALSE,
                   method = "snow", cl = cl)
stopCluster(cl)
checkEquals(sol3$values, temp)


###################################################
### code chunk number 17: NMOFdist.Rnw:381-397
###################################################
cl <- makeCluster(c(rep("localhost", nC)), type = "SOCK")
clusterSetupRNGstream (cl, seed = rep.int(12345, nC))
sol3 <- gridSearch(fun = testFun, lower = lower, upper = upper,
                   n = n, printDetail = FALSE,
                   method = "snow", cl = cl)
stopCluster(cl)
temp <- sol3$values

## ... and again
cl <- makeCluster(c(rep("localhost", nC)), type = "SOCK")
clusterSetupRNGstream (cl, seed = rep.int(12345, nC))
sol3 <- gridSearch(fun = testFun, lower = lower, upper = upper,
                   n = n, printDetail = FALSE,
                   method = "snow", cl = cl)
stopCluster(cl)
checkEquals(sol3$values, temp)


###################################################
### code chunk number 18: NMOFdist.Rnw:404-416
###################################################
xTRUE <- runif(5L)
data <- list(xTRUE = xTRUE,  ## the TRUE solution
             step = 0.02     ## step size for neighbourhood
             )
OF <- function(x, data)
    max(abs(x - data$xTRUE))
neighbour <- function(x, data)
    x + runif(length(data$xTRUE))*data$step - data$step/2
x0 <- runif(5L)              ## a random starting solution
algo <- list(q = 0.05, nS = 200L, nT = 10L,
             neighbour = neighbour, x0 = x0,
             printBar = FALSE, printDetail = FALSE)


###################################################
### code chunk number 19: NMOFdist.Rnw:419-439
###################################################
with_loop <- expression(
    sols1 <- restartOpt(fun = TAopt, n = 100L,
                        OF = OF, algo = algo, data = data)
    )
with_multicore <- expression(
    sols2 <- restartOpt(fun = TAopt, n = 100L,
                        OF = OF, algo = algo, data = data,
                        method = "multicore")
                )
with_snow <- expression(
    sols3 <- restartOpt(fun = TAopt, n = 100L,
                        OF = OF, algo = algo, data = data,
                        method = "snow", cl = nC)
    )
benchmark(with_loop, with_multicore, with_snow,
          columns = c("test", "elapsed", "relative"),
          order = "relative", replications = 1)
checkEquals(length(sols1), 100L)
checkEquals(length(sols2), 100L)
checkEquals(length(sols3), 100L)


###################################################
### code chunk number 20: NMOFdist.Rnw:464-465
###################################################
toLatex(sessionInfo())


