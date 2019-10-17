### R code from "Numerical Methods and Optimization in Finance"

###################################################
### code chunk number 1: chapter-settings
###################################################
library("NMOF")
library("PMwR")
library("parallel")
library("quadprog")
library("rbenchmark")
library("zoo")
options(continue = "  ",
        digits = 3,
        width = 55,
        str = strOptions(strict.width = "cut"),
        useFancyQuotes = FALSE,
        warn = 2)


###################################################
### code chunk number 2: one
###################################################
one <- function(...)
    1


###################################################
### code chunk number 3
###################################################
one()


###################################################
### code chunk number 4: one-loop
###################################################
runs <- 1000
ones <- numeric(runs)
for (i in seq_len(runs))
    ones[i] <- one()


###################################################
### code chunk number 5
###################################################
ones <- lapply(seq_len(runs), one)


###################################################
### code chunk number 6: one-wait
###################################################
one <- function(...) {
    Sys.sleep(1)  ## wait one second
    1
}


###################################################
### code chunk number 7: runs
###################################################
runs <- 4
system.time(
    for (i in seq_len(runs))
        one())


###################################################
### code chunk number 8
###################################################
system.time(
    lapply(seq_len(runs), one))


###################################################
### code chunk number 9: one-parallel
###################################################
cl <- makeCluster(4)  ## four cores
system.time(parLapply(cl, seq_len(runs), one))
system.time(clusterApply(cl, seq_len(runs), one))
stopCluster(cl)


###################################################
### code chunk number 10: sum-xy
###################################################
sum_xy <- function(x, y)
    x + y


###################################################
### code chunk number 11
###################################################
df <- expand.grid(x = 1:2, y = 5:6)
df


###################################################
### code chunk number 12: args-do-call
###################################################
args <- list(x = 1, y = 5)
do.call("sum_xy", args)


###################################################
### code chunk number 13: pack-df
###################################################
data <- vector("list", length = nrow(df))
for (i in seq_len(nrow(df)))
    data[[i]] <- list(x = df$x[i], y = df$y[i])


###################################################
### code chunk number 14: eval-lapply
###################################################
lapply(data, function(z) do.call(sum_xy, z))


###################################################
### code chunk number 15: export-fun
###################################################
cl <- makeCluster(4)
clusterExport(cl, "sum_xy")
parLapply(cl, data, function(x) do.call(sum_xy, x))
clusterApply(cl, data, function(x) do.call(sum_xy, x))
stopCluster(cl)


###################################################
### code chunk number 16: eval-fun
###################################################
cl <- makeCluster(4)
ignore <- clusterEvalQ(cl,
                       sum_xy <- function(x, y)
                           x + y)
###
parLapply(cl, data, function(x) do.call(sum_xy, x))
stopCluster(cl)


###################################################
### code chunk number 17: export-y
###################################################
y.value <- 100
x.values <- as.list(1:4)
###
cl <- makeCluster(4)
clusterExport(cl, "y.value")
ignore <- clusterEvalQ(cl,
                       sum_xy <- function(x, y = y.value)
                                          x + y)
###
parLapply(cl, x.values, function(x) sum_xy(x, y.value))
stopCluster(cl)


###################################################
### code chunk number 18: backtests-parallel1
###################################################
## set up data, functions
prices <- 101:110
signal <- function(threshold) {
    if (Close() > threshold)
        1
    else
        0
}
threshold.values <- as.list(102:105)
###
###
## create cluster and distribute data, functions
cl <- makeCluster(4)
clusterExport(cl,
              c("signal", "prices"))
###
ignore <- clusterEvalQ (cl,
              library("PMwR"))
###
###
## run btest
parLapply(cl, threshold.values,
          function(x)
              btest(prices = prices,
                    signal = signal,
                    threshold = x))

stopCluster(cl)


###################################################
### code chunk number 19: backtests-parallel2
###################################################
## set up data, functions
prices <- list(prices1 = 101:110,
               prices2 = 201:210)
signal <- function() {
    if (Close() > 105)
        1
    else
        0
}
###
###
cl <- makeCluster(4)  ## create cluster
###
clusterExport(cl,     ## distribute data, functions
              c("signal"))
ignore <- clusterEvalQ (cl,
                        library("PMwR"))
###
###
parLapply(cl, prices, ## run btest
          function(x)
              btest(prices = x,
                    signal = signal))

stopCluster(cl)


###################################################
### code chunk number 20: eval-files (eval = FALSE)
###################################################
## ## chunkname: eval-files
## files <- dir("~/Backtesting",
##              pattern = "^.*\\.R",
##              full.names = TRUE)
## cl <- makeCluster(4)
## clusterApplyLB(cl = cl, files, source)
## stopCluster(cl)


###################################################
### code chunk number 21
###################################################
## vignette("parallel", package = "parallel")


