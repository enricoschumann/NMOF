### R code from "Numerical Methods and Optimization in Finance"

###################################################
### code chunk number 1: chapter-settings
###################################################
Sys.setenv(LANGUAGE = "en")
library("NMOF")
library("PMwR")
library("parallel")
library("quadprog")
library("zoo")
options(continue = "  ",
        digits = 3,
        width = 55,
        str = strOptions(strict.width = "cut"),
        useFancyQuotes = FALSE,
        warn = 2)
par_btest <- list(bty = "n",
                  las = 1,
                  mar = c(3, 3, 1, 1),
                  mgp = c(2, 0.5, 0),
                  tck = 0.01,
                  ps = 9)
col.market <- grey(0.3)
col.ew <- grey(0.1)


###################################################
### code chunk number 4: seed
###################################################
library("NMOF")
library("PMwR")
set.seed(2552551)


###################################################
### code chunk number 5: rnd-series
###################################################
randomPriceSeries <-
    function(length, vol = 0.01, demean = FALSE) {

    x <- cumprod(1 + rnorm(length - 1, sd = vol))
    scale1(c(1, x), centre = demean, level = 100)
}


###################################################
### code chunk number 6: rnd-series-fig (eval = FALSE)
###################################################
## ## chunkname: rnd-series-fig
## x <- randomPriceSeries(250)
## plot(x, type = "s", xlab = "", ylab = "x", )


###################################################
### code chunk number 7: par
###################################################
par_btest


###################################################
### code chunk number 8
###################################################
## do.call(par, par_btest)


###################################################
### code chunk number 9: random-series-fig
###################################################
do.call(par, par_btest)
x <- randomPriceSeries(250)
plot(x, type = "s", xlab = "", ylab = "x", )


###################################################
### code chunk number 10: ma-crossover
###################################################
m <- c(10, 30)
###
MA_crossover <- function(m, S) {
    m.fast <- MA(S, m[1], pad = NA)
    m.slow <- MA(S, m[2], pad = NA)

    crossover <- function() {
        if (m.fast[Time()] > m.slow[Time()])
            1
        else
            0
    }
    tail(btest(S, signal = crossover,
               b = 60, initial.cash = 100,
               convert.weights = TRUE)$wealth,
         n = 1)
}


###################################################
### code chunk number 11
###################################################
MA_crossover(m, x)


###################################################
### code chunk number 12
###################################################
x <- randomPriceSeries(250)
MA_crossover(m, x)


###################################################
### code chunk number 13: experiment1
###################################################
m <- c(10, 30)
initial_wealth <- 100
buyhold_profit <- final_profit <- numeric(100)

for (i in seq_along(final_profit)) {
    S <- randomPriceSeries(250)
    final_profit[i] <- MA_crossover(m, S) - initial_wealth
    buyhold_profit[i] <- S[length(S)] - S[1]
}


###################################################
### code chunk number 14
###################################################
summary(final_profit - buyhold_profit)


###################################################
### code chunk number 15: rnd-profits (eval = FALSE)
###################################################
## ## chunkname: rnd-profits
## plot(ecdf(final_profit - buyhold_profit),
##      main = "",
##      pch = NA,
##      verticals = TRUE,
##      xlab = paste("Final-profit difference",
##                   "(crossover minus buy-and-hold)"))
## abline(v = 0,
##        h = ecdf(final_profit - buyhold_profit)(0))


###################################################
### code chunk number 16: rnd-profits-corr (eval = FALSE)
###################################################
## ## chunkname: rnd-profits-corr
## library("MASS")
## do.call(par, par_btest)
## eqscplot(buyhold_profit, final_profit,
##          main = "", pch = 19, cex = 0.6)
## abline(v = 0, h = 0)


###################################################
### code chunk number 17: rnd-profits-fig
###################################################
do.call(par, par_btest)
plot(ecdf(final_profit - buyhold_profit),
     main = "",
     pch = NA,
     verticals = TRUE,
     xlab = paste("Final-profit difference",
                  "(crossover minus buy-and-hold)"))
abline(v = 0,
       h = ecdf(final_profit - buyhold_profit)(0))


###################################################
### code chunk number 18: rnd-profits-corr-fig
###################################################
do.call(par, par_btest)
library("MASS")
do.call(par, par_btest)
eqscplot(buyhold_profit, final_profit,
         main = "", pch = 19, cex = 0.6)
abline(v = 0, h = 0)


###################################################
### code chunk number 19: ma-crossover-opt
###################################################
MA_crossover_optimized <- function(S) {
    fast <- 1:20
    slow <- 21:60

    crossover <- function() {
        if (m.fast[Time()] > m.slow[Time()])
            1
        else
            0
    }
    best <- -10000
    best.par <- c(0, 0)
    for (f in fast) {
        m.fast <- MA(S, f, pad = NA)
        for (s in slow) {
            m.slow <- MA(S, s, pad = NA)
            res <- btest(S, crossover, b = 60,
                         initial.cash = 100,
                         convert.weights = TRUE)
            if (tail(res$wealth,1) > best) {
                best <- tail(res$wealth,1)
                best.par <- c(f, s)
                best.wealth <- res$wealth
            }
        }
    }
    attr(best, "wealth") <- best.wealth
    attr(best, "parameters") <- best.par
    best
}


###################################################
### code chunk number 20
###################################################
x <- randomPriceSeries(1000)
res <- MA_crossover_optimized(x)


###################################################
### code chunk number 21: grid-search
###################################################
res2 <- gridSearch(function(m, S)
                       -MA_crossover(m, S),
                   S = x,
                   levels = list(1:20, 21:60))


###################################################
### code chunk number 22
###################################################
x <- scale1(c(1, cumprod(1 + rnorm(999, sd = 0.01))),
            centre = TRUE, level = 100)
res <- MA_crossover_optimized(x)


###################################################
### code chunk number 23: optimized_crossover1
###################################################
do.call(par, par_btest)
X <- scale1(cbind(x, attr(res, "wealth")), level = 100)
plot(X[,1], type = "s", xlab = "", ylab = "",
     ylim = range(X, na.rm = TRUE))
lines(X[,2], type = "l", lwd = 1.5, col = grey(0.5))


###################################################
### code chunk number 24
###################################################
x <- scale1(c(1, cumprod(1 + rnorm(999, sd = 0.01))),
            centre = TRUE, level = 100)
res <- MA_crossover_optimized(x)


###################################################
### code chunk number 25: optimized_crossover2
###################################################
do.call(par, par_btest)
X <- scale1(cbind(x, attr(res, "wealth")), level = 100)
plot(X[,1], type = "s", xlab = "", ylab = "",
     ylim = range(X, na.rm = TRUE))
lines(X[,2], type = "l", lwd = 1.5, col = grey(0.5))


###################################################
### code chunk number 26
###################################################
x <- scale1(c(1, cumprod(1 + rnorm(999, sd = 0.01))),
            centre = TRUE, level = 100)
res <- MA_crossover_optimized(x)


###################################################
### code chunk number 27: optimized_crossover3
###################################################
do.call(par, par_btest)
X <- scale1(cbind(x, attr(res, "wealth")), level = 100)
plot(X[,1], type = "s", xlab = "", ylab = "",
     ylim = range(X, na.rm = TRUE))
lines(X[,2], type = "l", lwd = 1.5, col = grey(0.5))


###################################################
### code chunk number 28: experiment2
###################################################
buyhold_profit_opt <- final_profit_opt <- numeric(100)

for (i in seq_along(final_profit_opt)) {
    x <- randomPriceSeries(1000)
    res_gs <- gridSearch(function(m, S) -MA_crossover(m, S),
                         S = x,
                         levels = list(1:20, 21:60))
    final_profit_opt[i] <- -res_gs$minfun - initial_wealth
}


###################################################
### code chunk number 29
###################################################
summary(final_profit_opt)


###################################################
### code chunk number 30: optimized-profits
###################################################
do.call(par, par_btest)
plot(ecdf(final_profit_opt - buyhold_profit_opt),
     verticals = TRUE, main = "", pch = NA,
     xlab = paste("Final-profit difference",
                  "(crossover minus buy-and-hold)"))
abline(v = 0,
       h = ecdf(final_profit_opt - buyhold_profit_opt)(0))



###################################################
### code chunk number 34
###################################################
library("PMwR")


###################################################
### code chunk number 35
###################################################
prices <- 101:110
prices


###################################################
### code chunk number 36
###################################################
bt.results <- btest(prices, function() 1)
bt.results


###################################################
### code chunk number 37
###################################################
## fun <- function()
##     1
##
## bt.results <- btest(prices, fun)


###################################################
### code chunk number 38
###################################################
bt.results


###################################################
### code chunk number 39
###################################################
trade_details <- function(bt.results, prices)
      data.frame(price    = prices,
                 suggest  = bt.results$suggested.position,
                 position = unname(bt.results$position),
                 wealth   = bt.results$wealth,
                 cash     = bt.results$cash)

trade_details(bt.results, prices)


###################################################
### code chunk number 40
###################################################
bt.results0 <- btest(prices = prices,
                     signal = function() 1,
                     b = 0)
trade_details(bt.results0, prices)


###################################################
### code chunk number 41
###################################################
journal(bt.results)
journal(bt.results0)


###################################################
### code chunk number 42
###################################################
position(bt.results)


###################################################
### code chunk number 43
###################################################
signal <- function() {
    if (Close() <= 105)
        1
    else
        0
}

trade_details(btest(prices, signal), prices)


###################################################
### code chunk number 44
###################################################
signal <- function()
    Close() <= 105

trade_details(btest(prices, signal), prices)


###################################################
### code chunk number 45
###################################################
## btest(prices, function(Close = NA) 1)


###################################################
### code chunk number 46
###################################################
cat(try(btest(prices, function(Close = NA) 1)))


###################################################
### code chunk number 47
###################################################
## Close()


###################################################
### code chunk number 48
###################################################
## Close(Time():1)


###################################################
### code chunk number 49
###################################################
## Close(n = Time())


###################################################
### code chunk number 50
###################################################
## Close(n = 10)


###################################################
### code chunk number 51
###################################################
signal <- function() {
    k <- 3
    ma <- sum(Close(n = k))/k
    if (Close() > ma)
        1
    else
        0
}

trade_details(btest(prices = prices,
                    signal = signal,
                    b = 3),
              prices)


###################################################
### code chunk number 52
###################################################
signal <- function(k) {
    ma <- sum(Close(n = k))/k
    if (Close() > ma)
        1
    else
        0
}

trade_details(btest(prices, signal, b = 3, k = 3), prices)
trade_details(btest(prices, signal, b = 5, k = 5), prices)


###################################################
### code chunk number 53
###################################################
ma <- MA(prices, 3, pad = NA)
signal <- function(ma) {
    if (Close() > ma[Time()])
        1
    else
        0
}

trade_details(btest(prices, signal, b = 3, ma = ma), prices)


###################################################
### code chunk number 54
###################################################
MA_crossover


###################################################
### code chunk number 55
###################################################
trade_details(btest(prices, signal = function() 5),
              prices)


###################################################
### code chunk number 56
###################################################
signal <- function()
    if (Time() == 3L)
        1 else 0

trade_details(btest(prices, signal), prices)


###################################################
### code chunk number 57
###################################################
signal <- function()
    if (Time() == 3L)
        1 else Portfolio()

trade_details(btest(prices, signal), prices)


###################################################
### code chunk number 58
###################################################
signal <- function()
    0.5  ## invest 50% of wealth in asset


###################################################
### code chunk number 59
###################################################
bt <- btest(prices, signal,
            convert.weights = TRUE,
            initial.cash = 100)

trade_details(bt, prices)


###################################################
### code chunk number 60
###################################################
journal(bt)


###################################################
### code chunk number 61
###################################################
dont_if_small <- function() {
    diff <- SuggestedPortfolio(0) - Portfolio()
    abs(diff) > 5e-2
}

bt <- btest(prices,
            signal,
            convert.weights = TRUE,
            initial.cash = 100,
            do.rebalance = dont_if_small)

trade_details(bt, prices)


###################################################
### code chunk number 62
###################################################
bt <- btest(prices,
            signal,
            convert.weights = TRUE,
            initial.cash = 100,
            tol = 5e-2)

trade_details(bt, prices)


###################################################
### code chunk number 63: shiller-data
###################################################
library("NMOF")
data <- Shiller(dest.dir = "~/Downloads/Shiller")
str(data)


###################################################
### code chunk number 64: packages
###################################################
library("PMwR")
library("zoo")


###################################################
### code chunk number 65: extract-data
###################################################
timestamp <- data$Date
price <- scale1(zoo(data$Price, timestamp),
                level = 100)
CAPE <- zoo(data$CAPE, timestamp)


###################################################
### code chunk number 66: shiller1 (eval = FALSE)
###################################################
## ## chunkname: shiller1
## plot(price,
##      xlab = "", ylab = "S&P Composite",
##      xaxt = "n", yaxt = "n", log  = "y",
##      type = "l", lwd  = 0)
## abline(h = c(100, axTicks(2)),
##        lwd = 0.25, col = grey(0.7))
## axis(2, lwd = 0)
##
## t <- axis.Date(1, x = data$Date, lwd = 0)
## abline(v = t, lwd = 0.25, col = grey(0.7))
##
## lines(price, type = "l")
## abline(v = as.Date(c("1929-09-30",
##                      "1999-12-31")),
##        col = grey(0.5))


###################################################
### code chunk number 67: shiller2 (eval = FALSE)
###################################################
## ## chunkname: shiller2
## plot(CAPE, xlab = "",
##      ylab = "CAPE",
##      type = "l",
##      xaxt = "n", yaxt = "n", lwd = 0)
## abline(h = axTicks(2), lwd = 0.25, col = grey(0.7))
## t <- axis.Date(1, x = data$Date, lwd = 0)
## abline(v = t, lwd = 0.25, col = grey(0.7))
## axis(2, lwd = 0)
## lines(CAPE, type = "l")
## abline(v = as.Date(c("1929-09-30", "1999-12-31")),
##        col = grey(.5))


###################################################
### code chunk number 68: shiller1-fig
###################################################
do.call(par, par_btest)
plot(price,
     xlab = "", ylab = "S&P Composite",
     xaxt = "n", yaxt = "n", log  = "y",
     type = "l", lwd  = 0)
abline(h = c(100, axTicks(2)),
       lwd = 0.25, col = grey(0.7))
axis(2, lwd = 0)

t <- axis.Date(1, x = data$Date, lwd = 0)
abline(v = t, lwd = 0.25, col = grey(0.7))

lines(price, type = "l")
abline(v = as.Date(c("1929-09-30",
                     "1999-12-31")),
       col = grey(0.5))


###################################################
### code chunk number 69: shiller2-fig
###################################################
do.call(par, par_btest)
plot(CAPE, xlab = "",
     ylab = "CAPE",
     type = "l",
     xaxt = "n", yaxt = "n", lwd = 0)
abline(h = axTicks(2), lwd = 0.25, col = grey(0.7))
t <- axis.Date(1, x = data$Date, lwd = 0)
abline(v = t, lwd = 0.25, col = grey(0.7))
axis(2, lwd = 0)
lines(CAPE, type = "l")
abline(v = as.Date(c("1929-09-30", "1999-12-31")), col = grey(.5))


###################################################
### code chunk number 70
###################################################
## precompute quantile
CAPE_q90 <- numeric(length(CAPE))
for (t in seq_along(CAPE)) {
    CAPE_q90[t] <- quantile(coredata(CAPE)[1:t],
                            0.9, na.rm = TRUE)
}
plot(CAPE)
lines(zoo(CAPE_q90, timestamp))


###################################################
### code chunk number 71: avoid-high-valuation
###################################################
avoid_high_valuation <- function(CAPE) {
    Q <- quantile(CAPE[n = Time()], 0.9, na.rm = TRUE)
    if (CAPE[Time()] > Q)
        0
    else
        1
}
bt_avoid_high_valuation <-
    btest(price,
          signal = avoid_high_valuation,
          initial.cash = 100,
          convert.weights = TRUE,
          CAPE = CAPE,
          timestamp = timestamp,
          b = as.Date("1899-12-31"))


###################################################
### code chunk number 72
###################################################
as.NAVseries(bt_avoid_high_valuation)


###################################################
### code chunk number 73
###################################################
## summary(as.NAVseries(bt_avoid_high_valuation))


###################################################
### code chunk number 74
###################################################
xx <- capture.output(summary(as.NAVseries(bt_avoid_high_valuation)))
xx <- xx[1:(grep("^Monthly", xx) -1)]
cat(xx, sep = "\n")


###################################################
### code chunk number 75
###################################################
journal(bt_avoid_high_valuation)


###################################################
### code chunk number 76: merge-series
###################################################
merge_series <- function(..., series.names) {
    s <- list(...)
    if (missing(series.names)) {
        if (!is.null(ns <- names(s)))
            series.names <- ns
        else
            series.names <- seq_along(s)
    }
    to_zoo <- function(x) {
        if (inherits(x, "btest")) {
            as.zoo(as.NAVseries(x))
        } else if (inherits(x, "NAVseries")) {
            as.zoo(x)
        } else if (inherits(x, "zoo")) {
            x
        } else
            stop("only zoo, btest and NAVseries are supported")
    }
    s <- lapply(s, to_zoo)
    series <- do.call(merge, s)
    if (is.null(dim(series)))
        series <- as.matrix(series)
    series <- scale1(series, level = 100)
    colnames(series) <- series.names
    series
}


###################################################
### code chunk number 77: series-ratio
###################################################
series_ratio <- function(t1, t2) {
    if (missing(t2))
        scale1(t1[, 1]/t1[, 2], level = 100)
    else
        scale1(t1/t2, level = 100)
}


###################################################
### code chunk number 78: series
###################################################
series <- merge_series(
    "btest" = bt_avoid_high_valuation,
    "S&P" = price)


###################################################
### code chunk number 79: shiller3 (eval = FALSE)
###################################################
## ## chunkname: shiller3
## cols <- c(grey(0.5), "black")
## plot(series,
##      xlab = "",
##      ylab = "",
##      type = "l", log = "y",
##      xaxt = "n", yaxt = "n", lwd = 0, plot.type = "single")
## abline(h = axTicks(2), lwd = 0.25, col = grey(0.7))
## t <- axis.Date(1, x = data$Date, lwd = 0)
## abline(v = t, lwd = 0.25, col = grey(0.7))
## axis(2, lwd = 0)
## for (i in 1:2)
##     lines(series[, i], type = "l", col = cols[i])


###################################################
### code chunk number 80: shiller4 (eval = FALSE)
###################################################
## ## chunkname: shiller4
## plot(series_ratio(series),
##      xlab = "",
##      ylab = "",
##      type = "l", log = "y",
##      xaxt = "n", yaxt = "n", lwd = 0, plot.type = "single")
## abline(h = axTicks(2), lwd = 0.25, col = grey(0.7))
## t <- axis.Date(1, x = data$Date, lwd = 0)
## abline(v = t, lwd = 0.25, col = grey(0.7))
## axis(2, lwd = 0)
## lines(series_ratio(series), type = "l", col = cols[i])


###################################################
### code chunk number 81: shiller3-fig
###################################################
do.call(par, par_btest)
cols <- c(grey(0.5), "black")
plot(series,
     xlab = "",
     ylab = "",
     type = "l", log = "y",
     xaxt = "n", yaxt = "n", lwd = 0, plot.type = "single")
abline(h = axTicks(2), lwd = 0.25, col = grey(0.7))
t <- axis.Date(1, x = data$Date, lwd = 0)
abline(v = t, lwd = 0.25, col = grey(0.7))
axis(2, lwd = 0)
for (i in 1:2)
    lines(series[, i], type = "l", col = cols[i])


###################################################
### code chunk number 82: shiller4-fig
###################################################
do.call(par, par_btest)
plot(series_ratio(series),
     xlab = "",
     ylab = "",
     type = "l", log = "y",
     xaxt = "n", yaxt = "n", lwd = 0, plot.type = "single")
abline(h = axTicks(2), lwd = 0.25, col = grey(0.7))
t <- axis.Date(1, x = data$Date, lwd = 0)
abline(v = t, lwd = 0.25, col = grey(0.7))
axis(2, lwd = 0)
lines(series_ratio(series), type = "l", col = cols[i])


###################################################
### code chunk number 83: avoid-high-valuation-q
###################################################
avoid_high_valuation_q <- function(CAPE, q) {
    Q <- quantile(CAPE[n = Time()], q, na.rm = TRUE)
    if (CAPE[Time()] > Q)
        0
    else
        1
}


###################################################
### code chunk number 84
###################################################
q.values <- c(0.6, 0.7, 0.8,
              seq(0.90, 0.99, by = 0.01))


###################################################
### code chunk number 85: collect-args
###################################################
args <- vector("list", length(q.values))
names(args) <- as.character(q.values)

for (q in q.values)
    args[[as.character(q)]] <-
        list(coredata(price),
             signal = avoid_high_valuation_q,
             initial.cash = 100,
             convert.weights = TRUE,
             CAPE = CAPE,
             q = q,
             timestamp = timestamp,
             b = as.Date("1899-12-31"))


###################################################
### code chunk number 86: compare-methods
###################################################
## serial
variations_q_serial <- vector("list", length(q.values))
names(variations_q_serial) <- as.character(q.values)

for (q in q.values)
    variations_q_serial[[as.character(q)]] <-
        btest(coredata(price),
              signal = avoid_high_valuation_q,
              initial.cash = 100,
              convert.weights = TRUE,
              CAPE = CAPE,
              q = q,
              timestamp = timestamp,
              b = as.Date("1899-12-31"))

## parallel
library("parallel")


## parallel: socket cluster
cl <- makePSOCKcluster(rep("localhost", 2))
ignore <- clusterEvalQ(cl, require("PMwR"))
variations_q_parallel1 <-
    clusterApplyLB(cl, args,
                   function(x) do.call(btest, x))

names(variations_q_parallel1) <- as.character(q.values)
stopCluster(cl)


## parallel: fork cluster
cl <- makeForkCluster(nnodes = 4)
ignore <- clusterEvalQ(cl, require("PMwR"))
variations_q_parallel2 <- clusterApplyLB(cl, args,
                                         function(x) do.call(btest, x))
names(variations_q_parallel2) <- as.character(q.values)
stopCluster(cl)


###################################################
### code chunk number 87
###################################################
all.equal(variations_q_serial, variations_q_parallel1)
all.equal(variations_q_serial, variations_q_parallel2)


###################################################
### code chunk number 88
###################################################
## exporting the data to the nodes
cl <- makePSOCKcluster(rep("localhost", 4))
system.time({

    clusterExport(cl,
                  c("avoid_high_valuation_q",
                    "price",
                    "CAPE",
                    "timestamp"))

    clusterEvalQ (cl, {
        library("PMwR")
        library("zoo")
    })

    variations_q_parallel3 <-
        parLapply(cl, as.list(q.values),
                  function(q)
                      btest(coredata(price),
                          signal = avoid_high_valuation_q,
                          initial.cash = 100,
                          convert.weights = TRUE,
                          CAPE = CAPE,
                          q = q[[1]],
                          timestamp = timestamp,
                          b = as.Date("1899-12-31")))
    stopCluster(cl)
})

all.equal(variations_q_serial, variations_q_parallel3)


###################################################
### code chunk number 89: variations-q
###################################################
variations_q <-
    btest(coredata(price),
      signal = avoid_high_valuation_q,
      initial.cash = 100,
      convert.weights = TRUE,
      CAPE = CAPE,
      timestamp = timestamp,
      b = as.Date("1899-12-31"),
      variations = list(q = q.values),
      variations.settings =
          list(method = "multicore",
               cores = 4,
               label = as.character(q.values)))

all.equal(variations_q, variations_q_serial)


###################################################
### code chunk number 90: series-var
###################################################
series_var <- do.call(merge_series, variations_q)


###################################################
### code chunk number 91: shillerq-fig
###################################################
do.call(par, par_btest)
## par(mar = c(3, 3, 1, 3))
plot(series[, "S&P"],
     xlab = "",
     ylab = "",
     type = "l", log = "y",
     xaxt = "n", yaxt = "n", lwd = 0,
     plot.type = "single")
abline(h = axTicks(2), lwd = 0.25, col = grey(0.7))
t <- axis.Date(1, x = data$Date, lwd = 0)
abline(v = t, lwd = 0.25, col = grey(0.7))
axis(2, lwd = 0)

## par(xpd=TRUE)
for (i in seq_len(ncol(series_var))) {
    lines(series_var[, i], type = "l", col = grey(.5))
    ## text(tail(index(series_var), 1),
    ##      tail(coredata(series_var[, i]), 1),
    ##      round(100*q.values[i]),
    ##      pos = 4, cex = 0.55)
}
## par(xpd=FALSE)
lines(series[, "S&P"])


###################################################
### code chunk number 92: returns-total
###################################################
returns(window(series[, "S&P"],
               start = as.Date("1901-01-31"),
               end = as.Date("2017-12-31")),
        period = "itd")  ## inception to date
returns(window(series_var,
               start = as.Date("1901-01-31"),
               end = as.Date("2017-12-31")),
        period = "itd")


###################################################
### code chunk number 93
###################################################
returns(window(series[, "S&P"],
               start = as.Date("1920-12-31"),
               end = as.Date("1950-12-31")),
        period = "itd")
returns(window(series_var,
               start = as.Date("1920-12-31"),
               end = as.Date("1950-12-31")),
        period = "itd")


###################################################
### code chunk number 95: french-data
###################################################
library("NMOF")
library("PMwR")
library("zoo")
P <- French("~/Downloads/French",
            dataset = "49_Industry_Portfolios_daily_CSV.zip",
            weighting = "value",
            frequency = "daily",
            price.series = TRUE,
            na.rm = TRUE)


###################################################
### code chunk number 96: prepare-data
###################################################
START <- as.Date("1990-01-01")
END <- as.Date("2018-07-31")
###
## make zoo series
P <- zoo(P, as.Date(row.names(P)))
P <- window(P, start = START, end = END)
timestamp <- index(P)
P <- P[, colnames(P) != "Other"]
###
short <- colnames(P)
###
instrument <- colnames(P)
defs <- French("~/Downloads/French", "Siccodes49.zip")
data.frame(Abbr = colnames(P),
           Description = defs[match(colnames(P), defs$abbr), "industry"])


###################################################
### code chunk number 97: industries-series (eval = FALSE)
###################################################
## ## chunkname: industries-series
## plot(scale1(P, level = 100),
##      plot.type = "single",
##      log = "y",
##      col = grey.colors(ncol(P)),
##      xlab = "",
##      ylab = "")


###################################################
### code chunk number 98: industries-series-fig
###################################################
do.call(par, par_btest)
plot(scale1(P, level = 100),
     plot.type = "single",
     log = "y",
     col = grey.colors(ncol(P)),
     xlab = "",
     ylab = "")


###################################################
### code chunk number 99: fanplot (eval = FALSE)
###################################################
## ## chunkname: fanplot
## P100 <- scale1(P, level = 100) ## P must be 'zoo'
## P100 <- aggregate(P100,
##                   datetimeutils::end_of_month(index(P100)),
##                   tail, 1)
## nt <- nrow(P100)
## levels <- seq(0.01, 0.49, length.out = 10)
## greys  <- seq(0.9,  0.50, length.out = length(levels))
## ###
## ### start with an empty plot ...
## plot(index(P100), rep(100, nt), ylim = range(P100),
##      xlab = "", ylab = "",
##      lty = 0,
##      type = "l",
##      log = "y")
## ###
## ### ... and add polygons
## for (level in levels) {
##
##     l <- apply(P100, 1, quantile, level)
##     u <- apply(P100, 1, quantile, 1 - level)
##     col <- grey(greys[level == levels])
##     polygon(c(index(P100), rev(index(P100))),
##             c(l, rev(u)),
##             col = col,
##             border = NA)
## }


###################################################
### code chunk number 100: fanplot-fig
###################################################
do.call(par, par_btest)
P100 <- scale1(P, level = 100) ## P must be 'zoo'
P100 <- aggregate(P100,
                  datetimeutils::end_of_month(index(P100)),
                  tail, 1)
nt <- nrow(P100)
levels <- seq(0.01, 0.49, length.out = 10)
greys  <- seq(0.9,  0.50, length.out = length(levels))
###
### start with an empty plot ...
plot(index(P100), rep(100, nt), ylim = range(P100),
     xlab = "", ylab = "",
     lty = 0,
     type = "l",
     log = "y")
###
### ... and add polygons
for (level in levels) {

    l <- apply(P100, 1, quantile, level)
    u <- apply(P100, 1, quantile, 1 - level)
    col <- grey(greys[level == levels])
    polygon(c(index(P100), rev(index(P100))),
            c(l, rev(u)),
            col = col,
            border = NA)
}


###################################################
### code chunk number 101: fanplot-fun
###################################################
fan_plot <- function(P, n.levels = 10, lines = FALSE,
                     log = "y", ...) {

    P100 <- scale1(P, level = 100)
    P100 <- aggregate(P100, datetimeutils::end_of_month(index(P100)),
                      tail, 1)
    nt <- nrow(P100)
    levels <- seq(0.05, 0.49, length.out = n.levels)
    greys  <- seq(0.9,  0.50, length.out = length(levels))

    args <- list(...)

    if (!lines)
        plot(index(P100), rep(100, nt),
             ylim = range(P100),
             xlab = "", ylab = "",
             lty = 0,
             type = "n",
             log = log)

    for (level in levels) {

        l <- apply(P100, 1, quantile, level)
        u <- apply(P100, 1, quantile, 1 - level)
        col <- grey(greys[level == levels])
        polygon(c(index(P100), rev(index(P100))), c(l, rev(u)),
                col = col, border = NA)
    }
    invisible(NULL)
}


###################################################
### code chunk number 103: correlations
###################################################
C <- cor(returns(P, period = "month"))
cors <- C[lower.tri(C)]
summary(cors)


###################################################
### code chunk number 104: ind-corr (eval = FALSE)
###################################################
## ## chunkname: ind-corr
## do.call(par, par_btest)
## hist(cors, breaks = 20,
##      main = "Correlations of monthly returns",
##      xlab = "")
## abline(v = median(cors))


###################################################
### code chunk number 105: ind-corr-fig
###################################################
do.call(par, par_btest)
hist(cors, breaks = 20,
     main = "Correlations of monthly returns",
     xlab = "")
abline(v = median(cors))


###################################################
### code chunk number 106: market
###################################################
### ... market
ff3 <- French("~/Downloads/French",
              "F-F_Research_Data_Factors_daily_CSV.zip",
              frequency = "daily",
              price.series = TRUE,
              na.rm = TRUE)

market <- zoo(ff3[["Mkt-RF"]]*ff3[["RF"]],
              as.Date(row.names(ff3)))
market <- scale1(window(market, start = START, end = END),
                 level = 100)


###################################################
### code chunk number 107: ew
###################################################
ew <- function() {
    n <- ncol(Close())
    rep(1/n, n)
}


###################################################
### code chunk number 108: bt-ew
###################################################
bt.ew <- btest(prices = list(coredata(P)),
               signal = ew,
               do.signal = "lastofmonth",
               convert.weights = TRUE,
               initial.cash = 100,
               b = 250,
               timestamp = timestamp,
               instrument = instrument)


###################################################
### code chunk number 109: benchmarks1 (eval = FALSE)
###################################################
## ## chunkname: benchmarks1
## bm <- merge_series(market = market,
##                    "Equal-Weight" = bt.ew)
## par(mar = c(3, 3, 1, 3))
## fan_plot(P)
## abline(v = attr(bm, "scale1_origin"))
##
## par(xpd=TRUE)
## for (i in 1:2) {
##     lines(bm[, i],
##           ## plot.type = "single",
##           col = c(col.market, col.ew)[i],
##           ylab = paste("Growth of USD 100 since",
##                        as.character(attr(bm, "scale1_origin"))))
##     text(max(timestamp), tail(bm[, i], 1),
##          c("market", "equal-weight")[i],
##          pos = 4, cex = 0.6)
##
## }


###################################################
### code chunk number 110: benchmarks2 (eval = FALSE)
###################################################
## ## chunkname: benchmarks2
## plot(series_ratio(bm), xlab = "", ylab = "Performance market/EW")
## abline(v = attr(bm, "scale1_origin"))


###################################################
### code chunk number 111: benchmarks1-fig
###################################################
do.call(par, par_btest)
bm <- merge_series(market = market,
                   "Equal-Weight" = bt.ew)
par(mar = c(3, 3, 1, 3))
fan_plot(P)
abline(v = attr(bm, "scale1_origin"))

par(xpd=TRUE)
for (i in 1:2) {
    lines(bm[, i],
          ## plot.type = "single",
          col = c(col.market, col.ew)[i],
          ylab = paste("Growth of USD 100 since",
                       as.character(attr(bm, "scale1_origin"))))
    text(max(timestamp), tail(bm[, i], 1),
         c("market", "equal-weight")[i],
         pos = 4, cex = 0.6)

}


###################################################
### code chunk number 112: benchmarks2-fig
###################################################
do.call(par, par_btest)
par(mar = c(3, 3, 1, 3))
plot(series_ratio(bm), xlab = "", ylab = "Performance market/EW")
abline(v = attr(bm, "scale1_origin"))


###################################################
### code chunk number 113: mom
###################################################
mom <- function(P, k) {
    o <- order(P[nrow(P), ]/P[1, ], decreasing = TRUE)
    w <- numeric(ncol(P))
    w[o[1:k]] <- 1/k
    w
}


###################################################
### code chunk number 114: mom-test
###################################################
P3 <- matrix(c( 1  , 1  , 1,
                1.1, 1.2, 1.3),
             nrow = 2, byrow = TRUE)
mom(P3, k = 2)


###################################################
### code chunk number 115
###################################################
mom(P3, k = 1)


###################################################
### code chunk number 116
###################################################
mom.latest <- mom(head(coredata(P), 250), 10)
table(mom.latest)
df <- data.frame(sector = instrument,
                 weight = mom.latest)
df[df$weight > 0, ]


###################################################
### code chunk number 117: signal-fun
###################################################
signal <- function(fun, ...) {
    P <- Close(n = 250)
    fun(P, ...)
}


###################################################
### code chunk number 118: bt-mom
###################################################
bt.mom <- btest(prices = list(coredata(P)),
                signal = signal,
                do.signal = "lastofmonth",
                convert.weights = TRUE,
                initial.cash = 100,
                k = 10,
                fun = mom,
                b = 250,
                timestamp = timestamp,
                instrument = instrument)


###################################################
### code chunk number 119
###################################################
## summary(as.NAVseries(bt.mom), na.rm = TRUE)


###################################################
### code chunk number 120
###################################################
print(summary(as.NAVseries(bt.mom), na.rm = TRUE),
      monthly.returns = FALSE)


###################################################
### code chunk number 121: series.mom
###################################################
series.mom <- merge_series(
    Momentum = bt.mom,
    "Equal-weight" = bt.ew,
    Market = market)


###################################################
### code chunk number 122: momentum1 (eval = FALSE)
###################################################
## ## chunkname: momentum1
## plot(series.mom,
##      plot.type = "single", log = "y",
##      col = c(grey(0), col.market, col.ew),
##      ylab = paste("Growth of USD 100 since",
##                   as.character(attr(series.mom, "scale1_origin"))))
## abline(v = attr(series.mom, "scale1_origin"))


###################################################
### code chunk number 123: momentum2 (eval = FALSE)
###################################################
## ## chunkname: momentum2
## plot(series.mom[, "Market"], log = "y",
##      col = grey(.5), ylab = "Performance", xlab = "")
## ## lines(series.mom[, "Momentum"])
## lines(series_ratio(
##     series.mom[, c("Momentum", "Market")]),
##       ylab = "Performance Momentum/Market", xlab = "")
## abline(v = attr(series.mom, "scale1_origin"))


###################################################
### code chunk number 124: momentum1-fig
###################################################
do.call(par, par_btest)
par(mar = c(3, 3, 1, 3))
plot(series.mom,
     plot.type = "single", log = "y",
     col = c(grey(0), col.market, col.ew),
     ylab = paste("Growth of USD 100 since",
                  as.character(attr(series.mom, "scale1_origin"))))
abline(v = attr(series.mom, "scale1_origin"))
par(xpd = TRUE)
for (i in 1:3)
    text(max(timestamp), tail(series.mom[, i], 1),
         c("momentum", "equal-weight", "market")[i],
         pos = 4, cex = 0.7)
par(xpd = FALSE)


###################################################
### code chunk number 125: momentum2-fig
###################################################
do.call(par, par_btest)
plot(series.mom[, "Market"], log = "y",
     col = grey(.5), ylab = "Performance", xlab = "")
## lines(series.mom[, "Momentum"])
lines(series_ratio(
    series.mom[, c("Momentum", "Market")]),
      ylab = "Performance Momentum/Market", xlab = "")
abline(v = attr(series.mom, "scale1_origin"))


###################################################
### code chunk number 126: long-short
###################################################
library("parallel")
runs <- 100
args_short <- vector("list", length = runs)
args_long <- vector("list", length = runs)
for (i in seq_len(runs)) {
    when <- cumsum(c(250, sample(5:25, 5000, replace=TRUE)))
    when <- when[when <= length(timestamp)]

    args_short[[i]] <- list(prices = list(coredata(P)),
                            signal = signal,
                            do.signal = when,
                            convert.weights = TRUE,
                            initial.cash = 100,
                            k = 10,
                            fun = mom,
                            b = 550,
                            ## tc = 0.0025,
                            timestamp = timestamp,
                            instrument = instrument)

    when <- cumsum(c(250, sample(26:300, 5000, replace=TRUE)))
    when <- when[when <= length(timestamp)]

    args_long[[i]] <- list(prices = list(coredata(P)),
                            signal = signal,
                            do.signal = when,
                            convert.weights = TRUE,
                            initial.cash = 100,
                            k = 10,
                            fun = mom,
                            b = 550,
                            ## tc = 0.0025,
                            timestamp = timestamp,
                            instrument = instrument)

}

cl <- makePSOCKcluster(rep("localhost", 4))
clusterEvalQ(cl, require("PMwR"))
variations1 <- clusterApplyLB(cl, args_short,
                             function(x) do.call(btest, x))
variations2 <- clusterApplyLB(cl, args_long,
                             function(x) do.call(btest, x))
stopCluster(cl)
mom_vars1 <- do.call(merge_series, variations1)
mom_vars2 <- do.call(merge_series, variations2)


###################################################
### code chunk number 127: sensitivity-check1
###################################################
do.call("par", par_btest)

fan_plot(mom_vars1)
fan_plot(mom_vars2, lines = TRUE)


###################################################
### code chunk number 128: sensitivity-check3
###################################################
do.call("par", par_btest)
plot(index(mom_vars1),
     apply(mom_vars1, 1, median)/apply(mom_vars2, 1, median),
     type = "l", ylab = "Ratio of median performance", xlab = "")


###################################################
### code chunk number 129: sensitivity-check2
###################################################
do.call("par", par_btest)

plot (density(returns(mom_vars2, period = "ann")), xlim = c(0.1, .2),
      ylim = c(0, 100), main = "",
      col = gray(0.5))
lines(density(returns(mom_vars1, period = "ann")))


###################################################
### code chunk number 130: prototypes
###################################################
cov_fun <- function(R, ...) {
    ## ....
}

mv_fun <- function(var, wmin, wmax) {
    ## ....
}


###################################################
### code chunk number 131: signal-mv
###################################################
signal_mv <- function(cov_fun, mv_fun,
                      wmin, wmax, n, ...) {

    ## cov_fun .. takes a matrix R of returns
    ##            (plus ...), and evaluates to
    ##            the variance--covariance matrix
    ##            of those returns
    ##
    ## mv_fun  .. takes a covariance matrix and
    ##            min/max weights, and returns
    ##            minimum-variance weights

    P <- Close(n = 2500)
    i <- seq(1, nrow(P), by = n)
    R <- PMwR::returns(P[i, ])
    cv <- cov_fun(R, ...)
    mv_fun(cv, wmin, wmax)
}


###################################################
### code chunk number 132: mv-qp
###################################################
mv_qp <- function(var, wmin, wmax) {

    na <- dim(var)[1L]
    A <- rbind(1, -diag(na), diag(na))
    bvec <- rbind(1,
                  array(-wmax, dim = c(na, 1L)),
                  array( wmin, dim = c(na, 1L)))

    quadprog::solve.QP(
                  Dmat = 2*var,
                  dvec = rep(0, na),
                  Amat = t(A),
                  bvec = bvec,
                  meq  = 1L)$solution
}


###################################################
### code chunk number 133
###################################################
## should we use .returns ?
R <- returns(head(P, 1250), period = "month")
all.equal(returns(head(coredata(P), 1250)),
          .returns(head(coredata(P), 1250), lag = 1))

library("rbenchmark")
benchmark(returns(head(coredata(P), 1250)),
          .returns(head(coredata(P), 1250), lag = 1),
          replications = 1000)


###################################################
### code chunk number 134: qp-test
###################################################
library("quadprog")
R <- returns(head(P, 1250), period = "month")
var <- cov(R)
w <- mv_qp(var, wmin = 0, wmax = 0.2)


###################################################
### code chunk number 135
###################################################
df <- data.frame(vol = apply(R, 2, sd),
                 weight = round(100*w, 2))
sum(df[order(df$vol)[1:10], ])


###################################################
### code chunk number 136: bt-mv
###################################################
bt.mv <- btest(prices = list(coredata(P)),
               signal = signal_mv,
               do.signal = "lastofquarter",
               convert.weights = TRUE,
               initial.cash = 100,
               b = 2500,
               cov_fun = cov,
               mv_fun = mv_qp,
               wmax = 0.2,
               wmin = 0.00,
               n = 20,
               timestamp = timestamp,
               instrument = instrument)


###################################################
### code chunk number 137: mv-market-ew
###################################################
do.call(par, par_btest)
tmp_series <- merge_series(MV = bt.mv, Market = market)
plot(tmp_series,
     plot.type = "single", log = "y",
     col = c(grey(0.5), col.market),
     ylab = "Growth of USD 100")


###################################################
### code chunk number 138: summaries
###################################################
print(summary(window(as.NAVseries(market, title = "Market"),
                     start = as.Date("1999-12-31"))),
      sparkplot = FALSE, monthly.returns = FALSE)
print(summary(window(as.NAVseries(bt.mv, title = "Minimum Variance"),
                     start = as.Date("1999-12-31"))),
      sparkplot = FALSE, monthly.returns = FALSE)


###################################################
### code chunk number 139: when-rebalance
###################################################
library("parallel")
runs <- 100
args_rnd_rebalance <- vector("list", length = runs)
for (i in seq_len(runs)) {
    when <- cumsum(c(2501, sample(5:125, 5000, replace=TRUE)))
    when <- when[when <= length(timestamp)]

    args_rnd_rebalance[[i]] <-
        list(prices = list(coredata(P)),
             signal = signal_mv,
             do.signal = when,
             convert.weights = TRUE,
             initial.cash = 100,
             cov_fun = cov,
             mv_fun = mv_qp,
             wmax = 0.1,
             wmin = 0.00,
             n = 20,
             b = 2500,
             timestamp = timestamp,
             instrument = instrument)
}

cl <- makePSOCKcluster(rep("localhost", 4))
clusterEvalQ(cl, require("PMwR"))
variations_rnd_rebalance <-
    clusterApplyLB(cl, args_rnd_rebalance,
                   function(x) do.call(btest, x))
stopCluster(cl)
series_rnd_rebalance <- do.call(merge_series,
                                variations_rnd_rebalance)


###################################################
### code chunk number 140
###################################################
cat("Annualized volatilities along price paths:\n")
summary(16*apply(returns(series_rnd_rebalance), 2, sd))
cat("Annualized returns along price paths:\n")
summary(returns(series_rnd_rebalance, period = "ann"))


###################################################
### code chunk number 141: mv-variations
###################################################
do.call(par, par_btest)
fan_plot(series_rnd_rebalance, log = "")
## plot(series_rnd_rebalance[, 1], lwd = 0.25)
## for (i in 2:ncol(series_rnd_rebalance))
##    lines(series_rnd_rebalance[, i], lwd = 0.25)


###################################################
### code chunk number 142: mv-pairs
###################################################
do.call(par, par_btest)
pairs(cbind("Variation 1" = returns(as.NAVseries(variations_rnd_rebalance[[1]])),
            "Variation 2" = returns(as.NAVseries(variations_rnd_rebalance[[2]])),
            "Variation 3" = returns(as.NAVseries(variations_rnd_rebalance[[3]])),
            "Variation 4" = returns(as.NAVseries(variations_rnd_rebalance[[4]]))),
      pch = 21, cex = 0.5,
      col = "white", bg = grey(0.3), lwd = 0.1)


###################################################
### code chunk number 143: mv-positions
###################################################
do.call(par, par_btest)
par(mfrow=c(4,1))
pp <- 2
plot.zoo(zoo(100*position(variations_rnd_rebalance[[1]])[, pp], timestamp), xlab = "", ylab = "position in percentage points")
plot.zoo(zoo(100*position(variations_rnd_rebalance[[2]])[, pp], timestamp), xlab = "", ylab = "")
plot.zoo(zoo(100*position(variations_rnd_rebalance[[3]])[, pp], timestamp), xlab = "", ylab = "")
plot.zoo(zoo(100*position(variations_rnd_rebalance[[4]])[, pp], timestamp), xlab = "", ylab = "")


###################################################
### code chunk number 144: mv-ls
###################################################
mv_ls <- function(var, wmin, wmax) {

    na <- dim(var)[1L]
    if (length(wmin) == 1L)
        wmin <- rep(wmin, na)
    if (length(wmax) == 1L)
        wmax <- rep(wmax, na)

    .neighbour <- function(w) {
        stepsize <- runif(1L, min = 0, max = 0.1)
        toSell <- which(w > wmin)
        toBuy <- which(w < wmax)
        i <- toSell[sample.int(length(toSell), size = 1L)]
        j <-  toBuy[sample.int(length( toBuy), size = 1L)]
        stepsize <- runif(1) * stepsize
        stepsize <- min(w[i] - wmin[i], wmax[j] - w[j],
                        stepsize)
        w[i] <- w[i] - stepsize
        w[j] <- w[j] + stepsize
        w
    }

    .pvar <- function(w)
        w %*% var %*% w

    NMOF::LSopt(.pvar,
                list(neighbour = .neighbour,
                     x0 = rep(1/na, na),
                     nI = 10000,
                     printBar    = FALSE,
                     printDetail = FALSE))$xbest
}


###################################################
### code chunk number 145
###################################################
w.qp <- mv_qp(var, 0.0, 0.2)
w.ls <- mv_ls(var, 0.0, 0.2)

summary(10000*(w.qp - w.ls))


###################################################
### code chunk number 146: variations-ls
###################################################
variations_ls <-
    btest(prices = list(coredata(P)),
          signal = signal_mv,
          do.signal = "lastofquarter",
          convert.weights = TRUE,
          initial.cash = 100,
          b = 2500,
          cov_fun = cov,
          mv_fun = mv_ls,
          wmax = 0.2,
          wmin = 0.00,
          n = 20,
          timestamp = timestamp,
          instrument = instrument,
          replications = 100,
          variations.settings =
              list(method = "parallel",
                   cores = 10))


###################################################
### code chunk number 147
###################################################
series_variations_ls <- do.call(merge_series, variations_ls)
cat("Annualized volatilities along price paths:\n")
summary(16*apply(returns(series_variations_ls), 2, sd))
cat("Annualized returns along price paths:\n")
summary(returns(series_variations_ls, period = "ann"))


###################################################
### code chunk number 148: mv-variations-ls
###################################################
do.call(par, par_btest)
fan_plot(series_variations_ls, log = "")


###################################################
### code chunk number 149: zoo
###################################################
mom.latest <- mom(tail(coredata(P), 250), 10)


###################################################
### code chunk number 150
###################################################
mom(tail(P, 250), 10)


###################################################
### code chunk number 151
###################################################
## merge(e1, e2, all = FALSE, retclass = NULL)
## NextMethod(.Generic)


###################################################
### code chunk number 152: zoo-merge
###################################################
zoo(1, 1) + zoo(1, 5)
zoo(1, 1:2) + zoo(1, 2:3)


###################################################
### code chunk number 153: zoo-integer
###################################################
zoo(1, 5:10)
zoo(1, 5:10)[1]
zoo(1, 5:10)[5]
zoo(1, 5:10)[I(5)]
