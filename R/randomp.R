## -*- truncate-lines: t; -*-
## Time-stamp: <2014-01-06 16:13:59 CET (es)>

## create random portfolios

long.only <- function(n, budget = 1) {
    ans <- runif(n)
    budget * ans/sum(ans)
}

long.only.k <- function(n, k, budget = 1) {
    ans <- numeric(n)
    i <- sample.int(n, k)
    ans[i] <- runif(length(i))
    budget*ans/sum(ans)
}

long.short <- function(n, budget = 1) {
    ans <- runif(n)
    while (all(ans >= 0) || all(ans < 0)) {
        tmp <- runif(n) < 0.3
        ans[tmp] <- -ans[tmp]
    }
    budget*ans/sum(ans)
}

random.p <- function(n, budget = 1,
                     wmin = 0, wmax = Inf, abs.max = Inf) {

}
