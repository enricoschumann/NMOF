## -*- truncate-lines: t; -*-
## Time-stamp: <2013-10-10 15:00:42 CEST (es)>

## create random portfolios

long.only <- function(n, budget = 1) {
    ans <- runif(n)
    budget * ans/sum(ans)
}

long.only2 <- function(n, k, budget = 1) {
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
