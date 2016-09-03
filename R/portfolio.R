## -*- truncate-lines: t; fill-column: 65; comment-column: 50; -*-

erc <- function(cov,  wmin = 0, wmax = 1, method = "ls") {

    fun <- function(x, Data) {
        tmp <- Data$S %*% x
        sd(x * tmp / c(sqrt(x %*% tmp)))
    }

    N <- function (w, Data) {
        toSell <- which(w > Data$wmin)
        toBuy <- which(w < Data$wmax)
        i <- toSell[sample.int(length(toSell), size = 1L)]
        j <- toBuy[sample.int(length(toBuy), size = 1L)]
        eps <- Data$epsmin + runif(1L) *
            (Data$epsmax-Data$epsmin) * (Data$nS-LS.info()$s)/Data$nS
        eps <- min(w[i] - Data$wmin, Data$wmax - w[j], eps)
        w[i] <- w[i] - eps
        w[j] <- w[j] + eps
        w
    }

    Data <- list(S = cov,
                 na = dim(cov)[1L],
                 wmin = 0,
                 wmax = 1,
                 epsmin = 0.0001,
                 epsmax = 0.1,             
                 eps = 0.0005,
                 nS = 1000)

    sol <- LSopt(fun,
                 list(neighbour = N,
                      nS = Data$nS,
                      x0 = rep(1/Data$na, Data$na),
                      printDetail = FALSE, ## print info every 1000 steps
                      printBar = FALSE),
                 Data)
    w <- sol$xbest
    w
}

## function for computing the minimum-variance portfolio
minvar <- function(var, wmin = 0, wmax = 1, method = "qp") {

    na <- dim(var)[1L]
    if (length(wmin) == 1L)
        wmin <- rep(wmin, na)
    if (length(wmax) == 1L)
        wmax <- rep(wmax, na)
    Q <- 2 * var
    A <- rbind(1, -diag(na), diag(na))
    bvec <- c(1, -wmax, wmin)
    quadprog::solve.QP(Dmat = Q,
                       dvec = rep(0, na),
                       Amat = t(A),
                       bvec = bvec,
                       meq  = 1L)$solution
}


## function for computing n points of the efficient frontier
mv_frontier <- function(m, var, wmin = 0, wmax = 1, n = 50) {
    na <- dim(var)[1L]
    if (length(wmin) == 1L)
        wmin <- rep(wmin, na)
    if (length(wmax) == 1L)
        wmax <- rep(wmax, na)

    rets <- risk <- numeric(n)
    portfolios <- array(0, dim = c(na, n))
        
    dvec <- numeric(na)
    A <- rbind(1, -diag(na), diag(na))
    
    sq <- seq(0.0001, 0.9999, length.out = n)
    for (i in seq_len(n)) {
        lambda <- sq[i]
        result <- quadprog::solve.QP(Dmat = 2*(1-lambda)*var,
                                     dvec = lambda*m,
                                     Amat = t(A),
                                     bvec = c(1, -wmax, wmin),
                                     meq  = 1L)
        rets[i] <- sum(m*result$solution)
        risk[i] <- sqrt(result$solution %*% var %*% result$solution)
        portfolios[, i] <- result$solution
    }

    list(returns = rets,
         volatility = risk,
         portfolios = portfolios)
}
