## -*- truncate-lines: t; -*-

erc <- function(cov,  wmin = 0, wmax = 1, method = "ls") {

    fun <- function(x, Data) {
        tmp <- Data$S %*% x
        sd(x * tmp / c(sqrt(x %*% tmp)))
    }

    na <- dim(cov)[1L]
    if (length(wmin) == 1L)
        wmin <- rep(wmin, na)
    if (length(wmax) == 1L)
        wmax <- rep(wmax, na)

    N <- function (w, Data) {
        toSell <- which(w > Data$wmin)
        toBuy <- which(w < Data$wmax)
        i <- toSell[sample.int(length(toSell), size = 1L)]
        j <- toBuy[sample.int(length(toBuy), size = 1L)]
        eps <- Data$epsmin + runif(1L) *
            (Data$epsmax-Data$epsmin) * (Data$nS-LS.info()$s)/Data$nS
        eps <- min(w[i] - Data$wmin[i], Data$wmax[j] - w[j], eps)
        w[i] <- w[i] - eps
        w[j] <- w[j] + eps
        w
    }

    Data <- list(S = cov,
                 na = na,
                 wmin = wmin,
                 wmax = wmax,
                 epsmin = 0.0001,
                 epsmax = 0.1,
                 eps = 0.0005,
                 nS = 1000)

    sol <- LSopt(fun,
                 list(neighbour = N,
                      nS = Data$nS,
                      x0 = rep(1/Data$na, Data$na),
                      printDetail = FALSE,
                      printBar = FALSE),
                 Data)
    w <- sol$xbest
    w
}


## function for computing the minimum-variance portfolio
minvar <- function(var, wmin = 0, wmax = 1, method = "qp") {

    if (method == "qp" && !requireNamespace("quadprog"))
        stop("package ", sQuote("quadprog"), " is not available")
    na <- dim(var)[1L]

    finite.min <- is.finite(wmin[1])
    finite.max <- is.finite(wmax[1])
    if (length(wmin) == 1L && finite.min)
        wmin <- rep(wmin, na)
    if (length(wmax) == 1L && finite.max)
        wmax <- rep(wmax, na)
    Q <- 2 * var
    A <- rbind(numeric(na) + 1,
               if (!finite.max) NULL else -diag(na),
               if (!finite.min) NULL else  diag(na))
    bvec <- c(1,
              if (!finite.max) NULL else -wmax,
              if (!finite.min) NULL else  wmin)
    qp_res <- quadprog::solve.QP(Dmat = Q,
                                 dvec = rep(0, na),
                                 Amat = t(A),
                                 bvec = bvec,
                                 meq  = 1L)
    ans <- qp_res$solution
    attr(ans, "variance") <- qp_res$value
    ans
}


## function for computing n points of the efficient frontier
mvFrontier <- function(m, var, wmin = 0, wmax = 1, n = 50, rf = NA) {

    if (!requireNamespace("quadprog"))
        stop("package ", sQuote("quadprog"), " is not available")

    na <- dim(var)[1L]
    if (length(wmin) == 1L)
        wmin <- rep(wmin, na)
    if (length(wmax) == 1L)
        wmax <- rep(wmax, na)
    if (length(m) == 1L)
        m <- rep(m, na)

    rets <- risk <- numeric(n)
    portfolios <- array(0, dim = c(na, n))

    dvec <- numeric(na)
    if (is.na(rf)) {
        A <- rbind(1, -diag(na), diag(na))
        sq <- seq(0.0001, 0.9999, length.out = n)
        for (i in seq_len(n)) {
            lambda <- sq[i]
            result <- quadprog::solve.QP(Dmat = 2*(1 - lambda)*var,
                                         dvec = lambda*m,
                                         Amat = t(A),
                                         bvec = c(1, -wmax, wmin),
                                         meq  = 1L)
            rets[i] <- sum(m*result$solution)
            risk[i] <- sqrt(result$solution %*% var %*% result$solution)
            portfolios[, i] <- result$solution
        }
    } else {
        A <- rbind(m - rf, -diag(na), diag(na))
        r.seq <- seq(rf, max(m), length.out = n)
        cash <- numeric(n)
        for (i in seq_len(n)) {
            bvec  <- c(r.seq[i], -wmax, wmin)
            result <- quadprog::solve.QP(Dmat = var,
                                         dvec = rep.int(0, na),
                                         Amat = t(A),
                                         bvec = bvec)
            cash[i] <- 1 - sum(result$solution)
            rets[i] <- sum(m*result$solution) + cash[i]*rf
            risk[i] <- sqrt(result$solution %*% var %*% result$solution)
            portfolios[, i] <- result$solution
        }
        portfolios <- rbind(portfolios, cash)

    }
    list(returns = rets,
         volatility = risk,
         portfolios = portfolios)
}


## compute mean-variance efficient portfolio
mvPortfolio <- function(m, var, min.return, wmin = 0, wmax = 1) {

    if (!requireNamespace("quadprog"))
        stop("package ", sQuote("quadprog"), " is not available")

    na <- dim(var)[1L]
    if (length(wmin) == 1L)
        wmin <- rep(wmin, na)
    if (length(wmax) == 1L)
        wmax <- rep(wmax, na)
    if (length(m) == 1L)
        m <- rep(m, na)

    Q <- 2 * var
    A <- array( 1, dim = c(1L, na))
    a <- 1
    B <- array(m, dim = c(1L, na))
    B <- rbind(B,-diag(na),diag(na))
    b <- rbind(min.return, array(-wmax, dim = c(na,1L)),
               array( wmin, dim = c(na,1L)))
    result <- quadprog::solve.QP(Dmat = Q,
                                 dvec = rep(0,na),
                                 Amat = t(rbind(A,B)),
                                 bvec = rbind(a,b),
                                 meq  = 1L)$solution

    result
}
