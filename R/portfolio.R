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
minvar <- function(var, wmin = 0, wmax = 1, method = "qp",
                   groups = NULL,
                   groups.wmin = NULL,
                   groups.wmax = NULL) {

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

    if (!is.null(groups)) {
        Groups <-
            group_constraints_matrices(na,
                                       groups,
                                       groups.wmin,
                                       groups.wmax)
        A <- rbind(A, Groups$A.ineq)
        bvec <- c(bvec, Groups$b.ineq)
    }

    qp_res <- quadprog::solve.QP(Dmat = Q,
                                 dvec = rep.int(0, na),
                                 Amat = t(A),
                                 bvec = bvec,
                                 meq  = 1L)
    ans <- qp_res$solution
    attr(ans, "variance") <- qp_res$value
    ans
}


## function for computing n points of the efficient frontier
mvFrontier <- function(m, var, wmin = 0, wmax = 1, n = 50, rf = NA,
                       groups = NULL,
                       groups.wmin = NULL,
                       groups.wmax = NULL) {

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
        bvec <- c(1, -wmax, wmin)

        if (!is.null(groups)) {
            Groups <-
                group_constraints_matrices(na,
                                           groups,
                                           groups.wmin,
                                           groups.wmax)
            A <- rbind(A, Groups$A.ineq)
            bvec <- c(bvec, Groups$b.ineq)
        }

        sq <- seq(0.0001, 0.9999, length.out = n)
        for (i in seq_len(n)) {
            lambda <- sq[i]
            result <- quadprog::solve.QP(Dmat = 2*(1 - lambda)*var,
                                         dvec = lambda*m,
                                         Amat = t(A),
                                         bvec = bvec,
                                         meq  = 1L)
            rets[i] <- sum(m*result$solution)
            risk[i] <- sqrt(result$solution %*% var %*% result$solution)
            portfolios[, i] <- result$solution
        }

    } else {
        A <- rbind(m - rf, -diag(na), diag(na))
        r.seq <- seq(rf, max(m), length.out = n)
        cash <- numeric(n)
        bvec.gr <- NULL
        if (!is.null(groups)) {
            Groups <-
                group_constraints_matrices(na,
                                           groups,
                                           groups.wmin,
                                           groups.wmax)
            A <- rbind(A, Groups$A.ineq)
            bvec.gr <- Groups$b.ineq
        }

        for (i in seq_len(n)) {
            bvec  <- c(r.seq[i], -wmax, wmin, bvec.gr)
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
mvPortfolio <- function(m, var, min.return, wmin = 0, wmax = 1,
                        lambda = NULL,
                        groups = NULL,
                        groups.wmin = NULL,
                        groups.wmax = NULL) {

    if (!requireNamespace("quadprog"))
        stop("package ", sQuote("quadprog"), " is not available")

    na <- dim(var)[1L]
    if (length(wmin) == 1L)
        wmin <- rep(wmin, na)
    if (length(wmax) == 1L)
        wmax <- rep(wmax, na)
    if (length(m) == 1L)
        m <- rep(m, na)

    if (is.null(lambda)) {
        lambda1 <- 1
        lambda2 <- 1
    } else if (length(lambda) == 1L) {
        if (lambda <= 0 || lambda >= 1)
            stop("lambda must lie in the range (0,1)")
        lambda1 <- lambda
        lambda2 <- 1 - lambda
    } else {
        lambda1 <- lambda[1L]
        lambda2 <- lambda[2L]
    }
    Q <- lambda2 * 2 * var
    A <- array( 1, dim = c(1L, na))
    a <- 1
    B <- rbind(if (is.null(lambda))
                   m,
               -diag(na),
                diag(na))
    b <- c(if (is.null(lambda))
                         min.return,
                     -wmax,
                     wmin)

    A <- rbind(A, B)
    bvec <- c(a, b)
    if (!is.null(groups)) {
        Groups <-
            group_constraints_matrices(na,
                                       groups,
                                       groups.wmin,
                                       groups.wmax)
        A <- rbind(A, Groups$A.ineq)
        bvec <- c(bvec, Groups$b.ineq)
    }

    result <- quadprog::solve.QP(Dmat = Q,
                                 dvec = if (is.null(lambda)) rep(0, na)
                                        else lambda1 * m,
                                 Amat = t(A),
                                 bvec = bvec,
                                 meq  = 1L)$solution
    result
}

group_constraints_matrices <-
function(na, groups, groups.wmin, groups.wmax) {

    if (is.factor(groups))
        groups <- as.character(groups)

    if (is.list(groups)) {
        G <- array(0, dim = c(length(groups), na))
        for (g in seq_along(groups))
            G[g, groups[[g]]] <- 1
    } else if (is.character(groups)) {
        sug <- sort(unique(groups))
        G <- array(0, dim = c(length(sug), na))
        rownames(G) <- sug
        for (g in sug)
            G[g, groups == g] <- 1
    } else
        stop("invalid", sQuote("groups"))

    A.ineq <- NULL
    b.ineq <- NULL
    if (!is.null(groups.wmax)) {
        G.max <- if (is.character(groups))
                     G[names(groups.wmax),, drop = FALSE] else G
        G.max <- G.max[is.finite(groups.wmax),, drop = FALSE]
        groups.wmax <- groups.wmax[is.finite(groups.wmax)]
        A.ineq <- rbind(A.ineq, -G.max)
        b.ineq <- c(b.ineq, -groups.wmax)
    }
    if (!is.null(groups.wmin)) {
        G.min <- if (is.character(groups))
                     G[names(groups.wmin),, drop = FALSE] else G
        G.min <- G.min[is.finite(groups.wmin),, drop = FALSE]
        groups.wmin <- groups.wmin[is.finite(groups.wmin)]
        A.ineq <- rbind(A.ineq, G.min)
        b.ineq <- c(b.ineq, groups.wmin)
    }
    list(A.ineq = A.ineq,
         b.ineq = b.ineq)
}

minCVaR <- function(R,
                    q = 0.1,
                    wmin = 0,
                    wmax = 1,
                    min.return = NULL,
                    m = NULL,
                    method = "Rglpk",
                    groups = NULL,
                    groups.wmin = NULL,
                    groups.wmax = NULL,
                    Rglpk.control = list()
                    ) {


    if (tolower(method) == "rglpk") {
        if (!requireNamespace("Rglpk"))
            stop("package ", sQuote("Rglpk"))

        control <- list(verbose = FALSE,
                        presolve = FALSE)

        control[names(Rglpk.control)] <- Rglpk.control

        b <- 1 - q
        ns <- nrow(R)
        na <- ncol(R)


        f.obj <- c(alpha = 1,
                   x = rep(0, na),
                   u = 1/rep(( 1 - b)*ns, ns))

        C <- cbind(1, R, diag(nrow(R)))
        C <- rbind(c(alpha = 0, x = rep(1, na), u = rep(0, nrow(R))), C)
        if (!is.null(min.return)) {
            if (is.null(m))
                m <- colMeans(R)
            C <- rbind(c(alpha = 0, x = m, u = rep(0, nrow(R))), C)
        }

        const.dir <- c(if (!is.null(min.return)) ">=", ## min return
                       "==")                           ## budget
        const.dir <- c(const.dir,
                       rep(">=", nrow(C) - length(const.dir)))

        const.rhs <- c(if (!is.null(min.return)) min.return, 1)
        const.rhs <- c(const.rhs, rep(0, nrow(C) - length(const.rhs)))

        default.bounds <- identical(wmin, 0) && identical(wmax, 1)

        na <- dim(R)[2L]
        if (!default.bounds) {
            if (length(wmin) == 1L)
                wmin <- rep.int(wmin, na)
            if (length(wmax) == 1L)
                wmax <- rep.int(wmax, na)
            bounds <- list(lower = list(ind = seq_len(na) + 1L, val = wmin),
                           upper = list(ind = seq_len(na) + 1L, val = wmax))
        }
        if (!is.null(groups)) {
            Groups <-
                group_constraints_matrices(na,
                                           groups,
                                           groups.wmin,
                                           groups.wmax)
            grp.lhs <- Groups$A.ineq
            grp.lhs <- cbind(0,
                             grp.lhs,
                             array(0, dim = c(nrow(grp.lhs), nrow(R))))
            grp.rhs <- Groups$b.ineq

            C <- rbind(C, grp.lhs)
            const.dir <- c(const.dir, rep.int(">=", nrow(grp.lhs)))
            const.rhs <- c(const.rhs, grp.rhs)
        }

        sol.lp <- Rglpk::Rglpk_solve_LP(f.obj,
                                        C,
                                        bounds = if (!default.bounds) bounds,
                                        const.dir,
                                        rhs = const.rhs,
                                        control = control)

        ans <- sol.lp$solution[2:(1+na)]
        attr(ans, "LP") <- sol.lp
    } else if (tolower(method) == "ls") {
        message("Not implemented. See http://enricoschumann.net/notes/minimising-conditional-var.html")
        ans <- invisible(NULL)
    } else {
        stop("method ", sQuote(method), " not supported")
    }
    ans
}

trackingPortfolio <- function(var, wmin = 0, wmax = 1,
                              method = "qp", objective = "variance",
                              R,
                              ls.algo = list()) {

    if (method == "qp") {

        if (!missing(R)) {
            na <- ncol(R) - 1L
        } else
            na <- ncol(var) - 1L

        if (length(wmin) == 1L)
            wmin <- rep(wmin, na)
        if (length(wmax) == 1L)
            wmax <- rep(wmax, na)

        A <- rbind(numeric(na) + 1,
                   -diag(na),
                   diag(na))
        b <- c(1, -wmax, wmin)

        if (!requireNamespace("quadprog"))
            stop("package ", sQuote("quadprog"), " is not available")

        if (objective == "variance" ) {
            if (missing(var))
                var <- cov(R)
            Dmat <- var[-1, -1]
            dvec <- var[1, -1]
        } else if (objective == "sum.of.squares") {
            Dmat <- crossprod(R[, -1])
            dvec <- c(crossprod(R[, 1], R[, -1]))
        }
        qp_res <- quadprog::solve.QP(Dmat = Dmat,
                                     dvec = dvec,
                                     Amat = t(A),
                                     bvec = b,
                                     meq  = 1L)
        ans <- qp_res$solution

    } else if (method %in% c("ls", "LSopt")) {

        if (objective == "variance" ) {
            te <- function(w, R)
                var(R[, -1] %*% w - R[, 1])

        } else if (objective == "sum.of.squares") {
            te <- function(w, R)
                crossprod(R[, -1] %*% w - R[, 1])
        }

        ## with package 'neighbours':
        ## nb <- neighbours::neighbourfun(type = "numeric",
        ##                                max = wmax,
        ##                                length = ncol(R) - 1,
        ##                                stepsize = 0.01)
        stepsize <- 0.01
        nb <- function(x, ...) {
            decrease <- which(x > wmin)
            increase <- which(x < wmax)
            i <- decrease[sample.int(length(decrease), size = 1L)]
            j <- increase[sample.int(length(increase), size = 1L)]
            stepsize <- stepsize * runif(1L)
            stepsize <- min(x[i] - wmin, wmax - x[j], stepsize)
            x[i] <- x[i] - stepsize
            x[j] <- x[j] + stepsize
            x
        }

        algo <- list(neighbour = nb,
                     nI = 2000,
                     printBar = FALSE,
                     printDetail = FALSE,
                     x0 = rep(1/(ncol(R) - 1), ncol(R) - 1))
        algo[names(ls.algo)] <- ls.algo

        sol.ls <- LSopt(te, algo = algo, R = R)
        ans <- sol.ls$xbest

    }
    ans
}

maxSharpe <- function(m, var, min.return,
                      wmin = -Inf,
                      wmax = Inf,
                      method = "qp",
                      groups = NULL,
                      groups.wmin = NULL,
                      groups.wmax = NULL) {

    if (is.infinite(wmin) && is.infinite(wmax) && is.null(groups)) {
        x <- solve(var, m)
        x/sum(x)
    }

    if (method == "qp" && !requireNamespace("quadprog"))
        stop("package ", sQuote("quadprog"), " is not available")
    na <- dim(var)[1L]
    finite.min <- is.finite(wmin[1])
    finite.max <- is.finite(wmax[1])

    if (missing(min.return)) {
        if (!finite.max)
            min.return <- 1
        else
            min.return <- max(c(wmin %*% m), 1e-6)
    }

    if (length(wmin) == 1L && finite.min)
        wmin <- rep(wmin, na)
    if (length(wmax) == 1L && finite.max)
        wmax <- rep(wmax, na)
    Q <- var
    A <- rbind(m,
               if (!finite.max) NULL else -diag(na) + wmax,
               if (!finite.min) NULL else  diag(na) - wmin)
    bvec <- c(min.return,
              if (!finite.max) NULL else rep(0, na),
              if (!finite.min) NULL else rep(0, na))

    if (!is.null(groups)) {
        stop("groups are not yet supported")
        Groups <-
            group_constraints_matrices(na,
                                       groups,
                                       groups.wmin,
                                       groups.wmax)
        A <- rbind(A, Groups$A.ineq)
        bvec <- c(bvec, Groups$b.ineq)
    }

    qp_res <- quadprog::solve.QP(Dmat = Q,
                                 dvec = rep.int(0, na),
                                 Amat = t(A),
                                 bvec = bvec,
                                 meq  = 1L)
    ans <- qp_res$solution/sum(qp_res$solution)
    ans
}

minMAD <- function(R,
                   wmin = 0,
                   wmax = 1,
                   min.return = NULL,
                   m = NULL,
                   demean = TRUE,
                   method = "lp",
                   groups = NULL,
                   groups.wmin = NULL,
                   groups.wmax = NULL,
                   Rglpk.control = list()) {

    na <- ncol(R)
    ns <- nrow(R)
    if (!is.null(groups))
        warning("group constraints not yet implemented")

    if (method == "lp") {
        rm <- colMeans(R)
        if (demean)
            R <- sweep(R, 2, rm)
        M <- rbind(c(rep(1, na), rep(0, ns)),  ## budget
                   cbind( R, diag(ns)),
                   cbind(-R, diag(ns)))
        dir <- c("==",  ## budget
                 rep(">=", 2*ns))
        rhs <- c(1,  ## budget
                 rep(0, 2*ns))

        if (!is.null(min.return)) {
            M <- rbind(M,
                       c(if (is.null(m)) rm else m, rep(0, ns)))
            dir <- c(dir, ">=")
            rhs <- c(rhs, min.return)
        }

        default.bounds <- identical(wmin, 0) && identical(wmax, 1)

        if (!default.bounds) {
            if (length(wmin) == 1L)
                wmin <- rep.int(wmin, na)
            if (length(wmax) == 1L)
                wmax <- rep.int(wmax, na)
            bounds <- list(lower = list(ind = seq_len(na) + 1L, val = wmin),
                           upper = list(ind = seq_len(na) + 1L, val = wmax))
        }

        sol.lp <- Rglpk::Rglpk_solve_LP(
                             obj = c(rep(0, na), rep(1/ns, ns)),
                             mat = M,
                             dir = dir,
                             rhs = rhs,
                             bounds = if (!default.bounds) bounds)
        ans <- sol.lp$solution[seq_len(na)]

    } else if (method == "ls") {

        if (demean) {
            mad <- function(w, R) {
                Rw <- R %*% w   ## compute portfolio returns under scenarios
                mean(abs(Rw - mean(Rw)))
            }
        } else {
            mad <- function(w, R) {
                Rw <- R %*% w   ## compute portfolio returns under scenarios
                sum(abs(Rw))
            }
        }

        stepsize <- 0.01
        nb <- function(x, ...) {
            decrease <- which(x > wmin)
            increase <- which(x < wmax)
            i <- decrease[sample.int(length(decrease), size = 1L)]
            j <- increase[sample.int(length(increase), size = 1L)]
            stepsize <- stepsize * runif(1L)
            stepsize <- min(x[i] - wmin, wmax - x[j], stepsize)
            x[i] <- x[i] - stepsize
            x[j] <- x[j] + stepsize
            x
        }

        ans <- LSopt(mad,
                     list(x0 = rep(1/ncol(R), ncol(R)),
                          neighbour = nb,
                          nI = 100000,
                          printDetail = FALSE,
                          printBar = FALSE),
                     R = R)$xbest
    }

    ans
}
