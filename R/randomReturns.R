randomReturns <- function(na, ns, sd, mean = 0, rho = 0,
                          exact = FALSE) {
    ## sd   = vol of returns
    ## mean = means of returns
    ##      ==> both may be scalars or vectors of length na

    ## TODO allow L to be passed as an argument
    L <- NULL

    rho1 <- all(rho == 1)
    rho0 <- identical(rho, 0) || identical(rho, 0L)

    if (rho1) {
        ans <-t(rnorm(ns))[rep(1L, na), , drop = FALSE]
    } else {
        ans <- rnorm(ns*na)
        dim(ans) <- c(na, ns)
    }

    if (exact) {
        if (!rho1)
            ## slightly faster for smaller matrices:
            ## compute inverse and premultiply
            ## ans <- t(backsolve(chol(cov(t(ans))), diag(1, na))) %*% ans
            ans <- solve(t(chol(cov(t(ans)))), ans)

        ans <- ans - rowMeans(ans)
        ans <- ans / apply(ans, 1, sd)
    }

    if (!rho0 && !rho1) {
        if (length(rho == 1L)) {
            C <- array(rho, dim = c(na, na))
            diag(C) <- 1
        } else
            C <- rho
        if (is.null(L))
            L <- t(chol(C))
        ans <- L %*% ans
    }
    ans <- ans*sd
    ans <- ans + mean
    t(ans)
}
