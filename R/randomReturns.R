randomReturns <- function(na, ns, sd, mean = 0, rho = 0) {
    ## sd   = vol of returns
    ## mean = means of returns
    ##      ==> both may be scalars or vectors of length na

    ans <- rnorm(ns*na)
    dim(ans) <- c(na, ns)

    if (rho != 0) {
        C <- array(rho, dim = c(na, na))
        diag(C) <- 1
        ans <- t(chol(C)) %*% ans
    }
    ans <- ans*sd
    ans <- ans + mean
    t(ans)
}
