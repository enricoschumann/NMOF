changeInterval <- function(nodes, weights,
                           oldmin, oldmax, newmin, newmax) {
    newrange <- newmax - newmin
    oldrange <- oldmax - oldmin
    nodes <- (newrange*nodes + newmin*oldmax - newmax*oldmin)/oldrange
    weights <- weights * newrange/oldrange
    list(nodes = nodes, weights = weights)
}

xwGauss <- function(n, method = "legendre") {
    n <- makeInteger(n, "'n'", 1L)
    method <- match.arg(method, choices = c("legendre"))
    if (n == 1L)
        return(list(nodes = 0, weights = 2))
    if (method == "legendre") {
        ind <- seq_len(n - 1L)
        eta <- 1 / sqrt(4 - (ind)^(-2))
        A <- matrix(0, nrow = n, ncol = n)
        A[cbind(ind,    ind+1L)] <- eta
        A[cbind(ind+1L, ind)] <- eta
        eig <- eigen(A, symmetric = TRUE)
        x <- eig$values
        i <- order(x)
        x <- x[i]
        w <- 2 * eig$vectors[1, i] * eig$vectors[1, i]
        list(nodes = x, weights = w)
    } else {
        stop("unknown method")
    }
}
