greedySearch <- function(OF, algo, ...) {

    ## all.neighbours(x, ...)
    ##     next.neighbour(x, ...)
    ## while (!is.null(n <- next.neigbour(x, ...))) {

    ## }
    OF1 <- function(x)
        OF(x, ...)

    xbest  <- algo$x0
    xbestF <- OF1(xbest)

    v.best <- Inf
    improve <- TRUE
    for (i in seq_len(algo$maxit)) {
        if (algo$loopOF) {
            all.xn <- algo$all.neighbours(xbest, ...)
            j.best <- NA
            for (j in seq_along(all.xn)) {
                v <- OF1(all.xn[[j]])
                if (v < xbestF) {
                    j.best <- j
                    xbestF <- v
                }
            }
            if (is.na(j.best)) {
                improve <- FALSE
            } else {
                xbest <- all.xn[[j.best]]
            }
        } else {
            stop("not implemented")
        }
        if (!improve)
            break
    }
    ans <- list(xbest = xbest, OFvalue = xbestF,
                Fmat = NA,
                xlist = NA,
                initial.state = NA,
                x0 = algo$x0,
                iterations = i)
}
