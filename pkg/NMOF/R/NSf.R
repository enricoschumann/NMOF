NSf <- function(lambda, tm) {
    aux <- tm/lambda
    X <- array(1, dim = c(length(tm),3L))
    X[ ,2L] <- (1 - exp(-aux))/aux
    X[ ,3L] <- ((1 - exp(-aux))/aux) - exp(-aux)
    X
}

