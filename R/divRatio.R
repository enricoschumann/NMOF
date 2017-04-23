## -*- truncate-lines: t; -*-

divRatio <- function(w, var) {
    w <- c(w)
    n <- dim(var)[[1L]]
    sqrt(var[seq(1L, n*n, by = n + 1L)]) %*% w / sqrt(w %*% var %*% w)
}
