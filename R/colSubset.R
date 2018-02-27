## -*- truncate-lines: t; -*-

colSubset <- function(x) {

    nr <- dim(x)[1L]
    nc <- dim(x)[2L]
    QR <- qr(x, LAPACK = FALSE)

    if ((qrank <- QR$rank) == nc) {
        list(columns = seq_len(nc),
             multiplier = diag(nc))
    } else {
        cols  <- QR$pivot[seq_len(qrank)]
        cols_ <- QR$pivot[(qrank + 1L):nc]
        D <- array(0, dim = c(qrank, nc))
        D[cbind(seq_along(cols), cols)] <- 1
        D[ ,cols_] <- qr.solve(x[ ,cols], x[ ,cols_])
        list(columns = cols,
             multiplier = D)
    }
}
