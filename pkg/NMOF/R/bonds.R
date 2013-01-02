vanillaBond <- function(cf, times, df, yields) {
    if (missing(times))
        times <- seq_len(length(cf))
    if (missing(df))
        df <- y2df(yields, times)
    
    cf %*% df
}
y2df <- function(yields, times) {
    if (length(yields)==1L)
        yields <- rep.int(yields, length(times))
    1/(1+yields)^times
}
ytm <- function(cf, times, y0 = 0.05,
                tol = 1e-05, maxit = 1000L) {        
    dr <- 1
    for (i in seq_len(maxit)) {
        y1 <- 1 + y0
        g <- sum(cf / y1 ^ times)
        t1 <- times - 1
        dg <- times * cf * 1/y1 ^ t1
        dg <- sum(dg)
        dr <- g/dg
        y0 <- y0 + dr
        if (abs(dr) < tol)
            break
    }
    y0
}
