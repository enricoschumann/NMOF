vanillaBond <- function(cf, times, df, yields) {
    if (missing(times))
        times <- seq_len(length(cf))
    if (missing(df))
        df <- 1/(1+yields)^times
    drop(cf %*% df)
}

ytm <- function(cf, times, y0 = 0.05,
                tol = 1e-05, maxit = 1000L, offset = 0) {
    dr <- 1
    t1 <- times - 1
    offset <- 1 + offset
    for (i in seq_len(maxit)) {
        y1 <- offset + y0
        g <- sum(cf / y1 ^ times)
        dg <- times * cf * y1 ^ t1
        dg <- sum(dg)
        dr <- g/dg
        y0 <- y0 + dr
        if (abs(dr) < tol)
            break
    }
    y0
}

duration <- function(cf, times, yield,
                     modified = TRUE, raw = FALSE) {

    y1 <- 1 + yield
    dcf <- cf/y1^times
    if (raw)
        -sum(times * dcf) / y1
    else if (modified)
        sum(times * dcf) / sum(dcf) / y1
    else
        sum(times * dcf) / sum(dcf)
}

convexity <- function(cf, times, yield, raw = FALSE) {

    y1 <- 1 + yield
    dcf <- cf/y1^times

    if (raw)
        sum( (times+1) * times * dcf) / y1 / y1
    else
        sum( (times+1) * times * dcf) / y1 / y1 / sum(dcf)

}
