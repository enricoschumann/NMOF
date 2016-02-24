## -*- truncate-lines: t; -*-

pm <- function(x, xp = 2, threshold = 0, lower = TRUE,
               normalise = FALSE, na.rm = FALSE) {

    if (length(d <- dim(x))) {
        n <- d[1L]
        sum <- colSums
    } else {
        n <- length(x)
    }
    x <- x - threshold
    if (lower)
        x <- x - abs(x)
    else
        x <- x + abs(x)        
    x <- abs(x)

    ans <- if (xp == 1L)
        sum(x, na.rm = na.rm)/2
    else if (xp == 2L)
        sum(x*x, na.rm = na.rm)/4
    else if (xp == 3L)
        sum(x*x*x, na.rm = na.rm)/8
    else if (xp == 4L)
        sum(x*x*x*x, na.rm = na.rm)/16
    else 
        sum(x^xp, na.rm = na.rm)/2^xp
    ans <- ans/n

    if (normalise)
        ans^(1/xp)
    else
        ans

}


cm <- function(x, xp = 2, threshold = 0, lower = TRUE,
               normalise = FALSE, na.rm = FALSE) {

    if (length(d <- dim(x))) {
        n <- d[1L]
        sum <- colSums
    } else 
        n <- length(x)

    x <- x - threshold
    if (lower)
        x <- x - abs(x)
    else
        x <- x + abs(x)        
    x <- abs(x)

    ans <- if (xp == 1L)
        sum(x, na.rm = na.rm)/2
    else if (xp == 2L)
        sum(x*x, na.rm = na.rm)/4
    else if (xp == 3L)
        sum(x*x*x, na.rm = na.rm)/8
    else if (xp == 4L)
        sum(x*x*x*x, na.rm = na.rm)/16
    else 
        sum(x^xp, na.rm = na.rm)/2^xp
    ans <- ans/n * sum(x > threshold)/n

    if (normalise)
        ans^(1/xp)
    else
        ans

}
