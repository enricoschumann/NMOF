approxBondReturn <- function(yield, tm, n = 2, scale = 1/250,
                             pad = NULL) {

    D <- 1 / yield*(1 - 1/(1 + yield/n)^(n*tm))
    C <- 2/(yield*yield) *
        (1 - 1 / (1 + yield/n)^(n*tm)) -
        (2*tm)/(yield*(1 + yield/n)^(n*tm + 1))

    dy <- diff(yield)
    n <- length(yield)
    ans <- (yield[-n]+1)^scale - dy*D[-n] + 0.5*C[-n] * dy^2 - 1
    if (!is.null(pad)) {
        ans <- c(pad, ans)
    }

    attr(ans, "duration") <- D
    attr(ans, "convexity") <- C
    ans
}
