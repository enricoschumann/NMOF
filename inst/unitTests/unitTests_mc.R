## -*- truncate-lines: t; -*-

test.gbm <- function() {
    checkEquals(dim(gbm(1, 100, 0, 0.2^2, 1, 100)), c(101,1))
    checkEquals(dim(gbm(10, 100, 0, 0.2^2, 1, 100)), c(101,10))
}

test.gbb <- function() {
    checkEquals(dim(gbb(1, 100, 100,100, 0.2^2, tau=1)), c(101,1))
    checkEquals(dim(gbb(10, 100, 100,100, 0.2^2, tau=1)), c(101,10))
}
