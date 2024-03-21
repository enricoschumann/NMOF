## example edition 1, ch. 11/p. 290
res0 <- structure(c(0.3, 0.348, 0.444, 0.78, 0.324, 0.372, 0.468, 0.804),
                  .Dim = c(4L, 2L),
                  .Dimnames = list(NULL, c("lower", "upper")))
testFun <- function(x)
    cos(1/x^2)
expect_equal(res0, bracketing(testFun, interval = c(0.3, 0.9), n = 26L))

expect_equal(res0, bracketing(testFun, interval = c(0.3, 0.9), n = 26L),
            method = "vectorise")
expect_equal(res0,bracketing(testFun, lower = 0.3, upper = 0.9, n = 26L))

## 'lower'/'upper' ignored if 'interval' is specified
expect_equal(res0, bracketing(testFun, interval = c(0.3, 0.9),
                              lower = 0.1, upper = 0.99, n = 26L))
expect_equal(bracketing(testFun, interval = c(0.1, 0.99)),
             bracketing(testFun, lower = 0.1, upper = 0.99))

## ERROR: lower < upper
expect_error(bracketing(testFun, lower = 0.3, upper = 0.3, n = 26L))
## ERROR: no interval
expect_error(bracketing(testFun, n = 26L))
expect_error(bracketing(testFun, lower = 0.1, n = 26L))
expect_error(bracketing(testFun, upper = 0.1, n = 26L))

## no zero
testFun <- function(x) 1
res <- bracketing(testFun, interval = c(1,10), n = 10L)
expect_true(all(dim(res) == c(0L, 2L)))

## no zero either
testFun <- function(x) 0
res <- bracketing(testFun, interval = c(1,10), n = 10L)
expect_true(all(dim(res) == c(0L, 2L)))

## additional parameter
testFun <- function(x,k) cos(1/x^2) + k
res <- bracketing(testFun, k = 0, interval = c(0.3,0.9), n = 26L,
                  method = "vectorise")
expect_equal(res0, res)
res <- bracketing(testFun, k = 1, interval = c(0.3,0.9), n = 26L,
                  method = "vectorise")
expect_true(all(dim(res) == c(0L, 2L)))
