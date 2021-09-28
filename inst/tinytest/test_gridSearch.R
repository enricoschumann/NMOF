library("NMOF")
library("tinytest")

sets <- function(x) {
    ans <- NULL
    for (i in 1:length(x)) {
        x1 <- utils::combn(x, i)
        ans <- c(ans, split(x1,  col(x1)))
    }
    names(ans) <- NULL
    ans
}

test_fun <- function(x) {
    List1 <- x$c1
    Num1  <- x$n1
    Num2  <- x$n2

    -length(List1) - Num1 - Num2
}

ans <- gridSearch(test_fun,
                  levels = list(c1 = sets(letters[1:5]),
                                n1 = 1:5,
                                n2 = 101:105),
                  asList = TRUE, keepNames = TRUE)

expect_equal(test_fun(ans$levels[[200]]), ans$values[[200]])

expect_error(ans <- gridSearch(test_fun,
                               levels = list(c1 = sets(letters[1:5]),
                                             n1 = 1:5,
                                             n2 = 101:105),
                               asList = FALSE, keepNames = TRUE))


## more tests
testFun <- function(x)
    x[[1L]] + x[[2L]]^2

## specify all levels
res <- gridSearch(fun = testFun, levels = list(1:2, c(2, 3, 5)),
                  asList = TRUE)
expect_equal(res$minfun, testFun(list(1, 2)))

levels <- list(a = 1:2, b = 1:3)
res <- gridSearch(testFun, levels, asList = TRUE)
expect_equal(res$minfun, testFun(list(1, 1)))

## specify lower, upper and npar
lower <- 1; upper <- 3; npar <- 2
res <- gridSearch(testFun, lower = lower, upper = upper, npar = npar,
                  asList = TRUE)
expect_equal(res$minfun, testFun(list(1, 1)))


## specify lower, upper, npar and n
lower <- 1; upper <- 3; npar <- 2; n <- 4
res <- gridSearch(testFun, lower = lower, upper = upper, npar = npar, n = n,
                  asList = TRUE)
expect_equal(res$minfun, testFun(list(1, 1)))
expect_equal(length(res$levels), n^npar)

lower <- 1; upper <- 3; npar <- 5; n <- 5
res <- gridSearch(testFun, lower = lower, upper = upper, npar = npar, n = n,
                  asList = TRUE)
expect_equal(res$minfun, testFun(list(1, 1)))
expect_equal(length(res$levels), n^npar)


## specify lower, upper and n
lower <- c(1,1); upper <- c(3,3); n <- 4
res <- gridSearch(testFun, lower = lower, upper = upper, n = n,
                  asList = TRUE)
expect_equal(res$minfun, testFun(list(1, 1)))
expect_equal(length(res$levels), n^length(lower))



## specify lower, upper (auto-expanded) and n
lower <- c(1,1); upper <- 3; n <- 4
res <- gridSearch(testFun, lower = lower, upper = upper, n = n)
expect_equal(res$minfun, testFun(list(1, 1)))
expect_equal(length(res$levels), n^length(lower))
