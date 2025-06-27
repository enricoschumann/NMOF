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








testFun  <- function(x) x[1L] + x[2L]^2
testFun1 <- function(x,k) x[1L] + x[2L]^2 + k
testFun2 <- function(x) x[[1L]] + x[[2L]]^2
testFun3 <- function(x) sum(x^2)
testFun4 <- function(x) sum(sapply(x, `^`, 2L))

## use 1 -- specify all levels
levels <- list(a = 1:2, b = 1:3)
res <- gridSearch(testFun, levels, printDetail = FALSE)
expect_equal(res$minfun,2)
expect_equal(res$values,
             apply(as.matrix(expand.grid(levels)), 1L, testFun))

## use 2 -- specify lower, upper and npar
lower <- 1; upper <- 3; npar <- 2
res <- gridSearch(testFun, lower = lower, upper = upper,
                  npar = npar, printDetail = FALSE)
expect_equal(res$minfun,2)

## use 3 -- specify lower, upper, npar and n
lower <- 1; upper <- 3; npar <- 2; n <- 4
res <- gridSearch(testFun, lower = lower, upper = upper,
                  npar = npar, n = n, printDetail = FALSE)
expect_equal(res$minfun,2)

## use 4 -- specify lower, upper and n (same result as 'use 3')
lower <- c(1,1); upper <- c(3,3); n <- 4
res1 <- gridSearch(testFun, lower = lower, upper = upper, n = n,
                   printDetail = FALSE)
expect_equal(res, res1)

## use 5 -- specify lower, upper (auto-expanded) and n
## (same result as 'use 3')
lower <- c(1,1); upper <- 3; n <- 4
res2 <- gridSearch(testFun, lower = lower, upper = upper,
                   n = n, printDetail = FALSE)
expect_equal(res1, res2)

## use 6 -- specify lower (auto-expanded), upper and n
## (same result as 'use 3')
lower <- 1; upper <- c(3,3); n <- 4
res3 <- gridSearch(testFun, lower = lower, upper = upper,
                   n = n, printDetail = FALSE)
expect_equal(res2, res3)

## additional argument passed with '...'
levels <- list(a = 1:2, b = 1:3); k <- 5
res <- gridSearch(testFun1, levels, k = k, printDetail = FALSE)
expect_equal(res$minfun, 7)

## 'keepNames'
testFunNames  <- function(x) x[["a"]] + x[["b"]]^2
levels <- list(a = 1:2, b = 1:3)
res <- gridSearch(testFunNames, levels, printDetail = FALSE,
                  keepNames = TRUE)
expect_equal(res$minfun, 2)
res <- gridSearch(testFunNames, levels, printDetail = FALSE,
                  keepNames = TRUE, asList = TRUE)
expect_equal(res$minfun, 2)
testFunNames  <- function(x) x[["a"]] + x[["C"]]^2
expect_error(gridSearch(testFunNames, keepNames = TRUE,
                        levels, printDetail = FALSE))

## ERROR -- length(upper) != length(lower)
lower <- 1:3; upper <- 5:4; n <- 8
expect_error(gridSearch(fun = testFun,
                        lower = lower, upper = upper,
                        n = n, printDetail = FALSE))

## ERROR -- upper < lower
lower <- 1:3; upper <- 2; n <- 8
expect_error(gridSearch(fun = testFun,
                        lower = lower, upper = upper, n = n,
                        printDetail = FALSE))

##
lower <- 1:3; upper <- 5; n <- 8
sol <- gridSearch(fun = testFun,
                  lower = lower, upper = upper,
                  n = n, printDetail = FALSE)
expect_equal(sol$minlevels,1:3)

## compare levels with 'expand.grid' results
levels <- list(a = 1:2, b = 1:3, c = 4:6)
sol <- gridSearch(fun = testFun3, levels = levels, printDetail = FALSE)
expect_equal(sol$minlevels,c(1,1,4))
## check levels
l1 <- do.call("rbind", sol$levels)
l2 <- as.matrix(expand.grid(levels))
dimnames(l2) <- NULL
expect_equal(l1,l2)

## error: must be greater than 1
lower <- 1; upper <- 5; n <- 1
sol <- expect_error(gridSearch(fun = testFun3,
                               lower = lower, upper = upper,
                               n = n, printDetail = FALSE))
##
## NSS fit
tm <- seq_len(10); trueP <- c(2,1,1,5,1,3); y <- NSS(trueP, tm)
qfit <- function(x, tm, y) {
    X <- NSSf(x[1], x[2], tm)
    mm <- lm(y ~ -1 + X)
    sum(abs(y - NSS(c(coef(mm), x[1], x[2]), tm)))
}
res <- gridSearch(qfit, y = y, tm = tm,
                  lower = 0.0, upper = 5, npar = 2L, n = 11L,
                  printDetail = FALSE)
ll <-  res$minlevels
X <- NSSf(ll[1L], ll[2L], tm)
model <- lm(y ~ -1 + X)
expect_equal(sum(abs(y - NSS(c(coef(model), ll[1L], ll[2L]), tm))), 0)
