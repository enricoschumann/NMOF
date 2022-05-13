
## always a matrix, even for a single path ...
s <- gbm(1, 10, r=0, v = 0, tau = 1, 100)
expect_true(all(dim(s) == c(11, 1)))
expect_true(all.equal(c(s), rep(100, length(s))))

## ... and single step
s <- gbm(1, 1, r=0, v = 0, tau = 1, 100)
expect_true(all(dim(s) == c(2, 1)))
expect_true(all.equal(c(s), rep(100, length(s))))
