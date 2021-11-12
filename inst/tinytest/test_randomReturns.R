R <- randomReturns(na = 3, ns = 300, sd = 1:3/100)
ans <- apply(R, 2, sd)
expect_true(ans[2] > ans[1])
expect_true(ans[3] > ans[2])

C <- rbind(c( 1  , 0.8, -0.2),
           c( 0.8, 1  ,  0  ),
           c(-0.2, 0  ,  1  ))
R <- randomReturns(na = 3, ns = 300, sd = 1:3/100,
                   rho = C)
expect_true(cor(R)[1, 2] > 0.6)
expect_true(cor(R)[1, 3] < 0.0)

expect_equal(dim(randomReturns(na = 1, ns = 10, sd = 0.02)), c(10, 1))


R <- randomReturns(na = 3, ns = 10,
                   mean = c(0.1, 0.2, 0.3),
                   sd = 0.02,
                   exact = TRUE)
expect_equal(apply(R, 2, sd),   rep(0.02, 3))
expect_equal(apply(R, 2, mean), c(0.1, 0.2, 0.3))
expect_equal(cor(R)[lower.tri(cor(R))], rep(0, 3))

R <- randomReturns(na = 3, ns = 10,
                   mean = c(0.1, 0.2, 0.3),
                   rho = 0.5,
                   sd = 0.02,
                   exact = TRUE)
expect_equal(cor(R)[lower.tri(cor(R))], rep(0.5, 3))
