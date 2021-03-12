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
