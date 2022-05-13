v <- barrierOptionEuropean(S = 100,
                           X = 90,
                           H = 95,
                           tau = 0.5,
                           r = 0.08,
                           q = 0.04,
                           v = 0.25^2,
                           type = "call",
                           barrier.type = "downout",
                           rebate = 3 )
expect_equivalent(round(v, 4), 9.0246)

v <- barrierOptionEuropean(S = 100,
                           X = 90,
                           H = 95,
                           tau = 0.5,
                           r = 0.08,
                           q = 0.04,
                           v = 0.25^2,
                           type = "put",
                           barrier.type = "downout",
                           rebate = 3 )
expect_equivalent(round(v, 4), 2.2798)

v <- barrierOptionEuropean(S = 100,
                           X = 100,
                           H = 95,
                           tau = 0.5,
                           r = 0.08,
                           q = 0.04,
                           v = 0.25^2,
                           type = "call",
                           barrier.type = "downout",
                           rebate = 3 )
expect_equivalent(round(v, 4), 6.7924)
