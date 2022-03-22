library("tinytest")

v <- vanillaOptionAmerican(S = 100,
                           X = 100,
                           tau = 1,
                           r = 0,
                           q = 0,
                           v = 0.1^2,
                           M = 1)
expect_true(is.na(v$delta))
expect_true(is.na(v$gamma))
expect_true(is.na(v$theta))

v <- vanillaOptionAmerican(S = 100,
                           X = 100,
                           tau = 1,
                           r = 0,
                           q = 0,
                           v = 0.1^2,
                           M = 2)
expect_true(!is.na(v$delta))
expect_true(is.na(v$gamma))
expect_true(is.na(v$theta))

v <- vanillaOptionAmerican(S = 100,
                           X = 100,
                           tau = 1,
                           r = 0,
                           q = 0,
                           v = 0.1^2,
                           M = 3)
expect_true(!is.na(v$delta))
expect_true(!is.na(v$gamma))
expect_true(!is.na(v$theta))
