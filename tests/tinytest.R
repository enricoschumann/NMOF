if (requireNamespace("tinytest", quietly = TRUE))
    tinytest.results <- tinytest::test_package("NMOF",
                                               color = interactive(),
                                               verbose = 1)
