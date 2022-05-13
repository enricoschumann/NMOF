## -*- truncate-lines: t; -*-

if (requireNamespace("quadprog") &&
    requireNamespace("Rglpk")) {

    ## --- minvar

    var <- structure(
        c(0.000988087100677907, -0.0000179669410403153, 0.000368923882626859,
          0.000208303611101873, 0.000262742052359594, -0.0000179669410403153,
          0.00171852167358765, 0.0000857467457561209, 0.0000215059246610556,
          0.0000283532159921211, 0.000368923882626859, 0.0000857467457561209,
          0.00075871953281751, 0.000194002299424151, 0.000188824454515841,
          0.000208303611101873, 0.0000215059246610556, 0.000194002299424151,
          0.000265780633005374, 0.000132611196599808, 0.000262742052359594,
          0.0000283532159921211, 0.000188824454515841, 0.000132611196599808,
          0.00025948420130626),
        .Dim = c(5L, 5L),
        .Dimnames = list(c("CBK.DE", "VOW.DE", "CON.DE", "LIN.DE", "MUV2.DE"),
                         c("CBK.DE", "VOW.DE", "CON.DE", "LIN.DE", "MUV2.DE")))


    res <- minvar(var, wmin = 0, wmax = 0.25)
    expect_true(all(res >= 0))
    expect_true(all(res <= 0.25))
    expect_equivalent(sum(res), 1)
    expect_equivalent(attr(res, "variance"),
                      c(res %*% var %*% res))


    ###      ---------------- group constraints
    res <- minvar(var, wmin = 0, wmax = 0.40, groups = list(1, 4:5),
                  groups.wmin = c(0.29, 0.1),
                  groups.wmax = c(0.30, 0.2))

    expect_true(res[1] >= 0.29 - 1e-10)
    expect_true(res[1] <= 0.30 + 1e-10)
    expect_equivalent(sum(res), 1)

    expect_true(sum(res[4:5]) >= 0.1 - 1e-10)
    expect_true(sum(res[4:5]) <= 0.2 + 1e-10)
    expect_equivalent(sum(res), 1)


    ## group constraints: names
    res2 <- minvar(var, wmin = 0, wmax = 0.40,
                   groups = c("A", "none", "none", "B", "B"),
                   groups.wmin = c(A = 0.29, B = 0.1),
                   groups.wmax = c(A = 0.30, B = 0.2))


    expect_true(res2[1] >= 0.29 - 1e-10)
    expect_true(res2[1] <= 0.30 + 1e-10)
    expect_equivalent(sum(res2), 1)

    expect_true(sum(res2[4:5]) >= 0.1 - 1e-10)
    expect_true(sum(res2[4:5]) <= 0.2 + 1e-10)
    expect_equivalent(sum(res2), 1)

    expect_equivalent(res, res2)

    if (Sys.getenv("ES_PACKAGE_TESTING_73179826243954") == "true") {

        ## mvPortfolio
        na <- 4
        vols <- c(0.10, 0.15, 0.20, 0.22)
        m <- c(0.06, 0.12, 0.09, 0.07)
        const_cor <- function(rho, na) {
            C <- array(rho, dim = c(na, na))
            diag(C) <- 1
            C
        }
        var <- diag(vols) %*% const_cor(0.5, na) %*% diag(vols)


        ## minimum variance
        x1 <- mvPortfolio(m, var, lambda = 1e-10)
        x2 <- minvar(var)
        expect_equivalent(x1, x2)

        ## maximum return
        x1 <- mvPortfolio(m, var, lambda = 0.99999999)
        expect_equivalent(x1, c(0, 1, 0, 0))




        ## group constraints
        na <- 7
        ns <- 50
        R <- randomReturns(na = na, ns = ns, sd = 0.02, mean = 0)

        sol1 <- mvPortfolio(m = colMeans(R), var = cov(R),
                            min.return = 0,
                            groups = list(1, 4:5),
                            groups.wmin = c(0.25, 0.1),
                            groups.wmax = c(0.30, 0.2))

        expect_true(sol1[1] >= 0.25 - 1e-12)
        expect_true(sol1[1] <= 0.30 + 1e-12)

        expect_true(sum(sol1[4:5]) >= 0.1 - 1e-12)
        expect_true(sum(sol1[4:5]) <= 0.2 + 1e-12)


        sol2 <- mvPortfolio(m = colMeans(R), var = cov(R),
                            min.return = 0,
                            groups = c("A", "none", "none", "B", "B", "none", "none"),
                            groups.wmin = c(A = 0.25, B = 0.1),
                            groups.wmax = c(A = 0.30, B = 0.2))

        expect_true(sol2[1] >= 0.25 - 1e-12)
        expect_true(sol2[1] <= 0.30 + 1e-12)

        expect_true(sum(sol2[4:5]) >= 0.1 - 1e-12)
        expect_true(sum(sol2[4:5]) <= 0.2 + 1e-12)

        expect_equivalent(sol1, sol2)





        ## --- minCVaR

        runs <- 1e2

        for (i in seq_len(runs)) {
            R <- randomReturns(na = 15, ns = 500, sd = 0.01, rho = 0.5)

            opt <- minCVaR(R = R,
                           q = 0.05,
                           wmin = 0.05,
                           wmax = 0.2,
                           m = rep(0.01, dim(R)[2L]),
                           min.return = 0.01)
            expect_true(all(opt >= 0.05))
            expect_true(all(opt <= 0.2))
        }


        for (i in seq_len(runs)) {
            R <- randomReturns(na = 15, ns = 500, sd = 0.01, rho = 0.5)

            opt <- minCVaR(R = R,
                           q = 0.05,
                           wmin = 0.05,
                           wmax = 0.2,
                           groups = list(1:3, 5:8),
                           groups.wmin = c(0.18, 0.4),
                           groups.wmax = c(0.3, 0.55),
                           m = rep(0.01, dim(R)[2L]),
                           min.return = 0.01)
            expect_true(all(opt >= 0.05))
            expect_true(all(opt <= 0.2))

            expect_true(sum(opt[1:3]) >= 0.18 - 1e-9)
            expect_true(sum(opt[5:8]) >= 0.40 - 1e-9)

            expect_true(sum(opt[1:3]) <= 0.30 + 1e-9)
            expect_true(sum(opt[5:8]) <= 0.55 + 1e-9)
        }
    }
}
