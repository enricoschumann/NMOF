## -*- truncate-lines: t; -*-

test.minvar <- function() {

    var <- structure(c(0.000988087100677907, -0.0000179669410403153, 0.000368923882626859,
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
    checkTrue(all(res >= 0))
    checkTrue(all(res <= 0.25))
    checkEqualsNumeric(sum(res), 1)
    checkEqualsNumeric(attr(res, "variance"),
                       res %*% var %*% res)


    ## group constraints
    res <- minvar(var, wmin = 0, wmax = 0.40, groups = list(1, 4:5),
                  groups.wmin = c(0.29, 0.1),
                  groups.wmax = c(0.30, 0.2))

    checkTrue(res[1] >= 0.29 - 1e-10)
    checkTrue(res[1] <= 0.30 + 1e-10)
    checkEqualsNumeric(sum(res), 1)

    checkTrue(sum(res[4:5]) >= 0.1 - 1e-10)
    checkTrue(sum(res[4:5]) <= 0.2 + 1e-10)
    checkEqualsNumeric(sum(res), 1)

}

test.mvPortfolio <- function() {

    na <- 4
    vols <- c(0.10, 0.15, 0.20,0.22)
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
    checkEqualsNumeric(x1, x2)

    ## maximum return
    x1 <- mvPortfolio(m, var, lambda = 0.99999999)
    checkEqualsNumeric(x1, c(0,1,0,0))

}
