## -*- truncate-lines: t; -*-


ns <- 120
R <- randomReturns(na = 1 + 20,
                   ns = ns,
                   sd = 0.03,
                   mean = 0.005,
                   rho = 0.7)

var <- cov(R)

if (requireNamespace("quadprog")) {

    sol.qp1 <- trackingPortfolio(var, wmax = 0.4)
    sol.qp2 <- trackingPortfolio(R = R, wmax = 0.4)
    expect_equivalent(sol.qp1, sol.qp2)


    ## use crossprod instead of var
    sol.qp.R   <- trackingPortfolio(R = R,
                                    wmax = 0.4,
                                    objective = "sum.of.squares")
    sol.qp.var <- trackingPortfolio(var = crossprod(R),
                                    wmax = 0.4,
                                    objective = "variance")
    expect_equivalent(sol.qp.R, sol.qp.var)


    ## local search
    sol.ls <- trackingPortfolio(var = var, R = R, wmax = 0.4,
                                method = "ls",
                                ls.algo = list(nI = 3500))
    expect_true(max(abs(sol.ls - sol.qp1)) < 0.002)

}
