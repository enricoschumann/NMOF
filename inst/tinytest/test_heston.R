## Reference Prices
## https://financepress.com/2019/02/15/heston-model-reference-prices/

## NMOF                         | LEWIS           | Unicode R
## -----------------------------+-----------------+------------------
## S     = spot                 |                 |
## X     = strike               |                 |
## tau   = time                 | ==> T           |
## r     = riskfree rate        |                 |
## q     = dividend yield       |                 |
## v0    = initial variance     | ==> V0          |
## vT    = long-run variance    | ==> omega/theta | "\u3c9"/"\u3b8"
## rho   = correlation          | rho             | "\u3c1"
## k     = speed mean-reversion | ==> theta       | "\u3b8"
## sigma = vol of vol           | ==> xi          | "\u3be"


S <- 100
X <- 100
tau <- 1
r <- 1/100
q <- 2/100
v0 <- 4/100
vT  <- 0.25
sigma <- 1
k <- 4

rho <- -0.5

H <- callHestoncf(S = S, X = X, tau = tau, r = r, q = q,
                  v0 = v0, vT = vT, rho = rho, k = k,
                  sigma = sigma, implVol = FALSE)
## [1] 16.07015492
##     16.0701549170
expect_equal(round(H, 5), 16.07015)

X <- 120
H <- callHestoncf(S = S, X = X, tau = tau, r = r, q = q,
                  v0 = v0, vT = vT, rho = rho, k = k,
                  sigma = sigma, implVol = FALSE)
## [1] 9.024913474
##     9.0249134834578
expect_equal(round(H, 5), 9.02491)


tau <- 0.01
v0 <- 0.01
X <- 100
H <- callHestoncf(S = S, X = X, tau = tau, r = r, q = q,
             v0 = v0, vT = vT, rho = rho, k = k,
             sigma = sigma, implVol = FALSE)
## [1] 0.4677826766
##     0.467782671512844
expect_equal(round(H, 5), 0.46778)
