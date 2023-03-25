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


## Heston
## example with extreme volatility
S <- 100
X <- 100
tau <- 1
r <- 0.00
q <- 0.00
v0 <- 3^2
vT <- 3^2
rho <- -0.5
k <- 0.5
sigma <- 1

value <- expect_error(callHestoncf(S = S, X = X,
                                   tau = tau, r = r, q = q,
                                   v0 = v0, vT = vT, rho = rho,
                                   k = k, sigma = sigma,
                                   implVol = TRUE))
## Error in uniroot(diffPrice, interval = c(0.0001, 2), call = result, S = S,  :
##   f() values at end points not of opposite sign



## 1) compute Heston price without implied vol
value <- callHestoncf(S = S, X = X,
                      tau = tau, r = r, q = q,
                      v0 = v0, vT = vT, rho = rho,
                      k = k, sigma = sigma,
                      implVol = FALSE)
## [1] 84.21328

## 2) compute implied vol separately
ivol <- vanillaOptionImpliedVol(
    price = value,
    S = S, X = X, tau = tau,
    r = r, q = q,
    uniroot.control = list(interval = c(0.5, 4),
                           maxiter = 2000))
## [1] 2.824572


## 3) Check: price European option with implied vol
value.recomp <- vanillaOptionEuropean(
    S = S, X = X,
    tau = tau, r = r, q = q,
    v = ivol^2)
## $value
## [1] 84.21342
expect_equal(value.recomp$value, value, tolerance = 1e-5)


value <- callHestoncf(
    S = S, X = X,
    tau = tau, r = r, q = q,
    v0 = v0, vT = vT, rho = rho,
    k = k, sigma = sigma,
    implVol = TRUE,
    uniroot.control = list(interval = c(0.5, 4),
                           maxiter = 2000))

## with the same 'uniroot.control', functions
## 'vanillaOptionImpliedVol' and 'callHestoncf' should
## result in the same implied volatility
expect_equal(value$impliedVol, ivol)


value <- callHestoncf(
    S = S, X = X,
    tau = tau, r = r, q = q,
    v0 = v0, vT = vT, rho = rho,
    k = k, sigma = sigma,
    implVol = TRUE,
    uniroot.control = list(interval = c(0.5, 4),
                           maxiter = 2000),
    uniroot.info = TRUE)





## more GREEKS
## DvegaDspot (Haug, pp 32-33)
S <- 90
X <- 80
tau <- 0.25
r <- 0.05
q <- 0
vol <- 0.2

tmp <- vanillaOptionEuropean(S = S, X = X, r = r, q = q,
                             tau = tau, v = vol*vol,
                             type = "put")

expect_equal(round(tmp$DvegaDspot, 4), -1.0008)


## DvegaDvol (Haug, pp 58-59)
S <- 90
X <- 130
tau <- 0.75
r <- 0.05
q <- 0.05
vol <- 0.28

tmp <- vanillaOptionEuropean(S = S, X = X, r = r, q = q,
                             tau = tau, v = vol*vol,
                             type = "put")

expect_equal(round(tmp$DvegaDvol, 4), 92.3444)
