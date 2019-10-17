### R code from "Numerical Methods and Optimization in Finance"

###################################################
### code chunk number 1: chapter-settings
###################################################
library("NMOF")
options(continue = "  ",
        digits = 3,
        width = 55,
        str = strOptions(strict.width = "cut"),
        useFancyQuotes = FALSE)


###################################################
### code chunk number 2: bsm
###################################################
callBSM <- function(S, X, tau, r, q, v, I = 1) {
    ## I .. 1 for a call, -1 for a put
    d1 <- (log(S/X) + (r - q + v/2) * tau)/
          sqrt(v * tau)
    d2 <- d1 - sqrt(v * tau)
    I * (S * exp(-q * tau) * pnorm(I * d1) -
         X * exp(-r * tau) * pnorm(I * d2))
}


###################################################
### code chunk number 3: warn
###################################################
options()$warn


###################################################
### code chunk number 4
###################################################
factorial(200)


###################################################
### code chunk number 5
###################################################
## options(warn = 2)
## factorial(200)


###################################################
### code chunk number 6
###################################################
options(warn = 2)
cat(try(factorial(200)))


###################################################
### code chunk number 7
###################################################
options(warn = 2)
exp( sum(log(1:200)) )
prod(1:200)


###################################################
### code chunk number 8: callHestoncf
###################################################
callHestoncf
