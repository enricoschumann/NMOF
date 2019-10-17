### R code from "Numerical Methods and Optimization in Finance"

###################################################
### code chunk number 1: appendix-settings
###################################################
Sys.setenv(LANGUAGE = "en")
library("NMOF")
options(continue = "  ",
        digits = 3,
        width = 55,
        str = strOptions(strict.width = "cut"),
        useFancyQuotes = FALSE,
        warn = 2)
par.nmof <- list(bty = "n",
                 las = 1,
                 mar = c(3, 3, 1, 1),
                 mgp = c(2, 0.5, 0),
                 tck = 0.01,
                 ps = 9)


###################################################
### code chunk number 2: attach-pkg
###################################################
library("NMOF")
set.seed(2345567)


###################################################
### code chunk number 3: LSopt-example
###################################################
## Aim: find the columns of X that, when summed, give y
###
## random data set
nc <- 25L          ## number of columns in data set
nr <- 5L           ## number of rows in data set
howManyCols <- 5L  ## length of true solution
X <- array(runif(nr*nc), dim = c(nr, nc))
xTRUE <- logical(nc)
xTRUE[sample(1L:nc, howManyCols)] <- TRUE
Xt <- X[, xTRUE, drop = FALSE]
y <- rowSums(Xt)
Data <- list(X = X, y = y, nc = nc, nr = nr, n = 1L)
###
## a random solution x0 ...
makeRandomSol <- function(nc) {
    ii <- sample.int(nc, sample.int(nc, 1L))
    x0 <- logical(nc)
    x0[ii] <- TRUE
    x0
}
x0 <- makeRandomSol(nc)
###
## ... but probably not a good one
abs(sum(y - rowSums(X[, xTRUE, drop = FALSE]))) ## should be 0
abs(sum(y - rowSums(X[, x0, drop = FALSE])))

## a neighbourhood function: switch n elements in solution
neighbour <- function(xc, Data) {
    xn <- xc
    p <- sample.int(Data$nc, Data$n)
    xn[p] <- !xn[p]
    if (sum(xn) < 1L)
        xn <- xc
    xn
}
###
## an objective function
OF <- function(xn, Data)
    abs(sum(Data$y - rowSums(Data$X[, xn, drop = FALSE])))
###
## LOCAL SEARCH
ls.settings <- list(nI = 5000L,
                    neighbour = neighbour,
                    x0 = x0,
                    printBar = FALSE,
                    printDetail = FALSE)
solLS <- LSopt(OF, algo = ls.settings, Data = Data)
solLS$OFvalue  ## the true solution has OF-value 0


###################################################
### code chunk number 4: SAopt-example
###################################################
solSA <- SAopt(OF, algo = ls.settings, Data = Data)
solSA$OFvalue  ## the true solution has OF-value 0


###################################################
### code chunk number 5: TAopt-example
###################################################
solTA <- TAopt(OF, algo = ls.settings, Data = Data)
solTA$OFvalue  ## the true solution has OF-value 0


###################################################
### code chunk number 6: TAopt-example2
###################################################
ls.settings$nI <- 100000
solTA <- TAopt(OF, algo = ls.settings, Data = Data)
solTA$OFvalue  ## the true solution has OF-value 0


###################################################
### code chunk number 7: GAopt-example
###################################################
ga.settings <- list(nP = 500,
                    nG = 100,
                    nB = Data$nc,
                    printBar = FALSE)
solGA <- GAopt(OF, algo = ga.settings, Data = Data)
solGA$OFvalue


###################################################
### code chunk number 8: DEopt-example
###################################################
## Example: Trefethen's 100-digit challenge (problem 4)
## http://people.maths.ox.ac.uk/trefethen/hundred.html
###
de.settings <- list(nP = 50L,   ## population size
                    nG = 1000L, ## number of generations
                    F = 0.5,    ## step size
                    CR = 0.9,   ## prob of crossover
                    min = c(-10, -10), ## range of initial
                    max = c( 10,  10), ## population
                    printBar = FALSE)

solDE <- DEopt(OF = tfTrefethen,  ## see ?testFunctions
               algo = de.settings)
## correct answer:
# -3.30686864747523
noquote(format(solDE$OFvalue, digits = 12))
## check convergence of population
sd(solDE$popF)


###################################################
### code chunk number 9: PSopt-example
###################################################
ps.settings <- list(nP = 50L,  ## population size
                    nG = 1000L, ## number of generations
                    maxV = 0.9,
                    c1 = 1,
                    c2 = 1,
                    min = c(-10, -10), ## range of initial
                    max = c( 10,  10), ## population
                    printBar = FALSE)
###
solPS <- PSopt(OF = tfTrefethen, ## see ?testFunctions,
               algo = ps.settings)
## correct answer:
# -3.30686864747523
noquote(format(solPS$OFvalue, digits = 12))


###################################################
### code chunk number 10: restartOpt-example
###################################################
sols.ls <- restartOpt(LSopt, n = 100L,
                      OF = OF,
                      algo = ls.settings,
                      Data = Data)
###
sols.sa <- restartOpt(SAopt, n = 100L,
                      OF = OF,
                      algo = ls.settings,
                      Data = Data)
###
sols.ta <- restartOpt(TAopt, n = 100L,
                      OF = OF,
                      algo = ls.settings,
                      Data = Data)


###################################################
### code chunk number 11: restarts
###################################################
summary(sapply(sols.ls, `[[`, "OFvalue"))
summary(sapply(sols.sa, `[[`, "OFvalue"))
summary(sapply(sols.ta, `[[`, "OFvalue"))
