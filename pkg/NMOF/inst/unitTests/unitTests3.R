require("NMOF")

## APPLICATIONS

## TA portfolio optimisation
test.TAopt.Applications <- function() {
    ## [requires quadprog]
    ## Test if TA can solve two simple portfolio optimisation
    ## problems: the minimum-variance portfolio and the tangency
    ## portfolio (subject to constraints).
    if (require(quadprog, quietly = TRUE)) {
        ## na      - number of assets
        ## ns      - observations
        ## R       - returns
        ## wsup    - maximum holding size
        ## winf    - minimum holding size
        ## mu, mu2 - means, excess means over risk-free rate rf

        na <- 50L; ns <- 100L
        R  <- array(rnorm(ns*na, mean = 0.005, sd = 0.015), dim = c(ns,na))
        mu <- colMeans(R); rf <- 0.0001; mu2 <- mu - rf

        ## TEST 1: minimum-variance portfolio
        wsup <- 0.05; winf <- -0.05; Q <- 2*cov(R)
        A <- array( 1, dim = c(1,na)); a <- 1
        B <- rbind(-diag(na),diag(na))
        b <- rbind(array(-wsup, dim = c(na,1)),
                   array( winf, dim = c(na,1)))
        result <- solve.QP(Dmat = Q, dvec = rep(0,na),
                           Amat = t(rbind(A,B)), bvec = rbind(a,b),
                           meq = 1)
        data <- list(RR = cov(R), na = na, ns = ns,
                     eps = 0.25/100, winf = winf, wsup = wsup)
        resample <- function(x, ...) x[sample.int(length(x), ...)]
        neighbour <- function(w, data){
            eps <- runif(1) * data$eps
            toSell <- w > data$winf; toBuy  <- w < data$wsup
            i <- resample(which(toSell), size = 1L)
            j <- resample(which(toBuy),  size = 1L)
            eps <- min(w[i] - data$winf, data$wsup - w[j], eps)
            w[i] <- w[i] - eps; w[j] <- w[j] + eps
            w
        }
        OF <- function(w, data) {
            aux <- crossprod(data$RR,w)
            crossprod(w,aux)
        }
        w0 <- runif(na); w0 <- w0/sum(w0)
        algo <- list(x0 = w0, neighbour = neighbour,
                     nS = 2500L, nT = 10L, nD = 2000L, q = 0.05,
                     printBar = FALSE, printDetail = FALSE)
        res <- TAopt(OF,algo,data)
        dF <- as.numeric(16 * 100 *sqrt(res$OFvalue)) -
            as.numeric(16 * 100 *sqrt(result$value))
        checkEquals(round(dF,4),0)

        ## TEST 2: tangency portfolio with non-negative weights
        winf <- 0; Q <- cov(R)
        A <- array(mu2, dim = c(1L, na)); a <- 1
        B <- diag(na); b <- array( winf, dim = c(na,1L))
        result <- solve.QP(Dmat = Q, dvec = rep(0,na),
                           Amat = t(rbind(A,B)), bvec = rbind(a,b),
                           meq = 1)
        w <- as.matrix(result$solution/sum(result$solution))
        SR <- t(w) %*% mu2 / sqrt(t(w) %*% Q %*% w)
        OF2 <- function(w, data) {
            aux <- crossprod(data$RR,w)
            sqrt(crossprod(w,aux)) / t(w) %*% data$mu2
        }
        w0 <- runif(na); w0 <- w0/sum(w0)
        data <- list(RR = cov(R), na = na, ns = ns, mu2 = mu2,
                     eps = 0.25/100, winf = winf, wsup = 1)
        res <- TAopt(OF2,algo,data)
        dSR <- 1/res$OFvalue - as.numeric(SR)
        checkEquals(as.numeric(round(dSR,3)),0)
    }
}
