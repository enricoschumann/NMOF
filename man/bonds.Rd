\name{vanillaBond}
\alias{vanillaBond}
\alias{ytm}
\title{
  Pricing Plain-Vanilla Bonds
}
\description{
  
  Calculate the theoretical price and yield-to-maturity of a list of
  cashflows.

}
\usage{
vanillaBond(cf, times, df, yields)
ytm(cf, times, y0 = 0.05, tol = 1e-05, maxit = 1000L)        
}
\arguments{
  \item{cf}{
    Cashflows; a numeric vector or a matrix. If a matrix, cashflows
    should be arranged in rows; times-to-payment correspond to columns.
  }
  \item{times}{
    times-to-payment; a numeric vector.
  }
  \item{df}{
    discount factors; a numeric vector.
  }
  \item{yields}{
    optional (instead of discount factors); zero yields to compute
    discount factor; if of length one, a flat zero curve is assumed
  }
  \item{y0}{
    starting value
  }
  \item{tol}{
    tolerance
  }
  \item{maxit}{
    maximum number of iterations
  }
}
\details{
  \code{vanillaBond} computes the present value of a vector of
  cashflows; it may thus be used to evaluate not just bonds but any
  instrument that can be reduced to a deterministic set of cashflows.

  \code{ytm} uses Newton's method to compute the yield-to-maturity of a
  bond (or internal interest rate). When used with a bond, the initial
  outlay (ie, the bonds dirty price) needs be included in the vector of cashflows.
}
\value{
  Numeric.
}
\references{
  Gilli, M., Maringer, D. and Schumann, E. (2011) \emph{Numerical
    Methods and Optimization in Finance}. Elsevier.
  \url{http://www.elsevierdirect.com/product.jsp?isbn=9780123756626}
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{NS}}, code{\link{NSS}}
}
\examples{
cf <- c(5, 5, 5, 5, 5, 105)   ## cashflows
times <- 1:6                  ## maturities
y <- 0.0127                   ## the "true" yield
b0 <- vanillaBond(cf, times, yields = y)
cf <- c(-b0, cf); times <- c(0, times)
ytm(cf, times)

##
cf <- c(5, 5, 5, 5, 5, 105)   ## cashflows
times <- 1:6                  ## maturities
df <- 1/(1+y)^times           ## discount factors
all.equal(vanillaBond(cf, times, df),
          vanillaBond(cf, times, yields = y))

## using Nelson--Siegel
vanillaBond(cf, times, yields = NS(c(0.03,0,0,1), times))


## several bonds
##   cashflows are numeric vectors in a list 'cf',
##   times-to-payment are are numeric vectors in a
##   list 'times'

times <- list(1:3,
              1:4,
              0.5 + 0:5)
cf <- list(c(6, 6,          106),
           c(4, 4, 4,       104),
           c(2, 2, 2, 2, 2, 102))

alltimes <- sort(unique(unlist(times)))
M <- array(0, dim = c(length(cf), length(alltimes)))
for (i in seq_along(times))
    M[i, match(times[[i]], alltimes)] <- cf[[i]]
rownames(M) <- paste("bond.", 1:3, sep = "")
colnames(M) <- format(alltimes, nsmall = 1)
M

vanillaBond(cf = M, times = alltimes, yields = 0.02)
}
