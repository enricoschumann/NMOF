\name{vanillaBond}
\alias{vanillaBond}
\alias{duration}
\alias{convexity}
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
ytm(cf, times, y0 = 0.05, tol = 1e-05, maxit = 1000L, offset = 0)

duration(cf, times, yield, modified = TRUE, raw = FALSE)
convexity(cf, times, yield, raw = FALSE)
}
\arguments{
  \item{cf}{
    Cashflows; a numeric vector or a matrix. If a matrix, cashflows
    should be arranged in rows; times-to-payment correspond to columns.
  }
  \item{times}{
    times-to-payment; a numeric vector
  }
  \item{df}{
    discount factors; a numeric vector
  }
  \item{yields}{
    optional (instead of discount factors); zero yields to compute
    discount factor; if of length one, a flat zero curve is assumed
  }
  \item{yield}{

    numeric vector of length one (both duration and
    convexity assume a flat yield curve)

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
  \item{offset}{
    numeric: a \sQuote{base} rate over which to compute the yield to
    maturity.  See Details and Examples.
  }
  \item{modified}{
    logical: return modified duration? (default \code{TRUE})
  }
  \item{raw}{

    logical: default \code{FALSE}. Compute
    duration/convexity as derivative of cashflows'
    present value? Use this if you want to approximate
    the change in the bond price by a Taylor series
    (see Examples).

  }
}
\details{
  \code{vanillaBond} computes the present value of a vector of
  cashflows; it may thus be used to evaluate not just bonds but any
  instrument that can be reduced to a deterministic set of cashflows.

  \code{ytm} uses Newton's method to compute the yield-to-maturity of a
  bond (a.k.a. internal interest rate). When used with a bond, the initial
  outlay (i.e. the bonds dirty price) needs be included in the vector of
  cashflows.  For a coupon bond, a good starting value \code{y0} is
  the coupon divided by the dirty price of the bond.

  An \code{offset} can be specified either as a single number or as a
  vector of zero rates.  See Examples.

  %% \deqn{\mathrm{upper\ partial\ moment} = \frac{1}{n} \sum_{x >
  %%     t}\left(x - t \right)^e}{%
  %%   upper partial moment = sum_{x > t}(x - t)^e / n}

}
\value{
  numeric
}
\references{
  Gilli, M., Maringer, D. and Schumann, E. (2019)
  \emph{Numerical Methods and Optimization in Finance}.
  2nd edition. Elsevier. \doi{10.1016/C2017-0-01621-X}
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{NS}}, \code{\link{NSS}}
}
\examples{
## ytm
cf <- c(5, 5, 5, 5, 5, 105)   ## cashflows
times <- 1:6                  ## maturities
y <- 0.0127                   ## the "true" yield
b0 <- vanillaBond(cf, times, yields = y)
cf <- c(-b0, cf); times <- c(0, times)
ytm(cf, times)

## ... with offset
cf <- c(5, 5, 5, 5, 5, 105)   ## cashflows
times <- 1:6                  ## maturities
y <- 0.02 + 0.01              ## risk-free 2\% + risk-premium 1\%
b0 <- vanillaBond(cf, times, yields = y)
cf <- c(-b0, cf); times <- c(0, times)
ytm(cf, times, offset = 0.02) ## ... only the risk-premium

cf <- c(5, 5, 5, 5, 5, 105)   ## cashflows
times <- 1:6                  ## maturities
y <- NS(c(6,9,10,5)/100, times) ## risk-premium 1\%
b0 <- vanillaBond(cf, times, yields = y + 0.01)
cf <- c(-b0, cf); times <- c(0, times)
ytm(cf, times, offset = c(0,y)) ## ... only the risk-premium

## bonds
cf <- c(5, 5, 5, 5, 5, 105)   ## cashflows
times <- 1:6                  ## maturities
df <- 1/(1+y)^times           ## discount factors
all.equal(vanillaBond(cf, times, df),
          vanillaBond(cf, times, yields = y))

## ... using Nelson--Siegel
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

vanillaBond(cf = M, times = alltimes, yields = 0.02)

## duration/convexity
cf <- c(5, 5, 5, 5, 5, 105)   ## cashflows
times <- 1:6                  ## maturities
y <- 0.0527                   ## yield to maturity

d <- 0.001                   ## change in yield (+10 bp)
vanillaBond(cf, times, yields = y + d) - vanillaBond(cf, times, yields = y)

duration(cf, times, yield = y, raw = TRUE) * d

duration(cf, times, yield = y, raw = TRUE) * d +
    convexity(cf, times, yield = y, raw = TRUE)/2 * d^2

}
