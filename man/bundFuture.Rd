\name{bundFuture}
\alias{bundFuture}
\alias{bundFutureImpliedRate}
\title{
  Theoretical Valuation of Euro Bund Future
}
\description{
  Compute theoretical prices of bund future.
}
\usage{
bundFuture(clean, coupon, trade.date,
           expiry.date, last.coupon.date,
           r, cf)

bundFutureImpliedRate(future, clean, coupon,
                      trade.date, expiry.date,
                      last.coupon.date, cf)
}
\arguments{
  \item{clean}{
    numeric: clean prices of CTD
}
  \item{future}{
    numeric: price of future
}
  \item{coupon}{
    numeric
}
  \item{trade.date}{
    \code{\link{Date}} or character in format \code{YYYY-MM-DD}
}
  \item{expiry.date}{
    \code{\link{Date}} or character in format \code{YYYY-MM-DD}
}
  \item{last.coupon.date}{
    \code{\link{Date}} or character in format \code{YYYY-MM-DD}
}
  \item{r}{
    numeric: 0.01
}
  \item{cf}{
    numeric: conversion factor of CTD
}
}
\details{

  \code{bundFuture} computes the theoretical prices of the Bund Future,
  given the prices of the cheapest-to-deliver eligible government bond.

  \code{bundFutureImpliedRate} computes the implied refinancing rate.

}
\value{
  numeric
}
\author{
  Enrico Schumann
}
\references{

  Gilli, M., Maringer, D. and Schumann, E. (2019)
  \emph{Numerical Methods and Optimization in Finance}.
  2nd edition. Elsevier. \doi{10.1016/C2017-0-01621-X}

  Schumann, E. (2023) Financial Optimisation with R (\pkg{NMOF} Manual).
  \url{https://enricoschumann.net/NMOF.htm#NMOFmanual}

}
\examples{
## Bund-Future with expiry Sep 2017
## CTD: DE0001102408 -- 0%, 15 Aug 2026
##
## On 21 August 2017, the CTD traded (clean) at 97.769
## the FGBL Sep 2017 closed at 164.44.

bundFuture(clean = 97.769,                   ## DE0001102408
           coupon = 0,
           trade.date = "2017-8-21",
           expiry.date = "2017-09-07",       ## Bund expiry
           last.coupon.date = "2017-08-15",  ## last co
           r = -0.0037,
           cf = 0.594455)   ## conversion factor (from Eurex website)

bundFutureImpliedRate(future = 164.44,
                      clean = 97.769,
                      coupon = 0,
                      trade.date = "2017-8-21",
                      expiry.date = "2017-09-07",
                      last.coupon.date = "2017-08-15",
                      cf = 0.594455)
}
