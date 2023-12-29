\name{approxBondReturn}
\alias{approxBondReturn}
\title{
  Approximate Total Return of Bond
}
\description{
  Approximate the total return of a bond by its current yield,
  duration and convexity.
}
\usage{
approxBondReturn(yield, tm, n = 2, scale = 1/250, pad = NULL)
}
\arguments{
  \item{yield}{
    a numeric vector
}
  \item{tm}{
    a numeric vector: time-to-maturity
}
  \item{n}{
    number of coupon payments per period
}
  \item{scale}{
    how to scale yield; see Details
}
  \item{pad}{
    how to pad the first observation: \code{NULL} (default) means to
    drop it; useful alternatives are \code{\link{NA}} or \code{0}
}
}
\details{%
  The function approximates the total return of a bond investor,
  based on changes in yield.  The computation is based on a
  Taylor-series expansion.
  See the references, in particular concerning the shortcomings
  of the approximation:
  \enumerate{
    \item approximation is based on par yield
    \item it relies on yield alone, so does not take into
    account defaults; so for indices, the approximation should
    only used for issuers without defaults
  }
}
\value{
  a numeric vector, with attributes \code{duration} and \code{convexity}
}
\references{
  Swinkels, L. (2019). Treasury Bond Return Data Starting in 1962.
  Data. \strong{4} (3).

  Tuckman, B. and Serrat, A. (2012).
  \emph{Fixed Income Securities -- Tools for Today's Markets}.
  3rd edition. Wiley.

}
\author{
  Enrico Schumann
}
\note{
  Package \pkg{treasuryTR} implements the method as well.
}
%% \seealso{
%% %% ~~objects to See Also as \code{\link{help}}, ~~~
%% }
\examples{
yield0 <- 0.05
tm <- 20
cf <- c(rep(5, tm-1), 105)
duration(cf, 1:tm, yield0)

approxBondReturn(yield = c(yield0, 0.05), tm = tm, n = 1)
## ==> no price change, current yield is earned

approxBondReturn(yield = c(yield0, 0.04), tm = tm, n = 1)
## ==> current yield + price changed is earned
}
