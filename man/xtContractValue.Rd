\name{xtContractValue}
\alias{xtContractValue}
\alias{xtTickValue}
\title{
  Contract Value of Australian Government Bond Future
}
\description{

  Compute the contract value of an Australian
  government-bond future from its quoted price.

}
\usage{
xtContractValue(quoted.price, coupon, do.round = TRUE)
xtTickValue(quoted.price, coupon, do.round = TRUE)
}
\arguments{
  \item{quoted.price}{
    The price, as in \code{99.02}.
}
  \item{coupon}{
    numeric; should be 6, not 0.06
}
  \item{do.round}{
    If \code{TRUE}, round as done by ASX clearing house.
}
}
\details{

  Australian government-bond futures, traded at the
  Australian Securities Exchange (\acronym{asx}), are
  quoted as \code{100 - yield}. The function computes
  the actual contract value from the quoted price.

  \code{xtTickValue} computes the tick value via a
  central difference.

}
\value{
  A numeric vector.
}
\references{
  \url{http://www.rba.gov.au/mkt-operations/resources/tech-notes/pricing-formulae.html}

  Gilli, M., Maringer, D. and Schumann, E. (2019) \emph{Numerical
  Methods and Optimization in Finance}. 2nd edition. Elsevier.
  \url{https://www.elsevier.com/books/numerical-methods-and-optimization-in-finance/gilli/978-0-12-815065-8}

  Schumann, E. (2019) Financial Optimisation with R (\pkg{NMOF} Manual).
  \url{http://enricoschumann.net/NMOF.htm#NMOFmanual}
}
\author{
  Enrico Schumann
}
%% \seealso{
%% %% ~~objects to See Also as \code{\link{help}}, ~~~
%% }
\examples{
quoted.price <- 99
coupon <- 6
xtContractValue(quoted.price, coupon)
xtTickValue(quoted.price, coupon)
## convexity
quoted.price <- seq(90, 100, by = 0.1)
plot(100 - quoted.price,
     xtContractValue(quoted.price, coupon),
     xlab = "Yield", ylab = "Contract value")
}
