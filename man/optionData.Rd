\name{optionData}
\alias{optionData}
\docType{data}
\title{
  Option Data
}
\description{
  Closing prices of DAX index options as of 2012-02-10.
}
\usage{optionData}
\format{
  \code{optionData} is a list with six components:
  \describe{
    \item{\code{pricesCall}}{a matrix of size 124 times 10. The rows
      are the strikes; each column belongs to one expiry date.}
    \item{\code{pricesPut}}{a matrix of size 124 times 10}
    \item{\code{index}}{The DAX index (spot).}
    \item{\code{future}}{The available future settlement prices.}
    \item{\code{Euribor}}{Euribor rates.}
    \item{\code{NSSpar}}{Paramaters for German government bond yields,
      as estimated by the Bundesbank.}
  }
}
\details{
  Settlement prices for EUREX options are computed at 17:30, Frankfurt
  Time, even though trading continues until 22:00.
}
\source{
  The data was obtained from several websites: close prices of EUREX
  products were collected from \url{https://www.eurexchange.com} ; Euribor rates
  and the parameters of the Nelson-Siegel-Svensson can be found at
  \url{https://www.bundesbank.de} .
}
\references{
  Gilli, M., Maringer, D. and Schumann, E. (2019) \emph{Numerical
  Methods and Optimization in Finance}. 2nd edition. Elsevier.
  \url{https://www.elsevier.com/books/numerical-methods-and-optimization-in-finance/gilli/978-0-12-815065-8}

  Schumann, E. (2019) Financial Optimisation with R (\pkg{NMOF} Manual).
  \url{http://enricoschumann.net/NMOF.htm#NMOFmanual}
}
\examples{
str(optionData)
NSS(optionData$NSSpar, 1:10)
}
\keyword{datasets}
