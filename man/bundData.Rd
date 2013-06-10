\name{bundData}
\alias{bundData}
\docType{data}
\title{
  German Government Bond Data
}
\description{
  A sample of data on 44 German government bonds. Contains
  \acronym{ISIN}, coupon, maturity and dirty price as of 2010-05-31.
}
\usage{bundData}
\format{
  \code{bundData} is a list with three components: \code{cfList},
  \code{tmList} and \code{bM}.
  \code{cfList} is list of 44 numeric vectors (the cash
  flows). \code{tmList} is a list of 44 character vectors (the payment dates)
  formatted as \acronym{YYYY}-\acronym{MM}-\acronym{DD}. \code{bM} is a
  numeric vector with 44 elements (the dirty prices of the bonds).
}
\details{
  All prices are as of 31 May 2010. See chapter 14 in Gilli et al. (2011).
}
\source{
  The data was obtained from \url{http://www.deutsche-finanzagentur.de}
  . The data is also freely available from the website of the Bundesbank
  \url{http://www.bundesbank.de} .
}
\references{

  Gilli, M., Maringer, D. and Schumann, E. (2011) \emph{Numerical
  Methods and Optimization in Finance}. Elsevier.
  \url{http://www.elsevierdirect.com/product.jsp?isbn=9780123756626}

  Schumann, E. (2013) The \pkg{NMOF} Manual.
  \url{http://enricoschumann.net/NMOF.htm}

}
\examples{
bundData
str(bundData)

## get ISINs of bonds
names(bundData$cfList)

## get a specific bond
thisBond <- "DE0001135358"
data.frame(dates = as.Date(bundData$tmList[[thisBond]]),
           payments = bundData$cfList[[thisBond]])
}
\keyword{datasets}
