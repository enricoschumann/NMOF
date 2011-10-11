% version 2011-05-01
\name{NSf}
\alias{NSf}
\alias{NSSf}
\title{
Factor Loadings for Nelson--Siegel and Nelson--Siegel--Svensson
}
\description{
Computes the factor loadings for Nelson--Siegel (\acronym{NS}) and Nelson--Siegel--Svensson (\acronym{NSS}) model for given \code{lambda} values.
}
\usage{
NSf(lambda, tm)
NSSf(lambda1, lambda2, tm)
}
\arguments{
  \item{lambda}{the \eqn{\lambda}{lambda} parameter of the \acronym{NS} model (a scalar)
}
  \item{lambda1}{the \eqn{\lambda_1}{lambda1} parameter of the \acronym{NSS} model (a scalar)
}
  \item{lambda2}{the \eqn{\lambda_2}{lambda2} parameter of the \acronym{NSS} model (a scalar)
}
  \item{tm}{a numeric vector with times-to-payment/maturity
}
}
\details{
The function computes the factor loadings for given \eqn{\lambda}{lambda} parameters. Checking the correlation between these 
factor loadings can help to set reasonable \eqn{\lambda}{lambda} values for the \acronym{NS}/\acronym{NSS} models.
}
\value{For \acronym{NS}, a matrix with \code{length(tm)} rows and three columns. 
For \acronym{NSS}, a matrix with \code{length(tm)} rows and four columns.
}
\references{
Gilli, M. and Grosse, S. and Schumann, E. (2010) Calibrating the Nelson-Siegel-Svensson model, \acronym{COMISEF} Working Paper Series No. 031. 
\url{http://comisef.eu/files/wps031.pdf}

Gilli, M., Maringer, D. and Schumann, E. (2011) \emph{Numerical Methods and Optimization in Finance}. Elsevier. 
\url{http://www.elsevierdirect.com/product.jsp?isbn=9780123756626}

Gilli, M. and Schumann, E. (2010) A Note on \sQuote{Good} Starting Values in Numerical Optimisation, 
\acronym{COMISEF} Working Paper Series No. 044. \url{http://comisef.eu/files/wps044.pdf}

Nelson, C.R. and Siegel, A.F. (1987) Parsimonious Modeling of Yield Curves. \emph{Journal of Business}, \strong{60}(4), pp. 473--489.

Svensson, L.E. (1994) Estimating and Interpreting Forward Interest Rates: Sweden 1992--1994. \acronym{IMF} Working Paper 94/114.
}
\author{
Enrico Schumann
}

\seealso{
\code{\link{NS}}, \code{\link{NSS}}
}
\examples{
## Nelson-Siegel
cor(NSf(lambda = 6, tm = 1:10)[-1L, -1L])

## Nelson-Siegel-Svensson
cor(NSSf(lambda1 = 1, lambda2 = 5, tm = 1:10)[-1L, -1L])
cor(NSSf(lambda1 = 4, lambda2 = 9, tm = 1:10)[-1L, -1L])
}
