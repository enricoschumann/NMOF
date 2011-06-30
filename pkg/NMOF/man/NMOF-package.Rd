\name{NMOF-package}
\alias{NMOF-package}
\alias{NMOF}
\docType{package}
\title{Numerical Methods and Optimization in Finance}
\description{
Functions, data and other \R code from the book \sQuote{Numerical Methods and Optimization in Finance}. Many functions are still experimental; 
interfaces may change in the future. There is little error handling. Comments/corrections/remarks/suggestions are very welcome 
(please contact the maintainer directly).
}
\details{
\tabular{ll}{
Package: \tab NMOF\cr
Type: \tab Package\cr
Version: \tab 0.13-1\cr
Date: \tab 2011-06-30\cr
Suggests: \tab MASS, quadprog, RUnit\cr
License: \tab GPL-3\cr
LazyLoad: \tab yes\cr
LazyData: \tab yes\cr
Classification/JEL: \tab C61, C63\cr
}
The package contains implementations of several general-purpose optimisation heuristics: Differential Evolution (\code{\link{DEopt}}), 
Genetic Algorithms (\code{\link{GAopt}}), (Stochastic) Local Search (\code{\link{LSopt}}), Particle Swarm (\code{\link{PSopt}}) and 
Threshold Accepting (\code{\link{TAopt}}).

}
\author{Enrico Schumann

Maintainer: Enrico Schumann <enricoschumann@yahoo.de>
}
\references{
Gilli, M., Maringer, D. and Schumann, E. (2011) \emph{Numerical Methods and Optimization in Finance}. Elsevier. 
\url{http://www.elsevierdirect.com/product.jsp?isbn=9780123756626}
}
\keyword{package}
\examples{
\dontrun{
require("NMOF")
help(package = "NMOF")
vignette(package = "NMOF")
nss <- vignette("DEnss")
print(nss)
edit(nss)}
}
