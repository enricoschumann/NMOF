\name{NMOF-package}
\alias{NMOF-package}
\alias{NMOF}
\docType{package}
\title{Numerical Methods and Optimization in Finance}
\description{
Functions and other \R code from the book \sQuote{Numerical Methods and Optimization in Finance}. The package serves 
as a convenient way to collect the book's code examples. Many functions are still experimental; 
interfaces may change in the future. There is little error handling. Comments/corrections/remarks/suggestions are very welcome 
(please contact the maintainer directly).
}
\details{
\tabular{ll}{
Package: \tab NMOF\cr
Type: \tab Package\cr
Version: \tab \Sexpr{packageDescription("NMOF", fields = "Version")}\cr
Date: \tab \Sexpr{packageDescription("NMOF", fields = "Date")}\cr
Suggests: \tab \Sexpr{packageDescription("NMOF", fields = "Suggests")}\cr
License: \tab GPL-3\cr
LazyLoad: \tab yes\cr
LazyData: \tab yes\cr
Classification/JEL: \tab C61, C63\cr
}
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
vignette(package = "NMOF")
nss <- vignette("DEnss")
print(nss)
edit(nss)}
}
