\name{NMOF-package}
\alias{NMOF-package}
\alias{NMOF}
\docType{package}
\title{Numerical Methods and Optimization in Finance}
\description{
  Functions, data and other \R code from the book \sQuote{Numerical
    Methods and Optimization in Finance}. Many functions are still
  experimental; interfaces may change in the future; there is little error
  handling. Comments/corrections/remarks/suggestions are very welcome
  (please contact the maintainer directly).
}
\details{
  The package contains implementations of several optimisation heuristics:
  Differential Evolution (\code{\link{DEopt}}), Genetic Algorithms
  (\code{\link{GAopt}}), (Stochastic) Local Search (\code{\link{LSopt}}),
  Particle Swarm (\code{\link{PSopt}}) and Threshold Accepting
  (\code{\link{TAopt}}). The term heuristic is understood here as a
  general purpose optimisation method.

  Dependencies: The package is completely written in \R. A number of
  packages are \emph{suggested}; they are not necessary to use the
  \pkg{NMOF} package. Package \pkg{MASS} is required to run the complete
  example for \code{\link{PSopt}}, and also in one of the vignettes
  (\code{PSlms}); \pkg{multicore} and \pkg{snow} are optional for
  functions \code{\link{bracketing}}, \code{\link{GAopt}},
  \code{\link{gridSearch}} and \code{\link{restartOpt}}, and they may
  become an option for other functions; \pkg{quadprog} is needed for a
  vignette (\code{TAportfolio}) and some tests; \pkg{RUnit} is needed to
  run the tests in subdirectory \sQuote{\code{unitTests}}.

  Version numbering: minor version numbers (like in \code{x.1-x}) are
  incremented when a feature is added or an existing feature is
  substantially revised. The patch level (\code{x.x-1}) is incremented
  with any published change.

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

## overview
packageDescription("NMOF")
help(package = "NMOF")

## code from the book
showExample("equations.R")
showExample("exampleLS.R", chapter = 13)

## show NEWS file
news(Version >= "0.23-0", package = "NMOF")

## vignettes
vignette(package = "NMOF")
nss <- vignette("DEnss", package = "NMOF")
print(nss)
edit(nss)

## book website
browseURL("http://nmof.net")

## more examples
file.show(system.file("NMOFex/README",   package = "NMOF"))
file.show(system.file("NMOFex/NMOFman.R", package = "NMOF"))}
}
