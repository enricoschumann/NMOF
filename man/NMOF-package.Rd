\name{NMOF-package}
\alias{NMOF-package}
\alias{NMOF}
\docType{package}
\title{Numerical Methods and Optimization in Finance}
\description{

  Functions, data and other \R code from the book \sQuote{Numerical
  Methods and Optimization in Finance}.
  Comments/corrections/remarks/suggestions are very welcome (please
  contact the maintainer directly).

}
\details{

  The package contains implementations of several optimisation
  heuristics: Differential Evolution (\code{\link{DEopt}}), Genetic
  Algorithms (\code{\link{GAopt}}), (Stochastic) Local Search
  (\code{\link{LSopt}}), Particle Swarm (\code{\link{PSopt}}) and
  Threshold Accepting (\code{\link{TAopt}}). The term heuristic is meant
  in the sense of general purpose optimisation method.  
  
  Dependencies: The package is completely written in \R. A number of
  packages are \emph{suggested}, but they are not necessary to use the
  \pkg{NMOF} package.  More specifically, package \pkg{MASS} is needed
  to run the complete example for \code{\link{PSopt}} and also in one of
  the vignettes (\code{PSlms}).  Package \pkg{parallel} is optional for
  functions \code{\link{bracketing}}, \code{\link{GAopt}},
  \code{\link{gridSearch}} and \code{\link{restartOpt}}, and may become
  an option for other functions.  Package \pkg{quadprog} is needed for a
  vignette (\code{TAportfolio}) and some tests.  Finally, \pkg{RUnit} is
  needed to run the tests in subdirectory \sQuote{\code{unitTests}}.

  Version numbering: package versions are numbered in the form
  \code{major-minor-patch}.  Minor version numbers are incremented when
  a feature is added or an existing feature is substantially revised.
  The patch level is incremented with any published change.

  \subsection{Data}{

    See \code{\link{bundData}}, \code{\link{fundData}} and
    \code{\link{optionData}}.

  }

  \subsection{Optimisation}{
    
    There are functions for Differential Evolution (\code{\link{DEopt}}),
    Genetic Algorithms (\code{\link{GAopt}}), (Stochastic) Local Search
    (\code{\link{LSopt}}), Particle Swarm (\code{\link{PSopt}}) and
    Threshold Accepting (\code{\link{TAopt}}). 

  }
  
  \subsection{Option Pricing}{

    See \code{\link{vanillaOptionEuropean}},
    \code{\link{vanillaOptionAmerican}}, \code{\link{putCallParity}}.
    For pricing methods that use the characteristic function, see
    \code{\link{callCF}}.
    
  }
  
  \subsection{Simulation}{
    
    See \code{\link{resampleC}} and \code{\link{mc}}.
    
  }
}
\author{Enrico Schumann

  Maintainer: Enrico Schumann <es@enricoschumann.net>
}
\references{

  Gilli, M., Maringer, D. and Schumann, E. (2011) \emph{Numerical
  Methods and Optimization in Finance}. Elsevier.
  \url{http://www.elsevierdirect.com/product.jsp?isbn=9780123756626}

  Schumann, E. (2013) The \pkg{NMOF} Manual.
  \url{http://enricoschumann.net/NMOF.htm}

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
news(Version >= "0.30-0", package = "NMOF")

## vignettes
vignette(package = "NMOF")
nss <- vignette("DEnss", package = "NMOF")
print(nss)
edit(nss)

## book website
browseURL("http://nmof.net")
browseURL("http://enricoschumann.net/NMOF.htm")

## more examples
file.show(system.file("NMOFex/README",   package = "NMOF"))
file.show(system.file("NMOFex/NMOFman.R", package = "NMOF"))

## unit tests
file.show(system.file("unitTests/report.txt",   package = "NMOF"))
}
test.rep <- readLines(system.file("unitTests/report.txt",   package = "NMOF"))
nt <- gsub(".*\\\\(([0-9]+) checks?\\\\).*", "\\\\1",
           test.rep[grep("\\\\(\\\\d+ checks?\\\\)", test.rep)])
message("Number of unit tests: ", sum(as.numeric(nt)))

}
