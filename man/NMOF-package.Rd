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
  (\code{\link{LSopt}}), Particle Swarm (\code{\link{PSopt}}),
  Simuleated Annealing (\code{\link{SAopt}}) and
  Threshold Accepting (\code{\link{TAopt}}). The term heuristic is meant
  in the sense of general-purpose optimisation method.

  Dependencies: The package is completely written in \R. A
  number of packages are \emph{suggested}, but they are not
  strictly required when using the \pkg{NMOF} package, and
  most of the package's functionality is available without
  them.  Specifically, package \pkg{MASS} is needed to run
  the complete example for \code{\link{PSopt}} and also in
  one of the vignettes (\code{PSlms}).  Package
  \pkg{parallel} is optional for functions
  \code{\link{bracketing}}, \code{\link{GAopt}},
  \code{\link{gridSearch}} and \code{\link{restartOpt}}, and
  may become an option for other functions.  Package
  \pkg{quadprog} is needed for a vignette
  (\code{TAportfolio}), some tests, and it may be used for
  computing mean-variance efficient portfolios.
  Package \pkg{Rglpk} is needed for function \code{\link{minCVaR}}.
  Package \pkg{readxl} is needed to process the raw data in function
  \code{\link{Shiller}}; package \pkg{datetimeutils} is
  used by \code{\link{French}} and \code{\link{Shiller}}.
  \pkg{PMwR} would be needed to run the examples
  of the backtesting examples in the NMOF book.
  Finally, packages \pkg{RUnit} and \pkg{tinytest} are needed to
  run the tests in subdirectory \sQuote{\code{unitTests}}.

  Version numbering: package versions are numbered in the
  form \code{major-minor-patch}.  The \emph{patch} level is
  incremented with any published change in a version.
  \emph{Minor} version numbers are incremented when a
  feature is added or an existing feature is substantially
  revised. (Such changes will be reported in the NEWS file.)
  The \emph{major} version number will only be increased if
  there were a new edition of the book.

  The source code of the \pkg{NMOF} package is also hosted at
  \url{https://github.com/enricoschumann/NMOF/}.
  Updates to the package and new features are described at
  \url{https://enricoschumann.net/notes/NMOF/}.

  \subsection{Optimisation}{

    There are functions for
    Differential Evolution (\code{\link{DEopt}}),
    Genetic Algorithms (\code{\link{GAopt}}),
    (Stochastic) Local Search (\code{\link{LSopt}}),
    Simuleated Annealing (\code{\link{SAopt}}),
    Particle Swarm (\code{\link{SAopt}}),
    and Threshold Accepting (\code{\link{TAopt}}).
    The function \code{\link{restartOpt}} helps with
    running restarts of these methods;
    also available are functions for
    grid search (\code{\link{gridSearch}}) and
    greedy search (\code{\link{greedySearch}}).

  }

  \subsection{Pricing Financial Instruments}{

    For options: See \code{\link{vanillaOptionEuropean}},
    \code{\link{vanillaOptionAmerican}}, \code{\link{putCallParity}}.
    For pricing methods that use the characteristic function, see
    \code{\link{callCF}}.

    For bonds and bond futures: See \code{\link{vanillaBond}},
    \code{\link{bundFuture}} and \code{\link{xtContractValue}}.

  }

  \subsection{Simulation}{

    See \code{\link{resampleC}} and \code{\link{mc}}.

  }

  \subsection{Data}{

    See \code{\link{bundData}}, \code{\link{fundData}} and
    \code{\link{optionData}}.

  }
}
\author{Enrico Schumann

  Maintainer: Enrico Schumann <es@enricoschumann.net>
}
\references{
  Gilli, M., Maringer, D. and Schumann, E. (2019)
  \emph{Numerical Methods and Optimization in Finance}.
  2nd edition. Elsevier. \doi{10.1016/C2017-0-01621-X}

  Schumann, E. (2023) Financial Optimisation with R (\pkg{NMOF} Manual).
  \url{https://enricoschumann.net/NMOF.htm#NMOFmanual}
}
\keyword{package}
\examples{
\dontrun{
library("NMOF")

## overview
packageDescription("NMOF")
help(package = "NMOF")

## code from book
showExample("equations.R", edition = 1)
showExample("Heur")

## show NEWS file
news(Version >= "2.0-0", package = "NMOF")

## vignettes
vignette(package = "NMOF")
nss <- vignette("DEnss", package = "NMOF")
print(nss)
edit(nss)

## _book_ websites
browseURL("https://nmof.net")
browseURL("https://enricoschumann.net/NMOF/")

## _package_ websites
browseURL("https://enricoschumann.net/R/packages/NMOF/")
browseURL("https://cran.r-project.org/package=NMOF")
browseURL("https://git.sr.ht/~enricoschumann/NMOF")
browseURL("https://github.com/enricoschumann/NMOF")

## unit tests
file.show(system.file("unitTests/test_results.txt", package = "NMOF"))
}

test.rep <- readLines(system.file("unitTests/test_results.txt",
                                  package = "NMOF"))
nt <- gsub(".*\\\\(([0-9]+) checks?\\\\).*", "\\\\1",
           test.rep[grep("\\\\(\\\\d+ checks?\\\\)", test.rep)])
message("Number of unit tests: ", sum(as.numeric(nt)))
}
