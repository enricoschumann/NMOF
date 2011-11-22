\name{gridSearch}
\alias{gridSearch}
\title{
  Grid Search
}
\description{
  Evaluate a function for a given list of possible arguments.
}
\usage{
gridSearch(fun, levels, ..., lower, upper, npar = 1L, n = 5L,
           printDetail = TRUE,
           method = c("loop", "multicore", "snow"),
           mc.control = list(), cl = NULL,
           keepNames = FALSE, asList = FALSE)
}
\arguments{
  \item{fun}{
    a function of the form \code{fun(x, \dots)}, with \code{x} being a
    numeric vector or a list of numeric vectors (of length one)
  }
  \item{levels}{
    a list of levels for the arguments (see Examples)
  }
  \item{\dots}{
    objects passed to \code{fun}
  }
  \item{lower}{
    a numeric vector. Ignored if levels are explicitly specified.
  }
  \item{upper}{
    a numeric vector. Ignored if levels are explicitly specified.
  }
  \item{npar}{
    the number of parameters. Must be supplied if \code{lower} and
    \code{upper} are to be expanded; see Details. Ignored when \code{levels} are
    explicitly specified, or when \code{lower}/\code{upper} are used and
    at least one has lenght greater than one. See Examples.
  }
  \item{n}{
    the number of levels. Default is 5. Ignored if levels are explicitly specified.
  }
  \item{printDetail}{
    print information on the number of objective function evaluations
  }
  \item{method}{
    can be \code{loop} (the default), \code{multicore}
    or \code{snow}. See Details.
  }
  \item{mc.control}{
    a list containing settings that will be passed to \code{mclapply} if
    \code{method} is \code{multicore}. Must be a list of named
    elements; see the documentation of \code{mclapply} in pkg{multicore}.
  }
  \item{cl}{default is \code{NULL}. If method \code{snow} is used, this must be a
    cluster object or an integer (the number of cores).}

  \item{keepNames}{
    \code{logical}: should the names of \code{levels} be kept?
  }
  \item{asList}{
    does \code{fun} expect a list? Default is \code{FALSE}.
  }

}
\details{
  A grid search can be used to find \sQuote{good} parameter values for a
  function. In principle, a grid search has an obvious deficiency: as the
  length of \code{x} (the first argument to \code{fun}) increases, the
  number of necessary function evaluations grows exponentially. Note that
  \code{gridSearch} will not warn about an unreasonable number of function
  evaluations, but if \code{printDetail} is \code{TRUE} it will print the
  required number of function evaluations.

  In practice, grid search is often better than its reputation. If a
  function takes only a few parameters, it is often a reasonable approach
  to find \sQuote{good} parameter values.

  The function uses the mechanism of \code{\link{expand.grid}} to create
  the list of parameter combinations for which \code{fun} is evaluated; it
  calls \code{\link{lapply}} to evaluate \code{fun} if
  \code{method == "loop"} (the default).

  If \code{method == "multicore"}, the function \code{mclapply} from
  package \pkg{multicore} is used. Further settings for \code{mclapply}
  can be passed through the list \code{mc.control}. If \code{multicore}
  is chosen but not available, then \code{method} will be set to
  \code{loop} and a warning is issued. If \code{method == "snow"}, the
  function \code{clusterApply} from package \pkg{snow} is used. In this
  case, the argument \code{cl} must either be a cluster object (see the
  documentation of \code{clusterApply}) or an integer. If an integer, a
  cluster will be set up via \code{makeCluster(c(rep("localhost", cl)),
    type = "SOCK")} (and \code{stopCluster} is called when the function is
  exited). If \code{snow} is chosen but not available or \code{cl} is
  not specified, then \code{method} will be set to \code{loop} and a
  warning is issued.
}
\value{
  A list.
  \item{minfun}{the minimum of \code{fun}.}
  \item{minlevels}{the levels that give this minimum.}
  \item{values}{a list. All the function values of \code{fun}.}
  \item{levels}{a list. All the levels for which \code{fun} was evaluated.}
}

\references{
  Gilli, M., Maringer, D. and Schumann, E. (2011) \emph{Numerical Methods and Optimization in Finance}. Elsevier.
  \url{http://www.elsevierdirect.com/product.jsp?isbn=9780123756626}

  Schumann, E. (2011) Examples and Extensions for the \pkg{NMOF}
  Package. \url{http://enricoschumann.net/files/NMOFex.pdf}
}

\author{
  Enrico Schumann
}

\examples{
testFun <- function(x) x[1L] + x[2L]^2
sol <- gridSearch(fun = testFun, levels = list(1:2, c(2, 3, 5)))
sol$minfun
sol$minlevels

## specify all levels
levels <- list(a = 1:2, b = 1:3)
res <- gridSearch(testFun, levels)
res$minfun
sol$minlevels

## specify lower, upper and npar
lower <- 1; upper <- 3; npar <- 2
res <- gridSearch(testFun, lower = lower, upper = upper, npar = npar)
res$minfun
sol$minlevels

## specify lower, upper, npar and n
lower <- 1; upper <- 3; npar <- 2; n <- 4
res <- gridSearch(testFun, lower = lower, upper = upper, npar = npar, n = n)
res$minfun
sol$minlevels

## specify lower, upper and n
lower <- c(1,1); upper <- c(3,3); n <- 4
res <- gridSearch(testFun, lower = lower, upper = upper, n = n)
res$minfun
sol$minlevels

## specify lower, upper (auto-expanded) and n
lower <- c(1,1); upper <- 3; n <- 4
res <- gridSearch(testFun, lower = lower, upper = upper, n = n)
res$minfun
sol$minlevels
}
\keyword{optimize}
