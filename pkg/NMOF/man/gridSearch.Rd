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
    A function of the form \code{fun(x, \dots)}, with \code{x} being a numeric vector or a list.
  }
  \item{levels}{
    A list of levels for the arguments (see Examples).
  }
  \item{\dots}{
    Objects passed to \code{fun}.
  }
  \item{lower}{
    A numeric vector.
  }
  \item{upper}{
    A numeric vector.
  }
  \item{npar}{
    The number of parameters. Must be supplied if lower and upper are to be expaned; see Details.
  }
  \item{n}{
    The number of levels. Default is 5.
  }
  \item{printDetail}{
    Print information on the number of objective function evaluations.
  }
  \item{method}{
    can be \code{loop} (the default), \code{multicore}
    or \code{snow}. See Details.
  }
  \item{mc.control}{
    a list containing settings that will be passed to \code{mclapply} if
    \code{multicore} is \code{TRUE}. Must be a list with named
    elements. See the documentation of \code{mclapply}.
  }
  \item{cl}{default is \code{NULL}. If method \code{snow} is used, this must be a
    cluster object or an integer (the number of cores).}
  
  \item{keepNames}{
    should the names of \code{levels} be kept? \strong{Not implemented.}
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
  \code{method=="loop"} (the default).

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

  Schumann, E. (2011) Examples and Extensions for the \pkg{NMOF} Package. \url{http://ssrn.com/abstract=1886522}
}

\author{
  Enrico Schumann
}

\examples{
testFun <- function(x) x[1L] + x[2L]^2
sol <- gridSearch(fun = testFun, levels = list(1:2, c(2, 3, 5)))
sol$minfun
sol$minlevels

testFun2 <- function(x) sum(x^2)
sol <- gridSearch(fun = testFun2, lower = rep(1, 3), upper = c(2, 8, 4), n = 5L)
sol$minfun
sol$minlevels
}
\keyword{optimize}
