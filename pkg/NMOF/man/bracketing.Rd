\name{bracketing}
\alias{bracketing}
\title{
Zero-Bracketing
}
\description{
Bracket the zeros (roots) of a univariate function
}
\usage{
bracketing(fun, interval, ...,
           lower = min(interval), upper = max(interval),
           n = 20L,
           method = c("loop", "vectorised", "multicore", "snow"),
           mc.control = list(), cl = NULL)
}
\arguments{
  \item{fun}{
    a univariate function; it will be called as \code{fun(x, \dots)} with
    \code{x} being a numeric vector}
  \item{interval}{
    a numeric vector, containing the end-points of the interval to be searched
  }
  \item{\dots}{
    further arguments passed to \code{fun}
  }
  \item{lower}{
    lower end-point. Ignored if \code{interval} is specified.
  }
  \item{upper}{
    upper end-point. Ignored if \code{interval} is specified.
  }
  \item{n}{
    the number of function evaluations. Must be at least 2 (in which
    case \code{fun} is evaluated only at the end-points); defaults to 20.
  }
  \item{method}{
    can be \code{loop} (the default), \code{vectorised}, \code{multicore}
    or \code{snow}. See Details.
  }
  \item{mc.control}{
    a list containing settings that will be passed to \code{mclapply} if
    \code{method} is \code{multicore}. Must be a list of named
  elements. See the documentation of \code{mclapply} in package \pkg{multicore}.
}
  \item{cl}{default is \code{NULL}. If \code{method} is \code{snow}, this must be a
cluster object or an integer (the number of cores to be used). See the
documentation of package \pkg{snow}.}
}
\details{
  \code{bracketing} evaluates \code{fun} at equal-spaced values of \code{x}
  between (and including) \code{lower} and \code{upper}. If the sign of
  \code{fun} changes between two consecutive \code{x}-values,
  \code{bracketing} reports these two \code{x}-values as containing (\sQuote{bracketing})
  a root. There is no guarantee that there is only one root
  within a reported interval. \code{bracketing} will not narrow the chosen intervals.

  The argument \code{method} determines how \code{fun} is
  evaluated. Default is \code{loop}. If \code{method == "vectorised"},
  \code{fun} must be written such that it can be evaluated for a vector
  \code{x} (see Examples). If \code{method == "multicore"}, function
  \code{mclapply} from package \pkg{multicore} is used. Further settings
  for \code{mclapply} can be passed through the list
  \code{mc.control}. If \code{multicore} is chosen but the package is
  not available (eg, currently on Windows), then \code{method} will be
  set to \code{loop} and a warning is issued. If \code{method ==
  "snow"}, function \code{clusterApply} from package \pkg{snow} is
  used. In this case, the argument \code{cl} must either be a cluster
  object (see the documentation of \code{clusterApply}) or an
  integer. If an integer, a cluster will be set up via
  \code{makeCluster(c(rep("localhost", cl)), type = "SOCK")}, and
  \code{stopCluster} is called when the function is exited. If
  \code{snow} is chosen but the package is not available or \code{cl} is
  not specified, then \code{method} will be set to \code{loop} and a
  warning is issued. In case that \code{cl} is a cluster object,
  \code{stopCluster} will not be called automatically. 
}
\value{
  a numeric matrix with two columns. Each row contains one interval
  that contains at least one root. If no roots were found, the matrix
  has zero rows.
}
\references{
  Gilli, M., Maringer, D. and Schumann, E. (2011) \emph{Numerical
    Methods and Optimization in Finance}. Elsevier.
  \url{http://www.elsevierdirect.com/product.jsp?isbn=9780123756626}

  Schumann, E. (2011) Examples and Extensions for the \pkg{NMOF}
  Package. \url{http://enricoschumann.net/files/NMOFex.pdf}
}
\author{
  Enrico Schumann
}

\seealso{
  \code{\link{uniroot}} (in package \pkg{stats})
}
\examples{
## Gilli/Maringer/Schumann (2011), p. 290
testFun <- function(x) cos(1/x^2)

bracketing(testFun, interval = c(0.3, 0.9), n = 26L)
bracketing(testFun, interval = c(0.3, 0.9), n = 26L, method = "vectorised")
}
\keyword{optimize}
