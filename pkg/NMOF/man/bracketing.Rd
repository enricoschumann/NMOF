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
    method = c("loop", "vectorise", "multicore"),
    mc.control = list())
}
\arguments{
  \item{fun}{
    a univariate function; it will be called as \code{fun(x,\dots)}
}
  \item{interval}{
    a numeric vector
  }
  \item{\dots}{
    further arguments passed to \code{fun}
}
  \item{lower}{
    upper limit
  }
  \item{upper}{
    lower limit
  }
  \item{n}{
    the number of function evaluations. Must be at least 2 (end points).
  }
  \item{method}{
    can be \code{loop} (the default), \code{vectorise} or \code{multicore}. See Details.
  }
  \item{mc.control}{
  a list containing settings that will be passed to \code{mclapply} if
  \code{multicore} is \code{TRUE}. Must be a list with named
  elements. See the documentation of \code{mclapply}.
  }
}
\details{
  Bracketing evaluates \code{fun} at equal spaced values of \code{x}
  between (and including) \code{lower} and \code{upper}. If the sign of
  \code{fun} switches between two consecutive \code{x}-values,
  \code{bracketing} reports these two \code{x}-values as containing (`bracketing')
  a root. There is no guarantee that there is only one root
  within such an interval.
}
\value{
  a numeric matrix with two columns. Every row contains one interval
  that contains at least one root. If no roots where found, the matrix
  has zero rows.
}
\references{
  Gilli, M., Maringer, D. and Schumann, E. (2011) \emph{Numerical
    Methods and Optimization in Finance}. Elsevier.
  \url{http://www.elsevierdirect.com/product.jsp?isbn=9780123756626}

}
\author{
  Enrico Schumann
}

\seealso{
  \code{\link{uniroot}} (in package \pkg{stats})
}
\examples{
## Gilli/Maringer/Schumann, p. 290
testFun <- function(x) cos(1/x^2)

bracketing(testFun, interval = c(0.3, 0.9), n = 26L)
bracketing(testFun, interval = c(0.3, 0.9), n = 26L, method = "vectorise")

}
\keyword{optimize}
