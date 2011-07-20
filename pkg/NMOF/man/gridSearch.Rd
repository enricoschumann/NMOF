\name{gridSearch}
\alias{gridSearch}
\title{
Grid Search
}
\description{
Evaluate a function for a given list of possible arguments.
}
\usage{
gridSearch(fun, levels, ..., lower, upper, npar, n = 5L, 
    printDetail = TRUE)
}
\arguments{
  \item{fun}{
A function of the form \code{fun(x, \dots)}, with \code{x} being a numeric vector.
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
Give information on progress. \strong{Not implemented.}
}  
}
\details{
A grid search can be used to find \sQuote{good} parameter values for a function. In principle, a grid search has an obvious deficiency: as the 
length of \code{x} (the first argument to \code{fun}) increases, the number of necessary function evaluations grows exponentially. Note that 
\code{gridSearch} will not warn about an unreasonable number of function evaluations, but if \code{printDetail} is \code{TRUE} it will print 
information on its workload. In practice, grid search is much better than its reputation.

This function is currently very experimental, and may change substantially in the near term. 
The function uses \code{\link{lapply}} to evaluate \code{fun}; this can optionally be replaced with 
\code{mclapply} from the \pkg{multicore} package.

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

sol <- gridSearch(fun = testFun, levels = list(1:2,c(2,3,5)))
sol$minfun
sol$minlevels

sol <- gridSearch(fun=testFun, lower = rep(1, 3), upper = c(2,8,4), n = 5)
sol$minfun
sol$minlevels

}
\keyword{optimize}
