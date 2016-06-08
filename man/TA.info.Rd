\name{TA.info}
\alias{TA.info}
\title{
  Threshold-Accepting Information
}
\description{
  The function can be called from the objective and neighbourhood
  function during a run of \code{\link{TAopt}}; it provides information
  such as the current iteration, the current solution, etc.
}
\usage{
  TA.info(n = 0L)
}
\arguments{
  \item{n}{
    generational offset; see Details.
  }
}
\details{

  \bold{This function is still experimental.}

  The function can be called in the neighbourhood function or the
  objective function during a run of \code{\link{TAopt}}.  It evaluates
  to a list with the state of the optimisation run, such as the current
  iteration.

  \code{TA.info} relies on \code{\link{parent.frame}} to retrieve its
  information.  If the function is called within another function in the
  neighbourhood or objective function, the argument \code{n} needs to be
  increased.

}
\value{
A list
\item{step}{current step}
\item{threshold}{current threshold (the number, not the value)}
\item{iteration}{current iteration}
\item{xbest}{the best solution found so far}
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
  \code{\link{TAopt}}
}
\examples{
### MINIMAL EXAMPLE for TAopt

## objective function evaluates to a constant
fun <- function(x)
    0

## neighbourhood function does not even change the solution,
## but it reports information
nb <- function(x) {
    tmp <- TA.info()
    cat("current threshold ",   tmp$threshold,
        "| current step ",      tmp$step,
        "| current iteration ", tmp$iteration, "\n")
    x
}

## run TA
algo <- list(nS = 5,
             nT = 2,
             nD = 10,
             x0 = rep(0, 5),
             neighbour = nb,
             printBar = FALSE)
ignore <- TAopt(fun, algo)
}
