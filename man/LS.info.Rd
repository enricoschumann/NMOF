\name{LS.info}
\alias{LS.info}
\title{
  Local-Search Information
}
\description{
  The function can be called from the objective and neighbourhood
  function during a run of \code{\link{LSopt}}; it provides information
  such as the current iteration.
}
\usage{
  LS.info(n = 0L)
}
\arguments{
  \item{n}{
    generational offset; see Details.
  }
}
\details{

  \bold{This function is still experimental.}

  The function can be called in the neighbourhood function or the
  objective function during a run of \code{\link{LSopt}}.  It evaluates
  to a list with the state of the optimisation run, such as the current
  iteration.

  \code{LS.info} relies on \code{\link{parent.frame}} to retrieve its
  information.  If the function is called within another function in the
  neighbourhood or objective function, the argument \code{n} needs to be
  increased.

}
\value{
A list
\item{iteration}{current iteration}
\item{step}{same as \sQuote{iteration}}
}
\references{
  Gilli, M., Maringer, D. and Schumann, E. (2019) \emph{Numerical
  Methods and Optimization in Finance}. 2nd edition. Elsevier.
  \url{https://www.elsevier.com/books/numerical-methods-and-optimization-in-finance/gilli/978-0-12-815065-8}

  Schumann, E. (2019) Financial Optimisation with R (\pkg{NMOF} Manual).
  \url{http://enricoschumann.net/NMOF.htm#NMOFmanual}
}
\author{
    Enrico Schumann
}
\seealso{
  \code{\link{LSopt}}, \code{\link{TA.info}}
}
\examples{
## MINIMAL EXAMPLE for LSopt

## objective function evaluates to a constant
fun <- function(x)
    0

## neighbourhood function does not even change the solution,
## but it reports information
nb <- function(x) {
    tmp <- LS.info()
    cat("current iteration ", tmp$iteration, "\n")
    x
}

## run LS
algo <- list(nS = 5,
             x0 = rep(0, 5),
             neighbour = nb,
             printBar = FALSE)
ignore <- LSopt(fun, algo)

}
