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
\item{OF.sampling}{logical: if \code{TRUE}, is the algorithm sampling the objective
  function to compute thresholds; otherwise (i.e. during the actual
  optimisation) \code{FALSE}}
\item{iteration}{current iteration}
\item{step}{current step (i.e. for a given threshold)}
\item{threshold}{current threshold (the number, not the value)}
\item{xbest}{the best solution found so far}
\item{OF.xbest}{objective function value of best solution}
}
\references{
  Gilli, M., Maringer, D. and Schumann, E. (2019)
  \emph{Numerical Methods and Optimization in Finance}.
  2nd edition. Elsevier. \doi{10.1016/C2017-0-01621-X}

  Schumann, E. (2023) Financial Optimisation with R (\pkg{NMOF} Manual).
  \url{https://enricoschumann.net/NMOF.htm#NMOFmanual}
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
             nD = 3,
             x0 = rep(0, 5),
             neighbour = nb,
             printBar = FALSE,
             printDetail = FALSE)
ignore <- TAopt(fun, algo)

## printed output:
##    current threshold  NA | current step  1 | current iteration  NA
##    current threshold  NA | current step  2 | current iteration  NA
##    current threshold  NA | current step  3 | current iteration  NA
##    current threshold   1 | current step  1 | current iteration  1
##    current threshold   1 | current step  2 | current iteration  2
##    current threshold   1 | current step  3 | current iteration  3
##    current threshold   1 | current step  4 | current iteration  4
##    current threshold   1 | current step  5 | current iteration  5
##    current threshold   2 | current step  1 | current iteration  6
##    current threshold   2 | current step  2 | current iteration  7
##    current threshold   2 | current step  3 | current iteration  8
##    current threshold   2 | current step  4 | current iteration  9
##    current threshold   2 | current step  5 | current iteration  10

}
