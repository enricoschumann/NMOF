\name{SA.info}
\alias{SA.info}
\title{
  Simulated-Annealing Information
}
\description{
  The function can be called from the objective and neighbourhood
  function during a run of \code{\link{SAopt}}; it provides information
  such as the current iteration, the current solution, etc.
}
\usage{
  SA.info(n = 0L)
}
\arguments{
  \item{n}{
    generational offset; see Details.
  }
}
\details{

  \bold{This function is still experimental.}

  The function can be called in the neighbourhood function or the
  objective function during a run of \code{\link{SAopt}}.  It evaluates
  to a list with information about the state of the optimisation run,
  such as the current iteration or the currently best solution.

  \code{SA.info} relies on \code{\link{parent.frame}} to retrieve its
  information.  If the function is called within another function within
  the neighbourhood or objective function, the argument \code{n} needs
  to be increased.

}
\value{
A list
\item{calibration}{logical: whether the algorithm is calibrating the
  acceptance probability}
\item{iteration}{current iteration}
\item{step}{current step for the given temperature level}
\item{temperature}{current temperature (the number, not the value)}
\item{xbest}{the best solution found so far}
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
  \code{\link{SAopt}}, \code{\link{TA.info}}
}
\examples{
### MINIMAL EXAMPLE for SAopt

## the objective function evaluates to a constant
fun <- function(x)
    0

## the neighbourhood function does not even change
## the solution; it only reports information
nb <- function(x) {
    info <- SA.info()
    cat("current step ",        info$step,
        "| current iteration ", info$iteration, "\n")
    x
}

## run SA
algo <- list(nS = 5, nT = 2, nD = 10,
             initT = 1,
             x0 = rep(0, 5),
             neighbour = nb,
             printBar = FALSE)
ignore <- SAopt(fun, algo)
}
