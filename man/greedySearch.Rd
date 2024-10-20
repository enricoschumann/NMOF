\name{greedySearch}
\alias{greedySearch}
\title{
  Greedy Search
}
\description{
  Greedy Search
}
\usage{
greedySearch(OF, algo, ...)
}
\arguments{
  \item{OF}{

    The objective function, to be minimised. Its first
    argument needs to be a solution; \code{\dots}
    arguments are also passed.

  }
  \item{algo}{

    List of settings. See Details.

  }
  \item{\dots}{

    Other variables to be passed to the objective
    function and to the neighbourhood function. See
    Details.

  }
}
\details{

  A greedy search works starts at a provided initial
  solution (called the current solution) and searches a
  defined neighbourhood for the best possible
  solution. If this best neighbour is not better than
  the current solution, the search stops.  Otherwise,
  the best neighbour becomes the current solution, and
  the search is repeated.


}
\value{

  A list:

  \item{\code{xbest}}{best solution found.}

  \item{\code{OFvalue}}{objective function value
    associated with best solution.}

  \item{\code{Fmat}}{a matrix with two
    columns. \code{Fmat[ ,1L]} contains the proposed
    solution over all iterations; \code{Fmat[ ,2L]}
    contains the accepted solutions.}

  \item{\code{xlist}}{a list}

  \item{\code{initial.state}}{the value of
    \code{\link{.Random.seed}} when the function was
    called.}

  \item{\code{x0}}{the initial solution}

  \item{\code{iterations}}{the number of iterations
    after which the search stopped}


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
  \code{LSopt}
}
\examples{
na <- 100
inc <- 5
R <- randomReturns(na = na,
                   ns = 1000,
                   sd = seq(0.01, 0.02, length.out = 100),
                   rho = 0.5)
S <- cov(R)
OF <- function(x, S, ...) {
    w <- 1/sum(x)
    sum(w * w * S[x, x])
}

x <- logical(na)
x[1:inc] <- TRUE


all.neighbours <- function(x, ...) {
    true  <- which( x)
    false <- which(!x)
    ans <- list()
    for (i in true) {
        for (j in false) {
            ans1 <- x
            ans1[i] <- !x[i]
            ans1[j] <- !x[j]
            ans <- c(ans, list(ans1))
        }
    }
    ans
}

algo <- list(loopOF = TRUE,
             maxit = 1000,
             all.neighbours = all.neighbours,
             x0 = x)

system.time(sol.gs <- greedySearch(OF, algo = algo, S = S))
sqrt(sol.gs$OFvalue)
}
