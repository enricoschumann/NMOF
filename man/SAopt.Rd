\name{SAopt}
\alias{SAopt}
\concept{heuristics}
\concept{Simluated Annealing}
\title{
  Optimisation with Simulated Annealing
}
\description{
  The function implements a Simulated-Annealing algorithm.
}
\usage{
SAopt(OF, algo = list(), \dots)
}
\arguments{

  \item{OF}{The objective function, to be minimised. Its first argument
    needs to be a solution \code{x}; it will be called as \code{OF(x,
      \dots)}.}
  \item{algo}{A list of settings for the algorithm. See
    Details.}
  \item{\dots}{other variables passed to \code{OF} and
    \code{algo$neighbour}. See Details.}

}
\details{

  Simulated Annealing (\acronym{SA}) changes an initial solution
  iteratively; the algorithm stops after a fixed number of
  iterations. Conceptually, \acronym{SA} consists of a loop than runs
  for a number of iterations. In each iteration, a current solution
  \code{xc} is changed through a function \code{algo$neighbour}. If this
  new (or neighbour) solution \code{xn} is not worse than \code{xc}, ie,
  if \code{OF(xn,\dots) <= OF(xc,\dots)}, then \code{xn} replaces
  \code{xc}. If \code{xn} is worse, it still replaces \code{xc}, but
  only with a certain probability. This probability is a function of the
  degree of the deterioration (the greater, the less likely the new
  solution is accepted) and the current iteration (the longer the
  algorithm has already run, the less likely the new
  solution is accepted).

  The list \code{algo} contains the following items.
  \describe{

    \item{\code{nS}}{The number of steps per temperature. The default is
      1000; but this setting depends very much on the problem.}

    \item{\code{nT}}{The number of temperatures. Default is 10.}

    \item{\code{nI}}{

      Total number of iterations, with default
      \code{NULL}. If specified, it will override
      \code{nS} with \code{ceiling(nI/nT)}. Using this
      option makes it easier to compare and switch
      between functions \code{\link{LSopt}},
      \code{\link{TAopt}} and \code{\link{SAopt}}.

    }

    \item{\code{nD}}{The number of random steps to calibrate the
      temperature. Defaults to 2000.}

    \item{\code{initT}}{Initial temperature. Defaults to \code{NULL}, in
    which case it is automatically chosen so that \code{initProb} is
    achieved.}

    \item{\code{finalT}}{Final temperature. Defaults to 0.}

    \item{\code{alpha}}{The cooling constant. The current temperature is
    multiplied by this value. Default is 0.9.}

  \item{\code{mStep}}{Step multiplier. The default is 1, which implies
    constant number of steps per temperature. If greater than 1, the step
    number \code{nS} is increased to \code{m*nS} (and rounded).}

    \item{\code{x0}}{The initial solution. If this is a function, it will
      be called once without arguments to compute an initial solution, ie,
      \code{x0 <- algo$x0()}. This can be useful when the routine is
      called in a loop of restarts, and each restart is to have its own
      starting value.}

    \item{\code{neighbour}}{The neighbourhood function, called as
      \code{neighbour(x, \dots)}. Its first argument must be a solution
      \code{x}; it must return a changed solution.}

    \item{\code{printDetail}}{If \code{TRUE} (the default), information
      is printed. If an integer \code{i} greater then one, information
      is printed at very \code{i}th iteration.}

    \item{\code{printBar}}{If \code{TRUE} (default is \code{FALSE}), a
      \code{\link{txtProgressBar}} (from package \pkg{utils}) is
      printed. The progress bar is not shown if \code{printDetail} is an
      integer greater than 1.}

    \item{\code{storeF}}{if \code{TRUE} (the default), the objective
      function values for every solution in every generation are stored
      and returned as matrix \code{Fmat}.}

    \item{\code{storeSolutions}}{Default is \code{FALSE}. If
      \code{TRUE}, the solutions (ie, decision variables) in every
      generation are stored and returned in list \code{xlist} (see Value
      section below). To check, for instance, the current solution at
      the end of the \code{i}th generation, retrieve \code{xlist[[c(2L,
      i)]]}.}

    \item{\code{classify}}{Logical; default is \code{FALSE}. If
      \code{TRUE}, the result will have a class attribute \code{SAopt}
      attached.}

    \item{\code{OF.target}}{Numeric; when specified, the algorithm will
      stop when an objective-function value as low as \code{OF.target} (or
      lower) is achieved. This is useful when an optimal
      objective-function value is known: the algorithm will then stop and
      not waste time searching for a better solution.}

  } % end describe

  At the minimum, \code{algo} needs to contain an initial solution
  \code{x0} and a \code{neighbour} function.

  The total number of iterations equals \code{algo$nT} times
   \code{algo$nS} (plus possibly \code{algo$nD}).

  } % end details
  \value{
    \code{SAopt} returns a list with five components:

    \item{\code{xbest}}{the solution}

    \item{\code{OFvalue}}{objective function value of the solution, ie,
      \code{OF(xbest, \dots)}}

    \item{\code{Fmat}}{if \code{algo$storeF} is \code{TRUE}, a matrix with
      one row for each iteration (excluding the initial \code{algo$nD}
      steps) and two columns. The first column contains the objective
      function values of the neighbour solution at a given iteration; the
      second column contains the value of the current solution. Since
      \acronym{SA} can walk away from locally-optimal solutions, the best
      solution can be monitored through \code{cummin(Fmat[ ,2L])}.}

    \item{\code{xlist}}{if \code{algo$storeSolutions} is \code{TRUE}, a
      list; else \code{NA}. Contains the neighbour solutions at a given
      iteration (\code{xn}) and the current solutions
      (\code{xc}). Example: \code{Fmat[i, 2L]} is the objective function
      value associated with \code{xlist[[c(2L, i)]]}.}

    \item{\code{initial.state}}{the value of \code{\link{.Random.seed}}
      when the function was called.}

    If \code{algo$classify} was set to \code{TRUE}, the resulting list
    will have a class attribute \code{TAopt}.

  }
  \note{If the \code{\dots} argument is used, then all the objects passed
    with \code{\dots} need to go into the objective function and the
    neighbourhood function. It is recommended to collect all information
    in a list \code{myList} and then write \code{OF} and \code{neighbour}
    so that they are called as \code{OF(x, myList)} and \code{neighbour(x,
      myList)}. Note that \code{x} need not be a vector but can be any data
    structure (eg, a \code{matrix} or a \code{list}).

    Using an initial and final temperature of zero means that
    \acronym{SA} will be equivalent to a Local Search. The function
    \code{\link{LSopt}} may be preferred then because of smaller
    overhead.

  }
\references{
  Gilli, M., Maringer, D. and Schumann, E. (2019) \emph{Numerical
  Methods and Optimization in Finance}. 2nd edition. Elsevier.
  \url{https://www.elsevier.com/books/numerical-methods-and-optimization-in-finance/gilli/978-0-12-815065-8}

  Kirkpatrick, S., Gelatt, C.D. and Vecchi, M.P. (1983). Optimization
  with Simulated Annealing. Science. \strong{220} (4598), 671--680.

  Schumann, E. (2019) Financial Optimisation with R (\pkg{NMOF} Manual).
  \url{http://enricoschumann.net/NMOF.htm#NMOFmanual}
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{LSopt}}, \code{\link{TAopt}}, \code{\link{restartOpt}}
}
\examples{
## Aim: given a matrix x with n rows and 2 columns,
##      divide the rows of x into two subsets such that
##      in one subset the columns are highly correlated,
##      and in the other lowly (negatively) correlated.
##      constraint: a single subset should have at least 40 rows

## create data with specified correlation
n <- 100L
rho <- 0.7
C <- matrix(rho, 2L, 2L); diag(C) <- 1
x <- matrix(rnorm(n * 2L), n, 2L) \%*\% chol(C)

## collect data
data <- list(x = x, n = n, nmin = 40L)

## a random initial solution
x0 <- runif(n) > 0.5

## a neighbourhood function
neighbour <- function(xc, data) {
    xn <- xc
    p <- sample.int(data$n, size = 1L)
    xn[p] <- abs(xn[p] - 1L)
    # reject infeasible solution
    c1 <- sum(xn) >= data$nmin
    c2 <- sum(xn) <= (data$n - data$nmin)
    if (c1 && c2) res <- xn else res <- xc
    as.logical(res)
}

## check (should be 1 FALSE and n-1 TRUE)
x0 == neighbour(x0, data)

## objective function
OF <- function(xc, data)
    -abs(cor(data$x[xc, ])[1L, 2L] - cor(data$x[!xc, ])[1L, 2L])

## check
OF(x0, data)
## check
OF(neighbour(x0, data), data)

## plot data
par(mfrow = c(1,3), bty = "n")
plot(data$x,
     xlim = c(-3,3), ylim = c(-3,3),
     main = "all data", col = "darkgreen")

## *Local Search*
algo <- list(nS = 3000L,
             neighbour = neighbour,
             x0 = x0,
             printBar = FALSE)
sol1 <- LSopt(OF, algo = algo, data=data)
sol1$OFvalue

## *Simulated Annealing*
algo$nT <- 10L
algo$nS <- ceiling(algo$nS/algo$nT)
sol <- SAopt(OF, algo = algo, data = data)
sol$OFvalue

c1 <- cor(data$x[ sol$xbest, ])[1L, 2L]
c2 <- cor(data$x[!sol$xbest, ])[1L, 2L]

lines(data$x[ sol$xbest, ], type = "p", col = "blue")

plot(data$x[ sol$xbest, ], col = "blue",
     xlim = c(-3, 3), ylim = c(-3, 3),
     main = paste("subset 1, corr.", format(c1, digits = 3)))

plot(data$x[!sol$xbest, ], col = "darkgreen",
     xlim = c(-3,3), ylim = c(-3,3),
     main = paste("subset 2, corr.", format(c2, digits = 3)))

## compare LS/SA
par(mfrow = c(1, 1), bty = "n")
plot(sol1$Fmat[ , 2L],type = "l", ylim=c(-1.5, 0.5),
     ylab = "OF", xlab = "Iterations")
lines(sol$Fmat[ , 2L],type = "l", col = "blue")
legend(x = "topright", legend = c("LS", "SA"),
       lty = 1, lwd = 2, col = c("black", "blue"))
}
\keyword{optimize}
