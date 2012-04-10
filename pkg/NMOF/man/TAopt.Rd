\name{TAopt}
\alias{TAopt}
\concept{Threshold Accepting}
\title{
  Optimisation with Threshold Accepting
}
\description{
  The function implements the Threshold Accepting algorithm.
}
\usage{
TAopt(OF, algo = list(), \dots)
}
\arguments{
  \item{OF}{The objective function, to be minimised. Its first argument
    needs to be a solution \code{x}; it will be called as \code{OF(x, \dots)}.}
  \item{algo}{A list of settings for the algorithm. See Details.}
  \item{\dots}{other variables passed to \code{OF} and \code{algo$neighbour}. See Details.}
}
\details{

  Threshold Accepting (\acronym{TA}) changes an initial solution
  iteratively; the algorithm stops after a fixed number of
  iterations. Conceptually, \acronym{TA} consists of a loop than runs
  for a number of iterations. In each iteration, a current solution
  \code{xc} is changed through a function \code{algo$neighbour}. If this
  new (or neighbour) solution \code{xn} is not worse than \code{xc}, ie,
  if \code{OF(xn,\dots) <= OF(xc,\dots)}, then \code{xn} replaces
  \code{xc}. If \code{xn} is worse, it still replaces \code{xc} as long
  as the difference in \sQuote{quality} between the two solutions is
  less than a threshold \code{tau}; more precisely, as long as
  \code{OF(xn,\dots) - tau <= OF(xc,\dots)}. Thus, we also accept a new
  solution that is worse than its predecessor; just not too much
  worse. The threshold is typically decreased over the course of the
  optimisation. For zero thresholds TA becomes a stochastic local
  search.

  The thresholds can be passed through the list \code{algo} (see
  below). Otherwise, they are automatically computed through the
  procedure described in Gilli et al. (2006). When the thresholds are
  created automatically, the final threshold is always zero.


  The list \code{algo} contains the following items.
  \describe{

    \item{\code{nS}}{The number of steps per threshold. The default is
      1000; but this setting depends very much on the problem.}

    \item{\code{nT}}{The number of thresholds. Default is 10; ignored if
      \code{algo$vT} is specified.}

    \item{\code{nD}}{The number of random steps to compute the threshold
      sequence. Defaults to 2000. Only used if \code{algo$vT} is \code{NULL}.}

    \item{\code{q}}{The highest quantile for the threshold
      sequence. Defaults to 0.5. Only used if \code{algo$vT} is
      \code{NULL}. If \code{q} is zero, \code{TAopt} will run with
      zero-thresholds (ie, like a Local Search).}

    \item{\code{x0}}{The initial solution. If this is a function, it will
      be called once without arguments to compute an initial solution, ie,
      \code{x0 <- algo$x0()}. This can be useful when the routine is
      called in a loop of restarts, and each restart is to have its own
      starting value.}

    \item{\code{vT}}{The thresholds. A numeric vector. If \code{NULL} (the
      default), \code{TAopt} will compute \code{algo$nT} thresholds.
      Passing threshold can be useful when similar problems are
      handled. Then the time to sample the objective function to compute
      the thresholds can be saved (ie, we save \code{algo$nD} function
      evaluations). If the thresholds are computed and
      \code{algo$printDetail} is \code{TRUE}, the time required to
      evaluate the objective function will be measured and an estimate for
      the remaining computing time is issued. This estimate is often
      very crude.}

    \item{\code{neighbour}}{The neighbourhood function, called as
      \code{neighbour(x, \dots)}. Its first argument must be a solution
      \code{x}; it must return a changed
      solution.}

    \item{\code{printDetail}}{If \code{TRUE} (the default), information is
      printed. If an integer \code{i} greater then one, information is
      printed at very \code{i}th iteration.}

    \item{\code{printBar}}{If \code{TRUE} (default is \code{FALSE}), a
      \code{\link{txtProgressBar}} (from package \pkg{utils}) is
      printed. The progress bar is not shown if \code{printDetail} is
      \code{TRUE}.}

    \item{\code{scale}}{The thresholds are multiplied by
    \code{scale}. Default is 1.}

    \item{\code{stepUp}}{Defaults to \code{0}. If \code{stepUp > 0}, then
      the thresholds are recycled, ie, \code{vT <- rep(vT,
        algo$stepUp + 1)} (and the number of thresholds will be increased by
      \code{algo$nT * algo$stepUp}). This option works for supplied as well as
      computed thresholds. Practically, this will have the same effect as
      restarting from a returned
      solution. (In Simulated Annealing, this strategy
      goes by the name of \sQuote{reheating}.)}

    \item{\code{storeF}}{if \code{TRUE} (the default), the objective
      function values for every solution in every generation are stored
      and returned as matrix \code{Fmat}.}

    \item{\code{storeSolutions}}{default is \code{FALSE}. If
      \code{TRUE}, the solutions (ie, decision variables) in every
      generation are stored and returned in list
      \code{xlist} (see Value section below). To check, for instance,
      the current solution at the end of the \code{i}th generation, retrieve
      \code{xlist[[c(2L, i)]]}.}
      
    } % end describe
    The total number of iterations equals \code{algo$nT * (algo$stepUp+1) *
      algo$nS} (plus possibly \code{algo$nD}).
  } % end details
  \value{
    \code{TAopt} returns a list with four components:

    \item{\code{xbest}}{the solution}

    \item{\code{OFvalue}}{objective function value of the solution, ie,
      \code{OF(xbest, \dots)}}

    \item{\code{Fmat}}{if \code{algo$storeF} is \code{TRUE}, a matrix with
      one row for each iteration (excluding the initial \code{algo$nD}
      steps) and two columns. The first column contains the objective
      function values of the neighbour solution at a given iteration; the
      second column contains the value of the current solution. Since
      \acronym{TA} can walk away from locally-optimal solutions, the best
      solution can be monitored through \code{cummin(Fmat[ ,2L])}.}

    \item{\code{xlist}}{if \code{algo$storeSolutions} is \code{TRUE}, a
      list; else \code{NA}. Contains the neighbour solutions at a given
      iteration (\code{xn}) and the current solutions
      (\code{xc}). Example: \code{Fmat[i, 2L]} is the objective function
      value associated with \code{xlist[[c(2L, i)]]}.}

  }
  \note{If the \code{\dots} argument is used, then all the objects passed
    with \code{\dots} need to go into the objective function and the
    neighbourhood function. It is recommended to collect all information
    in a list \code{myList} and then write \code{OF} and \code{neighbour}
    so that they are called as \code{OF(x, myList)} and \code{neighbour(x,
      myList)}. Note that \code{x} need not be a vector but can be any data
    structure (eg, a \code{matrix} or a \code{list}).

    Using thresholds of size 0 makes \acronym{TA} run as a Local Search. The
    function \code{\link{LSopt}} may be preferred then because of smaller
    overhead.
  }
  \references{
    Dueck, G. and Scheuer, T. (1990) Threshold Accepting. A General Purpose Optimization Algorithm Superior to Simulated Annealing.
    \emph{Journal of Computational Physics}. \strong{90}, 1, pp. 161--175.

    Gilli, M., Kellezi, E. and Hysi, H. (2006) A Data-Driven Optimization Heuristic for Downside Risk Minimization.
    \emph{Journal of Risk}. \strong{8}, 3, pp. 1--18.

    Gilli, M., Maringer, D. and Schumann, E. (2011) \emph{Numerical Methods and Optimization in Finance}. Elsevier.
    \url{http://www.elsevierdirect.com/product.jsp?isbn=9780123756626}

    Moscato, P.  and Fontanari, J.F. (1990). Stochastic Versus Deterministic Update in Simulated Annealing.
    Physics Letters A. \strong{146}, 4, pp. 204--208.

    Schumann, E. (2011) Examples and Extensions for the \pkg{NMOF}
    Package. \url{http://enricoschumann.net/files/NMOFex.pdf}

    Winker, P. (2001). \emph{Optimization Heuristics in Econometrics: Applications of Threshold Accepting}. Wiley.

  }
  \author{
    Enrico Schumann
  }

  \seealso{
    \code{\link{LSopt}}
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

## *Threshold Accepting*
algo$nT <- 10L
algo$nS <- ceiling(algo$nS/algo$nT)
sol <- TAopt(OF, algo = algo, data = data)
sol$OFvalue

c1 <- cor(data$x[ sol$xbest, ])[1L, 2L]
c2 <- cor(data$x[!sol$xbest, ])[1L, 2L]

lines(data$x[ sol$xbest, ], type = "p", col = "blue")

plot(data$x[ sol$xbest, ], col = "blue",
     xlim = c(-3,3), ylim = c(-3,3),
     main = paste("subset 1, corr.", format(c1, digits = 3)))

plot(data$x[!sol$xbest, ], col = "darkgreen",
     xlim = c(-3,3), ylim = c(-3,3),
     main = paste("subset 2, corr.", format(c2, digits = 3)))

## compare LS/TA
par(mfrow = c(1,1), bty = "n")
plot(sol1$Fmat[ ,2L],type="l", ylim=c(-1.5,0.5),
     ylab = "OF", xlab = "iterations")
lines(sol$Fmat[ ,2L],type = "l", col = "blue")
legend(x = "topright",legend = c("LS", "TA"),
       lty = 1, lwd = 2,col = c("black", "blue"))
}
\keyword{optimize}
