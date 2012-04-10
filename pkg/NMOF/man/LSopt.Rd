\name{LSopt}
\alias{LSopt}
\concept{Local Search}
\title{
  Stochastic Local Search
}
\description{
  Performs a simple stochastic local search.
}
\usage{
LSopt(OF, algo = list(), ...)
}
\arguments{
  \item{OF}{
    the objective function, to be minimised. Its first argument needs to be a solution; \code{\dots} arguments are also passed.
  }
  \item{algo}{
    list of settings. See Details.
  }
  \item{\dots}{
    other variables to be passed to the objective function and to the neighbourhood function. See Details.
  }
}
\details{
  
  Local Search (\acronym{LS}) changes an initial solution for a number
  of times, accepting only such changes that lead to an improvement in
  solution quality (as measured by the objective function
  \code{OF}). More specifically, in each iteration, a current solution
  \code{xc} is changed through a function \code{algo$neighbour}. This
  function takes \code{xc} as an argument and returns a new solution
  \code{xn}.  If \code{xn} is not worse than \code{xc}, ie, if
  \code{OF(xn,\dots)<=OF(xc,\dots)}, then \code{xn} replaces \code{xc}.

  The list \code{algo} contains the following items:

  \describe{

    \item{\code{nS}}{The number of steps. The default is 1000; but this
      setting depends very much on the problem.}

    \item{\code{x0}}{The initial solution. This can be a function; it
      will then be called once without arguments to compute an initial
      solution, ie, \code{x0 <- algo$x0()}. This can be useful when
      \code{LSopt} is called in a loop of restarts and each restart is
      to have its own starting value.}

    \item{\code{neighbour}}{The neighbourhood function, called as
      \code{neighbour(x, \dots)}. Its first argument must be a solution
      \code{x}; it must return a changed solution.}

    \item{\code{printDetail}}{If \code{TRUE} (the default), information
      is printed. If an integer \code{i} greater then one, information
      is printed at very \code{i}th step.}

    \item{\code{printBar}}{If \code{TRUE} (the default), a
      \code{txtProgressBar} (from package \pkg{utils}) is printed).}

    \item{\code{storeF}}{if \code{TRUE} (the default), the objective
      function values for every solution in every generation are stored
      and returned as matrix \code{Fmat}.}

    \item{\code{storeSolutions}}{default is \code{FALSE}. If
      \code{TRUE}, the solutions (ie, decision variables) in every
      generation are stored and returned in list
      \code{xlist} (see Value section below). To check, for instance,
      the current solution at the end of the \code{i}th generation, retrieve
      \code{xlist[[c(2L, i)]]}.} 
    }%describe
    \acronym{LS} works on solutions through the functions \code{neighbour}
    and \code{OF}, which are specified by the user. Thus, a solution need
    not be a numeric vector, but can be any other data structure as well
    (eg, a list or a matrix).

    To run silently (except for warnings and errors),
    \code{algo$printDetail} and \code{algo$printBar} must be \code{FALSE}.

  }

  \value{
    Returns a list.
    
    \item{\code{xbest}}{best solution found.}

    \item{\code{OFvalue}}{objective function value associated with best
      solution.}

    \item{\code{Fmat}}{a matrix with two columns. \code{Fmat[ ,1L]}
      contains the proposed solution over all iterations; \code{Fmat[
        ,2L]} contains the accepted solutions.}

    \item{\code{xlist}}{if \code{algo$storeSolutions} is \code{TRUE}, a
      list; else \code{NA}. Contains the neighbour solutions at a given
      iteration (\code{xn}) and the current solutions
      (\code{xc}). Example: \code{Fmat[i, 2L]} is the objective function
      value associated with \code{xlist[[c(2L, i)]]}.}

  }
  \references{
    Gilli, M., Maringer, D. and Schumann, E. (2011) \emph{Numerical
      Methods and Optimization in Finance}. Elsevier.
    \url{http://www.elsevierdirect.com/product.jsp?isbn=9780123756626}

    Schumann, E. (2011) Examples and Extensions for the \pkg{NMOF}
    Package. \url{http://enricoschumann.net/NMOF.htm}

  }
  \author{
    Enrico Schumann
  }
  \seealso{
    \code{\link{TAopt}}
  }
  \examples{
## Aim: find the columns of X that, when summed, give y

## random data set
nc <- 25L; nr <- 5L; howManyCols <- 5L
X <- array(runif(nr*nc), dim = c(nr, nc))
xTRUE <- sample(1L:nc, howManyCols)
Xt <- X[ , xTRUE, drop = FALSE]
y <- rowSums(Xt)

## a random solution x0 ...
makeRandomSol <- function(nc) {
    ii <- sample.int(nc, sample.int(nc, 1L))
    x0 <- numeric(nc); x0[ii] <- 1L
    x0
}
x0 <- makeRandomSol(nc)

## ... but probably not a good one
sum(y - rowSums(X[ , as.logical(x0), drop = FALSE]))
sum(y - rowSums(X[ , xTRUE, drop = FALSE]))


## a neighbourhood function: switch n elements in solution
neighbour <- function(xc, data) {
    xn <- xc
    p <- sample.int(data$nc, data$n)
    xn[p] <- abs(xn[p] - 1L)
    if (sum(xn) < 1L) xn <- xc 
    xn
}
## a greedy neighbourhood function
neighbourG <- function(xc, data) {
    of <- function(x)
        abs(sum(data$y - rowSums(data$X[,as.logical(x), drop = FALSE])))
    xbest <- xc
    Fxbest <- of(xbest)
    for (i in 1L:data$nc) {
        xn <- xc; p <- i
        xn[p] <- abs(xn[p] - 1L)
        if (sum(xn) > 1L) {
            Fxn <- of(xn) 
            if (Fxn <= Fxbest) {
                xbest <- xn; FXbest <- Fxn
            }
        }
    }
    xbest
}

## an objective function
OF <- function(xn, data)
    abs(sum(data$y - rowSums(data$X[ ,as.logical(xn), drop = FALSE])))


## (1) *greedy search*
## note: this could be done in a simpler fashion. but the 
##       redundancies/overhead here are small; and the example is to
##       show how LSopt can be used for such a search
data <- list(X = X, y = y, nc = nc, nr = nr, n = 1L)
algo <- list(nS = 500L, neighbour = neighbourG, x0 = x0,
             printBar = FALSE, printDetail = FALSE)

sol <- LSopt(OF, algo = algo, data = data)
sort(which(as.logical(sol$xbest)))
sort(xTRUE)
sol$OFvalue
par(ylog = TRUE)
plot(sol$Fmat[ ,2L],type = "l", log = "y", 
     ylim = c(1e-4, max(pretty(sol$Fmat[ ,2L]))), 
     xlab = "iterations", ylab = "OF value")

## (2) *Local Search*
algo$neighbour <- neighbour
sol <- LSopt(OF, algo = algo, data = data)
sort(which(as.logical(sol$xbest)))
sort(xTRUE)
sol$OFvalue
lines(sol$Fmat[ ,2L], type = "l", lty = 2)

## (3) *Threshold Accepting*
algo$nT <- 10L
algo$nS <- ceiling(algo$nS/algo$nT)
sol2 <- TAopt(OF, algo = algo, data = data)
sort(which(as.logical(sol2$xbest)))
sort(xTRUE)
sol2$OFvalue
lines(cummin(sol2$Fmat[ ,2L]),type = "l", lty = 3)
}
\keyword{optimize}
