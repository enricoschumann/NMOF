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
nc <- 25L          ## number of columns in data set
nr <- 5L           ## number of rows in data set
howManyCols <- 5L  ## length of true solution
X <- array(runif(nr*nc), dim = c(nr, nc))
xTRUE <- sample(1L:nc, howManyCols)
Xt <- X[ , xTRUE, drop = FALSE]
y <- rowSums(Xt)

## a random solution x0 ...
makeRandomSol <- function(nc) {
    ii <- sample.int(nc, sample.int(nc, 1L))
    x0 <- logical(nc); x0[ii] <- TRUE
    x0
}
x0 <- makeRandomSol(nc)

## ... but probably not a good one
sum(y - rowSums(X[ , x0, drop = FALSE]))
sum(y - rowSums(X[ , xTRUE, drop = FALSE]))

## a neighbourhood function: switch n elements in solution
neighbour <- function(xc, Data) {
    xn <- xc
    p <- sample.int(Data$nc, Data$n)
    xn[p] <- !xn[p]
    if (sum(xn) < 1L)
        xn <- xc
    xn
}

## a greedy neighbourhood function
neighbourG <- function(xc, Data) {
    of <- function(x)
        abs(sum(Data$y - rowSums(Data$X[ ,x, drop = FALSE])))
    xbest <- xc
    Fxbest <- of(xbest)
    for (i in 1L:Data$nc) {
        xn <- xc; p <- i
        xn[p] <- !xn[p]
        if (sum(xn) >= 1L) {
            Fxn <- of(xn)
            if (Fxn < Fxbest) {
                xbest <- xn
                Fxbest <- Fxn
            }
        }
    }
    xbest
}

## an objective function
OF <- function(xn, Data)
    abs(sum(Data$y - rowSums(Data$X[ ,xn, drop = FALSE])))


## (1) GREEDY SEARCH
## note: this could be done in a simpler fashion, but the
##       redundancies/overhead here are small; and the example is to
##       show how LSopt can be used for such a search
Data <- list(X = X, y = y, nc = nc, nr = nr, n = 1L)
algo <- list(nS = 500L, neighbour = neighbourG, x0 = x0,
             printBar = FALSE, printDetail = FALSE)
solG <- LSopt(OF, algo = algo, Data = Data)

## after how many iterations did we stop?
iterG <- min(which(solG$Fmat[ ,2L] == solG$OFvalue))
solG$OFvalue  ## the true solution has OF-value 0

## (2) LOCAL SEARCH
algo$neighbour <- neighbour
solLS <- LSopt(OF, algo = algo, Data = Data)
iterLS <- min(which(solLS$Fmat[ ,2L] == solLS$OFvalue))
solLS$OFvalue  ## the true solution has OF-value 0

## (3) *Threshold Accepting*
algo$nT <- 10L
algo$nS <- ceiling(algo$nS/algo$nT)
algo$q <- 0.99
solTA <- TAopt(OF, algo = algo, Data = Data)
iterTA <- min(which(solTA$Fmat[ ,2L] == solTA$OFvalue))
solTA$OFvalue  ## the true solution has OF-value 0

## look at the solution
all <- sort(unique(c(which(solTA$xbest),
                     which(solLS$xbest),
                     which(solG$xbest),
                     xTRUE)))
ta <- ls <- greedy <- true <- character(length(all))
true[  match(xTRUE, all)] <- "o"
greedy[match(which(solG$xbest),  all)] <- "o"
ls[    match(which(solLS$xbest), all)] <- "o"
ta[    match(which(solTA$xbest), all)] <- "o"
data.frame(true = true, greedy = greedy, LS = ls , TA = ta,
           row.names=all)

## plot results
par(ylog = TRUE, mar = c(5,5,1,6), las = 1)
plot(solTA$Fmat[seq_len(iterTA) ,2L],type = "l", log = "y",
     ylim = c(1e-4,
              max(pretty(c(solG$Fmat,solLS$Fmat,solTA$Fmat)))),
     xlab = "iterations", ylab = "OF value", col = grey(0.5))
lines(cummin(solTA$Fmat[seq_len(iterTA), 2L]), type = "l")
lines(solG$Fmat[ seq_len(iterG),  2L], type = "p", col = "blue")
lines(solLS$Fmat[seq_len(iterLS), 2L], type = "l", col = "goldenrod3")
legend(x = "bottomleft",
       legend = c("TA best solution", "TA current solution",
                  "Greedy", "LS current/best solution"),
       lty = c(1,1,0,1),
       col = c("black",grey(0.5),"blue","goldenrod2"),
       pch = c(NA,NA,21,NA))
axis(4, at = c(solG$OFvalue, solLS$OFvalue, solTA$OFvalue),
        labels = NULL, las = 1)
lines(x = c(iterG, par()$usr[2L]), y = rep(solG$OFvalue,2),
      col = "blue", lty = 3)
lines(x = c(iterTA, par()$usr[2L]), y = rep(solTA$OFvalue,2),
      col = "black", lty = 3)
lines(x = c(iterLS, par()$usr[2L]), y = rep(solLS$OFvalue,2),
      col = "goldenrod3", lty = 3)
}
\keyword{optimize}
