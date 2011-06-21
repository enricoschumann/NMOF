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
other variables to be passed to the objective function and the neighbourhood function. See Details.
}
}
\details{
Local Search (\acronym{LS}) changes an initial solution for a number of times, accepting only such changes that lead to an improvement in 
solution quality (as measured by the objective function \code{OF}). More specifically, in each iteration, a current solution \code{xc} 
is changed through a function \code{algo$neighbour}. This function takes \code{xc} as an argument and returns a new solution \code{xn}. 
If \code{xn} is not worse than \code{xc}, ie, if \code{OF(xn,\dots)<=OF(xc,\dots)}, then \code{xn} replaces \code{xc}. 


The list \code{algo} contains the following items.
\describe{
\item{\code{nS}}{The number of steps. The default is 1000; but this setting depends heavily on the problem.}
\item{\code{x0}}{The initial solution. This can be a function; it will then be called once without arguments to compute an initial solution, ie, 
 \code{x0 <- algo$x0()}. This can be useful when the routine is called in a loop of restarts and each restart is to have its own starting value.}
\item{\code{neighbour}}{The neighbourhood function. It takes a solution \code{x} and returns a changed solution.}
\item{\code{printDetail}}{If \code{TRUE} (the default), information is printed.}
\item{\code{printBar}}{If \code{TRUE} (the default), information on progress is printed (\code{txtProgressBar} from the \pkg{utils} 
package is used).}
}

\acronym{LS} works on solutions through the functions \code{neighbour} and \code{OF} which are specified by the user. Thus, a solution 
need not be a numeric vector, but can be another data structure as well (eg, a list or a matrix).

}

\value{
Returns a list.
\item{\code{xbest}}{best solution found.}
\item{\code{OFvalue}}{objective function value associated with best solution.}
\item{\code{Fmat}}{a matrix with two columns. \code{Fmat[ ,1L]} contains the proposed solution over all iterations; 
\code{Fmat[ ,2L]} contains the accepted solutions.}
}
\references{
Gilli, M., Maringer, D. and Schumann, E. (2011) \emph{Numerical Methods and Optimization in Finance}. Elsevier. 
\url{http://www.elsevierdirect.com/product.jsp?isbn=9780123756626}
}
\author{
Enrico Schumann
}
\seealso{
\code{\link{TAopt}}
}
\examples{
## Aim: find the columns of X that, when summed, give y

# random data set
nc <- 25L; nr <- 5L; howManyCols <- 5L
X <- array(runif(nr*nc), dim = c(nr, nc))
xTRUE <- sample(1L:nc, howManyCols)
Xt <- X[ ,xTRUE]
y <- rowSums(Xt)

# a random solution x0
makeRandomSol <- function(nc) {
    ii <- sample.int(nc, sample.int(nc, 1L))
    x0 <- numeric(nc); x0[ii] <- 1L
    x0
}
x0 <- makeRandomSol(nc)

# ... but probably not the correct one
sum(y - rowSums(X[ ,as.logical(x0), drop = FALSE]))
sum(y - rowSums(X[ ,xTRUE]))


# a neighbourhood function: switch n elements in solution
neighbour <- function(xc, data) {
    xn <- xc
    p <- sample.int(data$nc, data$n)
    xn[p] <- abs(xn[p] - 1L)
    if (sum(xn) < 1L) xn <- xc 
    xn
}
# a greedy neighbourhood function
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

# an objective function
OF <- function(xn, data)
    abs(sum(data$y - rowSums(data$X[ ,as.logical(xn), drop = FALSE])))


# (1) *greedy search*
# note: this could be done in a simpler fashion. but the 
#       redundancies/overhead here are small; and the example is to
#       show how LSopt can be used for such a search
data <- list(X = X, y = y, nc = nc, nr = nr, n = 1L)
algo <- list(nS = 5000L, neighbour = neighbourG, x0 = x0,
             printBar = FALSE, printDetail = FALSE)

system.time(sol <- LSopt(OF, algo = algo, data = data))
sort(which(as.logical(sol$xbest)))
sort(xTRUE)
sol$OFvalue
plot(sol$Fmat[ ,2L],type = "l")

# (2) *Local Search*
algo$neighbour <- neighbour
algo$nS <- 5000L
system.time(sol <- LSopt(OF, algo = algo, data = data))
sort(which(as.logical(sol$xbest)))
sort(xTRUE)
sol$OFvalue
plot(sol$Fmat[ ,2L], type = "l")

# (3) *Threshold Accepting*
algo$nT <- 10L
algo$nS <- ceiling(algo$nS/algo$nT)
system.time(sol2 <- TAopt(OF, algo = algo, data = data))
sort(which(as.logical(sol2$xbest)))
sort(xTRUE)
sol2$OFvalue
plot(cummin(sol2$Fmat[ ,2L]),type = "l")
}
\keyword{optimize}
