\name{GAopt}
\alias{GAopt}
\title{
Optimisation with a Genetic Algorithm
}
\description{
The implements a simple Genetic Algorithm for minimising a function.
}
\usage{
GAopt(OF, algo = list(), ...)
}
\arguments{
  \item{OF}{The objective function, to be minimised. See Details.
}
  \item{algo}{A list with the settings for algorithm. See Details and Examples.
}
  \item{\dots}{Other pieces of data required to evaluate the objective function. See Details and examples.
}
}
\details{
The function implements a simple Genetic Algorithm (\acronym{GA}). A \acronym{GA} evolves a collection of solutions (the so-called population), 
all of which are coded as vectors containing only zeros and ones. (In \code{GAopt} solutions are of mode \code{logical}.) 
The algorithm starts with randomly-chosen or user-supplied population and aims to iteratively improve this population by randomly mixing 
solutions and by randomly switching single bits in solutions. In each iteration, such randomly-changed solutions are compared with the original 
population and better solutions replace inferior ones. Typically, the population size is kept constant.

\acronym{GA} language: iterations are often called generations; new solutions are called offspring or children (and the existing solutions, 
from which the children are created, are parents); the objective function is called a fitness function; mixing solutions 
is a crossover; and randomly changing them is called mutation. The choice which solutions remain in the population and which ones are discarded 
is called selection. In \code{GAopt} selection is pairwise: a given child is compared with a given parent; the better of the two is kept. 
In this way, the best solution is automatically retained in the population.

To allow for constraints, the evaluation works as follows: after new solutions are created, they are (i) repaired,
(ii) evaluated through the objective function, (iii) penalised. Step (ii) is done by a call to \code{OF}; steps (i) and (iii) by calls to 
\code{algo$repair} and \code{algo$pen}. Step (i) and (iii) are optional, so the respective functions default to \code{NULL}. A penalty 
can also be directly written in the \code{OF}, since it amounts to a positive number added to the \sQuote{clean} objective function value; but a 
separate function is often clearer. A separate penalty function is also advantagous if either only the objective function or only 
the penalty function can be vectorised.

Conceptually a \acronym{GA} consists of two loops: one loop across the generations and, in any given generation, one loop across the solutions. 
This is the default, controlled by the variables \code{algo$loopOF}, \code{algo$loopRepair} and \code{algo$loopPen}, which all 
default to \code{TRUE}. But it does not matter in what order the solutions are evaluated (or repaired or penalised), 
so the second loop can be vectorised. Examples are given in the vignettes and in the book. The respective 
\code{algo$loopFun} must then be set to \code{FALSE}.

All objects that are passed through \code{\dots} will be passed to the objective function, to the repair function and 
to the penalty function.

The list \code{algo} contains the following items.
\describe{
\item{\code{nB}}{number of bits per solution. Must be specified.}
\item{\code{nP}}{population size. Defaults to 50. Using default settings may not be a good idea.}
\item{\code{pen}}{a penalty function. Default is NULL (no penalty).}
\item{\code{mP}}{optional: the initial population. A logical matrix of size \code{length(algo$min)} times \code{algo$nP}, 
or a function that creates such a matrix. If a function, it should take no arguments. If \code{mode(mP)} is not \code{logical}, 
then \code{storage.mode(mP)} will be tried (and a warning will be issued).}
\item{\code{repair}}{a repair function. Default is NULL (no repairing).}
\item{\code{loopOF}}{logical. Should the OF be evaluated through a loop? Defaults to TRUE.}
\item{\code{loopPen}}{logical. Should the penalty function (if specified) be evaluated through a loop? Defaults to TRUE.}
\item{\code{loopRepair}}{logical. Should the repair function (if specified) be evaluated through a loop? Defaults to TRUE.}
\item{\code{printDetail}}{If \code{TRUE} (the default), information is printed.}
\item{\code{printBar}}{If \code{TRUE} (the default), information in progress is printed (\code{txtProgressBar} is used).}
}

}
\value{
A list:
\item{xbest}{the solution (the best member of the population).}
\item{OFvalue}{objective function value of best solution.}
\item{popF}{a vector. The objective function values in the population.}
\item{Fmat}{a matrix of size \code{algo$nG} times \code{algo$nP} containing the objective function values of all solutions over the generations.}
}
\references{
Gilli, M., Maringer, D. and Schumann, E. (2011) \emph{Numerical Methods and Optimization in Finance}. Elsevier. 
\url{http://www.elsevierdirect.com/product.jsp?isbn=9780123756626}
}
\author{
Enrico Schumann
}

\seealso{
\code{\link{DEopt}}, \code{\link{PSopt}}
}
\examples{
# (1) a *very* simple problem (why?):
#     match a binary (logical) string y
#
size <- 20L  # the length of the string
OF <- function(x, data) sum(x != y)
y <- runif(size) > 0.5
x <- runif(size) > 0.5
data <- list(y = y)
OF(y, data)  # the optimum value is zero
OF(x, data)
algo <- list(nB = size, nP = 20L, nG = 100L, prob = 0.002, 
    printBar = TRUE)
system.time(sol <- GAopt(OF, algo = algo, data = data))
# show differences (if any: marked by a dot)
cat(as.integer(y), "\n", as.integer(sol$xbest), "\n", 
    ifelse(y == sol$xbest , " ", "^"), "\n", sep = "")

algo$nP <- 3L  # that shouldn't work so well
sol2 <- GAopt(OF, algo = algo, data = data)
# show differences (if any: marked by a dot)
cat(as.integer(y), "\n", as.integer(sol2$xbest), "\n", 
    ifelse(y == sol2$xbest , " ", "^"), "\n", sep = "")


# (2) asset selection: find equal-weight portfolio; 
#     compare with LS and TA

# create random data with vols between minVol and maxVol 
# and pairwise correlation of 0.6
na <- 50L
C <- array(0.6, dim = c(na,na)); diag(C) <- 1
minVol <- 0.20; maxVol <- 0.40
Vols <- (maxVol - minVol) * runif(na) + minVol
Sigma <- outer(Vols,Vols) * C

# the objective function for LS/TA
OF <- function(x, data) {
    w <- x/sum(x)
    res <- crossprod(w[x], data$Sigma[x,x])
    res <- tcrossprod(w[x], res)
    res
}

# neighborhood function for LS/TA
neighbour <- function(xc, data) {
    xn <- xc
    p <- sample.int(data$na, data$nn, replace = FALSE)
    xn[p] <- !xn[p]
    # reject infeasible solution
    sumx <- sum(xn)
    if ( (sumx > data$Ksup) || (sumx < data$Kinf) )
        xc else xn
}

# data 
data <- list(Sigma = Sigma, # cov-matrix
              Kinf = 10L,   # min cardinality
              Ksup = 30L,   # max cardinality
                na = na,    # number of assets
                nn = 1L)    # assets to change per iteration

# a random solution x0
card0 <- sample(data$Kinf:data$Ksup, 1L, replace = FALSE) 
assets <- sample.int(na, card0, replace = FALSE)
x0 <- logical(na)
x0[assets] <- TRUE


# *Local Search*
algo <- list(x0 = x0, neighbour = neighbour, nS = 5000L)
system.time(solLS <- LSopt(OF, algo = algo, data = data))

# *Threshold Accepting*
algo$nT <- 10; algo$nS <- trunc(algo$nS/algo$nT); algo$q <- 0.2
system.time(solTA <- TAopt(OF, algo = algo, data = data))

# *Genetic Algorithm*
OF <- function(x, data) {
    res <- colSums(data$Sigma \%*\% x * x)
    n <- colSums(x); res <- res / n^2
    # penalise
    p <- pmax(data$Kinf - n, 0) + pmax(n - data$Ksup, 0)
    res + p
}
algo <- list(nB = na, nP = 100L, nG = 500L, prob = 0.002, 
printBar = TRUE, loopOF = FALSE)
system.time(solGA <- GAopt(OF = OF, algo = algo, data=data))

cat(
"Local Search        best solution ", format(sqrt(solLS$OFvalue), digits = 4), "\n", 
"Threshold Accepting best solution ", format(sqrt(solTA$OFvalue), digits = 4), "\n", 
"Genetic Algorithm   best solution ", format(sqrt(solGA$OFvalue), digits = 4), "\n", 
sep = "")

}
\keyword{optimize}

