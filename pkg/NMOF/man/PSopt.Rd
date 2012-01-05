\name{PSopt}
\alias{PSopt}
\title{
Particle Swarm Optimisation
}
\description{
The function implements Particle Swarm Optimisation.
}
\usage{
PSopt(OF, algo = list(), ...)
}
\arguments{
  \item{OF}{
the objective function to be minimised. See Details.
}
  \item{algo}{
a list with the settings for algorithm. See Details and Examples.
}
  \item{\dots}{
pieces of data required to evaluate the objective function. See Details.
}
}
\details{
The function implements Particle Swarm Optimisation (\acronym{PS}); see the references for details on the implementation. \acronym{PS} is a 
population-based optimisation heuristic. It develops several solutions (a \sQuote{population}) over a number of iterations. \acronym{PS} 
is directly applicable to continuous problems since the population is stored in real-valued vectors. In each iteration, a solution is
updated by adding another vector called velocity. Think of a solution as a position in the search space, and of velocity as the direction into 
which this solution moves. Velocity changes over the course of the optimization: it is biased towards the best solution found by the particular
solution and the best overall solution. The algorithm stops after a fixed number of iterations.

To allow for constraints, the evaluation works as follows: after a new solution is created, it is (i) repaired,
(ii) evaluated through the objective function, (iii) penalised. Step (ii) is done by a call to \code{OF}; steps (i) and (iii) by calls to 
\code{algo$repair} and \code{algo$pen}. Step (i) and (iii) are optional, so the respective functions default to \code{NULL}. A penalty 
can also be directly written in the \code{OF}, since it amounts to a positive number added to the \sQuote{clean} objective function value. 
It can be advantageous to write a separate penalty function if either only the objective function or only the penalty function can be vectorised.
(Constraints can also be added without these mechanisms. Solutions that violate constraints can, for instance, be mapped to feasible solutions, 
but without actually changing them. See Maringer and Oyewumi, 2007, for an example with Differential Evolution.)

Conceptually, \acronym{PS} consists of two loops: one loop across the iterations and, in any given generation, one loop across the solutions. 
This is the default, controlled by the variables \code{algo$loopOF}, \code{algo$loopRepair} and \code{algo$loopPen}, which all 
default to \code{TRUE}. But it does not matter in what order the solutions are evaluated (or repaired or penalised), 
so the second loop can be vectorised. Examples are given in the vignettes and in the book. The respective 
\code{algo$loopFun} must then be set to \code{FALSE}.

The objective function \code{OF}, the repair function and and the
penalty function will be called as \code{fun(solution, \dots)}.

The list \code{algo} contains the following items:
\describe{
\item{\code{nP}}{population size. Defaults to 100. Using default settings may not be a good idea.}
\item{\code{nG}}{number of iterations. Defaults to 500. Using default settings may not be a good idea.}
\item{\code{c1}}{the weight towards the individual's best solution. Typically between 0 and 2; defaults to 1. 
Using default settings may not be a good idea. In some cases, even negative values work well: the solution is then driven off its 
past best position. For \sQuote{simple} problems, setting \code{c1} to zero may work well: the population moves then towards the best 
overall solution.}
\item{\code{c2}}{the weight towards the populations's best solution. Typically between 0 and 2; defaults to 1. 
Using default settings may not be a good idea. In some cases, even negative values work well: the solution is then driven off the population's 
past best position.}
\item{\code{iner}}{the inertia weight (a scalar), which reduces velocity. Typically between 0 and 1. Default is 0.9.}
\item{\code{initV}}{the standard deviation of the initial velocities. Defaults to 1.}
\item{\code{maxV}}{the maximum (absolute) velocity. Setting limits to velocity is sometimes called velocity clamping. Velocity is the 
change in a given solution in a given iteration. A maximum velocity can be set so to prevent unreasonable 
velocities (\sQuote{overshooting}): for instance, if a decision variable may lie between 0 and 1, then an absolute velocity much greater than 1 
makes rarely sense.}
\item{\code{min}}{a vector of minimum parameter values. See below.}
\item{\code{max}}{a vector of maximum parameter values. See below.}
\item{\code{pen}}{a penalty function. Default is \code{NULL} (no penalty).}
\item{\code{repair}}{a repair function. Default is \code{NULL} (no repairing).}
\item{\code{changeV}}{a function to change velocity. Default is \code{NULL} (no change). This function is called before
    the velocity is added to the current solutions; it can be used to impose restrictions like changing only a number of 
    decision variables.}
\item{\code{initP}}{optional: the initial population. A matrix of size \code{length(algo$min)} times \code{algo$nP}, or a function that creates 
such a matrix. If a function, it should take no arguments.}
\item{\code{loopOF}}{logical. Should the \code{OF} be evaluated through a loop? Defaults to \code{TRUE}.}
\item{\code{loopPen}}{logical. Should the penalty function (if specified) be evaluated through a loop? Defaults to \code{TRUE}.}
\item{\code{loopRepair}}{logical. Should the repair function (if specified) be evaluated through a loop? Defaults to \code{TRUE}.}
\item{\code{loopChangeV}}{logical. Should the \code{changeV} function (if specified) be evaluated through a loop? Defaults to \code{TRUE}.}
\item{\code{printDetail}}{If \code{TRUE} (the default), information is
  printed. If an integer \code{i} greater then one, information is
  printed at very \code{i}th iteration.}
\item{\code{printBar}}{If \code{TRUE} (the default), a
  \code{txtProgressBar} (from package \pkg{utils}) is printed).}
\item{\code{storeF}}{If \code{TRUE} (the default), the objective
  function values for every solution in every generation are stored and returned as matrix \code{Fmat}.}
\item{\code{storeSolutions}}{default
  is \code{FALSE}. If \code{TRUE}, the solutions (ie, decision
  variables) in every generation are stored as lists
  \code{P} and \code{Pbest}, both stored in the list \code{xlist} which the
  function returns. To check, for instance, the solutions at the end of
  the \code{i}th iteration, retrieve \code{xlist[[c(1L, i)]]}; the best
  solutions at the end of this iteration are in \code{xlist[[c(2L, i)]]}. \code{P[[i]]}/\code{Pbest[[i]]} will be
  a matrix of size \code{length(algo$min)} times \code{algo$nP}.}
} % end describe
The vectors \code{min} and \code{max} are used to determine the dimension of the problem and 
to randomly initialise the population. They are no constraints: a solution may well be outside these limits.
}
\value{
Returns a list:
\item{xbest}{the solution}
\item{OFvalue}{objective function value of best solution}
\item{popF}{a vector: the objective function values in the final population}
\item{Fmat}{if \code{algo$storeF} is \code{TRUE}, a matrix of size \code{algo$nG}
  times \code{algo$nP}. Each column contains the best objective function value found by the particular solution.}
\item{xlist}{if \code{algo$storeSolutions} is \code{TRUE}, a list that contains two
  lists \code{P} and \code{Pbest} of matrices; else \code{NA}.
}

}
\references{
Eberhart, R.C. and Kennedy, J. (1995) A New Optimizer using Particle Swarm theory. 
\emph{Proceedings of the Sixth International Symposium on Micromachine and Human Science}, pp. 39--43.

Gilli, M., Maringer, D. and Schumann, E. (2011) \emph{Numerical Methods and Optimization in Finance}. Elsevier. 
\url{http://www.elsevierdirect.com/product.jsp?isbn=9780123756626}

  Schumann, E. (2011) Examples and Extensions for the \pkg{NMOF}
  Package. \url{http://enricoschumann.net/NMOF.htm}

}
\author{
Enrico Schumann
}
\seealso{
\code{\link{DEopt}}
}
\examples{
## Least Median of Squares (LMS) estimation
genData <- function(nP, nO, ol, dy) {
    ### create dataset as in Salibian-Barrera & Yohai 2006
    ### nP = regressors, nO  = number of obs
    ### ol = number of outliers, dy  = outlier size 
    mRN <- function(m, n) array(rnorm(m * n), dim = c(m, n))
    y <- mRN(nO, 1)
    X <- cbind(as.matrix(numeric(nO) + 1), mRN(nO, nP - 1L))
    zz <- sample(nO)
    z  <- cbind(1, 100, array(0, dim = c(1L, nP - 2L)))
    for (i in 1L:ol) {
        X[zz[i], ] <- z
        y[zz[i]] <- dy 
    }
    list(X = X, y = y)
}

OF <- function(param, data) {
    X <- data$X
    y <- data$y
    aux <- as.vector(y) - X \%*\% param
    ### as.vector(y) for recycling; param is a matrix
    aux <- aux * aux
    aux <- apply(aux, 2, sort, partial = data$h)
    aux[h, ]
}

nP <- 2L; nO <- 100L; ol <- 10L; dy <- 150
aux <- genData(nP,nO,ol,dy); X <- aux$X; y <- aux$y

h <- (nO + nP + 1L) \%/\% 2
data <- list(y = y,X = X, h = h)

algo <- list(min = rep(-10, nP), max = rep( 10, nP),
    c1 = 1.0, c2 = 2.0,
    iner = 0.7, initV = 1, maxV = 3, 
    nP = 100L, nG = 300L, loopOF = FALSE)

system.time(sol <- PSopt(OF = OF, algo = algo, data = data))
if (require("MASS", quietly = TRUE)) {
    ### for nsamp = "best", in this case, complete enumeration 
    ### will be tried. See ?lqs
    system.time(test1 <- lqs(data$y ~ data$X[ ,-1L], 
            adjust = TRUE, 
            nsamp = "best", 
            method = "lqs", 
            quantile = data$h))
}
## check
x1 <- sort((y - X \%*\% as.matrix(sol$xbest))^2)[h]
cat("Particle Swarm\n",x1,"\n\n")

if (require("MASS", quietly = TRUE)) {
    x2 <- sort((y - X \%*\% as.matrix(coef(test1)))^2)[h]
    cat("lqs\n",x2,"\n\n")
}
}
\keyword{optimize}
