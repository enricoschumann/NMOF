% version 2011-05-30
\name{DEopt}
\alias{DEopt}
\title{
Optimisation with Differential Evolution
}
\description{
The function implements the standard Differential Evolution algorithm.
}
\usage{
DEopt(OF, algo = list(), ...)
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
The function implements the standard Differential Evolution (no jittering, etc). Differential Evolution (\acronym{DE}) 
is a population-based optimisation heuristic; it was proposed by Storn and Price (1997). \acronym{DE} evolves several solutions 
(collected in the \sQuote{population}) over a number of iterations (\sQuote{generations}). 
In a given generation, new solutions are created and evaluated; better solutions replace inferior ones in the population. Finally, the 
best solution of the population is returned. See the references for more details on the mechanisms.

To allow for constraints, the evaluation works as follows: after a new solution is created, it is (i) repaired,
(ii) evaluated through the objective function, (iii) penalised. Step (ii) is done by a call to \code{OF}; steps (i) and (iii) by calls to 
\code{algo$repair} and \code{algo$pen}. Step (i) and (iii) are optional, so the respective functions default to \code{NULL}. A penalty 
can also be directly written in the \code{OF}, since it amounts to a positive number added to the \sQuote{clean} objective function value. 
It can be advantageous to write a separate penalty function if either only the objective function or only the penalty function can be vectorised.
(Constraints can also be added without these mechanisms. Solutions that violate constraints can, for instance, be mapped to feasible solutions, 
but without actually changing them. See Maringer and Oyewumi (2007) for an example.)

Conceptually, \acronym{DE} consists of two loops: one loop across the generations and, in any given generation, one loop across the solutions. 
This is the default, controlled by the variables \code{algo$loopOF}, \code{algo$loopRepair} and \code{algo$loopPen}, which all 
default to \code{TRUE}. But it does not matter in what order the solutions are evaluated (or repaired or penalised), 
so the second loop can be vectorised. Examples are given in the vignettes and in the book. The respective 
\code{algo$loopFun} must then be set to \code{FALSE}.

All objects that are passed through \code{\dots} will be passed to the objective function, to the repair function and 
to the penalty function.

The list \code{algo} contains the following items.
\describe{
\item{\code{nP}}{population size. Defaults to 50. Using default settings may not be a good idea.}
\item{\code{nG}}{number of generations. Defaults to 300. Using default settings may not be a good idea.}
\item{\code{F}}{defaults to 0.5. Using default settings may not be a good idea.}
\item{\code{CR}}{mutation probability. Defaults to 0.9. Using default settings may not be a good idea.}
\item{\code{min}}{A vector of minimum parameter values.}
\item{\code{max}}{A vector of minimum parameter values.}
\item{\code{pen}}{a penalty function. Default is NULL (no penalty).}
\item{\code{mP}}{optional: the initial population. A matrix of size \code{length(algo$min)} times \code{algo$nP}, or a function that creates 
such a matrix. If a function, it should take no arguments.}
\item{\code{repair}}{a repair function. Default is NULL (no repairing).}
\item{\code{loopOF}}{logical. Should the OF be evaluated through a loop? Defaults to TRUE.}
\item{\code{loopPen}}{logical. Should the penalty function (if specified) be evaluated through a loop? Defaults to TRUE.}
\item{\code{loopRepair}}{logical. Should the repair function (if specified) be evaluated through a loop? Defaults to TRUE.}
\item{\code{printDetail}}{If \code{TRUE} (the default), information is printed.}
\item{\code{printBar}}{If \code{TRUE} (the default), information in progress is printed (\code{txtProgressBar} is used).}
}
The vectors \code{min} and \code{max} are used to determine the dimension of the problem and to randomly 
initialise the population. They are no constraints.


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

Maringer, D. and Oyewumi, O. (2007). Index Tracking with Constrained Portfolios. \emph{Intelligent Systems in Accounting, Finance and Management}, 
\bold{15}(1), pp. 57--71.

Storn, R., and Price, K. (1997) Differential Evolution -- a Simple and Efficient Heuristic for Global Optimization over Continuous Spaces. 
\emph{Journal of Global Optimization}, \bold{11}(4), pp. 341--359. 
}
\author{
Enrico Schumann
}

\seealso{
\code{\link{PSopt}}
}
\examples{
## Example 1: Trefethen's 100-digit challenge (problem 4)
## http://people.maths.ox.ac.uk/trefethen/hundred.html

OF <- tfTrefethen  # see ?testFunctions
algo <- list(nP = 50L, 
             nG = 300L,
              F = 0.5, 
             CR = 0.9, 
            min = c(-10, -10), 
            max = c( 10,  10))

sol <- DEopt(OF = OF, algo = algo)
# correct answer: -3.30686864747523
format(sol$OFvalue, digits = 12)
# check convergence of population
sd(sol$popF)
ts.plot(sol$Fmat, xlab = "generations", ylab = "OF")


## Example 2: vectorising the evaluation of the population
OF <- tfRosenbrock  # see ?testFunctions
size <- 20L         # define dimension
x <- rep(1, size)   # the known solution ...
OF(x)               # ... should give zero

algo <- list(printBar = FALSE,
                   nP = 100L, 
                   nG = 5000L,
                    F = 0.5, 
                   CR = 0.9, 
                  min = rep(-20, size), 
                  max = rep( 20, size))

# run DEopt
(t1 <- system.time(sol <- DEopt(OF = OF, algo = algo)))
sol$xbest
sol$OFvalue  # should be zero (with luck)

# a vectorised OF: works *only* with matrix x
OF2 <- function(x) {
    n <- dim(x)[1L]
    xi <- x[1L:(n - 1L), ]
    colSums(100 * (x[2L:n, ] - xi * xi)^2 + (1 - xi)^2)
}

# random solution
x <- matrix(rnorm(size * algo$nP), size, algo$nP)
all.equal(OF2(x)[1L:3L], c(OF(x[ ,1L]), OF(x[ ,2L]), OF(x[ ,3L])))

# run DEopt and compare computing time
algo$loopOF <- FALSE
(t2 <- system.time(sol2 <- DEopt(OF = OF2, algo = algo)))
sol2$xbest
sol2$OFvalue  # should be zero (with luck)
t1/t2  # speedup
}
\keyword{optimize}

