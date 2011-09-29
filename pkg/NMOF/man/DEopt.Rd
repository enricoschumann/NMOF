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
  \item{\dots}{Other pieces of data required to evaluate the objective function. See Details and Examples.
}
}
\details{

  The function implements the standard Differential Evolution (no
  jittering or other features). Differential Evolution (\acronym{DE}) is
  a population-based optimisation heuristic proposed by Storn and Price
  (1997). \acronym{DE} evolves several solutions (collected in the
  \sQuote{population}) over a number of iterations
  (\sQuote{generations}). In a given generation, new solutions are
  created and evaluated; better solutions replace inferior ones in the
  population. Finally, the best solution of the population is
  returned. See the references for more details on the mechanisms.

  To allow for constraints, the evaluation works as follows: after a new
  solution is created, it is (i) repaired, (ii) evaluated through the
  objective function, (iii) penalised. Step (ii) is done by a call to
  \code{OF}; steps (i) and (iii) by calls to \code{algo$repair} and
  \code{algo$pen}. Step (i) and (iii) are optional, so the respective
  functions default to \code{NULL}. A penalty is a positive number added
  to the \sQuote{clean} objective function value, so it can also be
  directly written in the \code{OF}. Writing a separate penalty function
  is often clearer; it can be more efficient if either only the objective
  function or only the penalty function can be vectorised.  (Constraints
  can also be added without these mechanisms. Solutions that violate
  constraints can, for instance, be mapped to feasible solutions, but
  without actually changing them. See Maringer and Oyewumi (2007) for an
  example.)

  Conceptually, \acronym{DE} consists of two loops: one loop across the
  generations and, in any given generation, one loop across the solutions.
  \code{DEopt} indeed uses, as the default, two loops. But it does not
  matter in what order the solutions are evaluated (or repaired or
  penalised), so the second loop can be vectorised. This is controlled by
  the variables \code{algo$loopOF}, \code{algo$loopRepair} and
  \code{algo$loopPen}, which all default to \code{TRUE}. Examples are
  given in the vignettes and in the book. The respective
  \code{algo$loopFun} must then be set to \code{FALSE}.

  All objects that are passed through \code{\dots} will be passed to the
  objective function, to the repair function and to the penalty function.

The list \code{algo} collects the following settings for the algorithm:
\describe{
  \item{\code{nP}}{population size. Defaults to 50. Using default settings may not be a good idea.}
  \item{\code{nG}}{number of generations. Defaults to 300. Using default settings may not be a good idea.}
  \item{\code{F}}{The step size. Typically a numeric vector of length
    one; default is 0.5. Using default settings may not be a good
    idea. (\code{F} can also be a vector with different values for each decision variable.)}
  \item{\code{CR}}{probability for crossover. Defaults to 0.9. Using
    default settings may not be a good idea.}
  \item{\code{min}}{a vector of minimum parameter values. See below.}
  \item{\code{max}}{a vector of maximum parameter values. See below.}
  \item{\code{pen}}{a penalty function. Default is \code{NULL} (no penalty).}
  \item{\code{initP}}{optional: the initial population. A matrix of size
    \code{length(algo$min)} times \code{algo$nP}, or a function that
    creates such a matrix. If a function, it should take no arguments.}
  \item{\code{repair}}{a repair function. Default is \code{NULL} (no repairing).}
  \item{\code{loopOF}}{logical. Should the \code{OF} be evaluated
    through a loop? Defaults to \code{TRUE}.}
  \item{\code{loopPen}}{logical. Should the penalty function (if
    specified) be evaluated through a loop? Defaults to \code{TRUE}.}
  \item{\code{loopRepair}}{logical. Should the repair function (if
    specified) be evaluated through a loop? Defaults to \code{TRUE}.}
  \item{\code{printDetail}}{If \code{TRUE} (the default), information is printed.}
  \item{\code{printBar}}{If \code{TRUE} (the default), information in
    progress is printed (\code{\link{txtProgressBar}} is used).}
  \item{\code{storeF}}{if \code{TRUE} (the default), the objective
    function values for every solution in every generation are stored
    and returned as matrix \code{Fmat}.}
  \item{\code{storeSolutions}}{default is \code{FALSE}. If \code{TRUE},
    the solutions (ie, decision variables) in every generation are
    stored and returned as a list \code{P} in list \code{xlist} (see
    Value section below). To check, for instance, the solutions at the end of the
    \code{i}th generation, retrieve \code{xlist[[c(1L, i)]]}. This will be
    a matrix of size \code{length(algo$min)} times \code{algo$nP}.
    (To be consistent with other functions, \code{xlist} is
    itself a list. In the case of \code{DEopt}, it contains just one element.)}
} % end describe
  The vectors \code{min} and \code{max} are used to determine the
  dimension of the problem and to randomly initialise the
  population. They are no constraints: a solution may well be outside
  these limits.
} % end Details
\value{
A list:
\item{\code{xbest}}{the solution (the best member of the population), which is
  a numeric vector}
\item{\code{OFvalue}}{objective function value of best solution}
\item{\code{popF}}{a vector. The objective function values in the population.}
\item{\code{Fmat}}{if \code{algo$storeF} is \code{TRUE}, a matrix of size
  \code{algo$nG} times \code{algo$nP} containing the objective function
  values of all solutions over the generations; else \code{NA}.}
\item{\code{xlist}}{if \code{algo$storeSolutions} is TRUE, a list that
  contains a list \code{P} of matrices; else \code{NA}.}
} % end value
\references{
Gilli, M., Maringer, D. and Schumann, E. (2011) \emph{Numerical Methods
  and Optimization in Finance}. Elsevier.
\url{http://www.elsevierdirect.com/product.jsp?isbn=9780123756626}

Maringer, D. and Oyewumi, O. (2007). Index Tracking with Constrained
Portfolios. \emph{Intelligent Systems in Accounting, Finance and Management},
\bold{15}(1), pp. 57--71.

Schumann, E. (2011) Examples and Extensions for the \pkg{NMOF} Package. \url{http://ssrn.com/abstract=1886522}

Storn, R., and Price, K. (1997) Differential Evolution -- a Simple and
Efficient Heuristic for Global Optimization over Continuous Spaces.
\emph{Journal of Global Optimization}, \bold{11}(4), pp. 341--359.
} % end references
\author{
Enrico Schumann
}
\seealso{
    \code{\link{GAopt}}, \code{\link{PSopt}}
}
\examples{
## Example 1: Trefethen's 100-digit challenge (problem 4)
## http://people.maths.ox.ac.uk/trefethen/hundred.html

OF <- tfTrefethen               ### see ?testFunctions
algo <- list(nP = 50L,          ### population size
             nG = 300L,         ### number of generations
              F = 0.6,          ### step size
             CR = 0.9,          ### prob of crossover
            min = c(-10, -10),  ### range for initial population
            max = c( 10,  10))

sol <- DEopt(OF = OF, algo = algo)
## correct answer: -3.30686864747523
format(sol$OFvalue, digits = 12)
## check convergence of population
sd(sol$popF)
ts.plot(sol$Fmat, xlab = "generations", ylab = "OF")


## Example 2: vectorising the evaluation of the population
OF <- tfRosenbrock     ### see ?testFunctions
size <- 3L             ### define dimension
x <- rep.int(1, size)  ### the known solution ...
OF(x)                  ### ... should give zero

algo <- list(printBar = FALSE,
                   nP = 30L,
                   nG = 300L,
                    F = 0.6,
                   CR = 0.9,
                  min = rep(-100, size),
                  max = rep( 100, size))

## run DEopt
(t1 <- system.time(sol <- DEopt(OF = OF, algo = algo)))
sol$xbest
sol$OFvalue  ### should be zero (with luck)

## a vectorised OF: works only with a *matrix* x
OF2 <- function(x) {
    n <- dim(x)[1L]
    xi <- x[seq_len(n - 1L), ]
    colSums(100 * (x[2L:n, ] - xi * xi)^2 + (1 - xi)^2)
}

## random solution
x <- matrix(rnorm(size * algo$nP), size, algo$nP)
all.equal(OF2(x)[1:3],
          c(OF(x[ ,1L]), OF(x[ ,2L]), OF(x[ ,3L])))

## run DEopt and compare computing time
algo$loopOF <- FALSE
(t2 <- system.time(sol2 <- DEopt(OF = OF2, algo = algo)))
sol2$xbest
sol2$OFvalue       ### should be zero (with luck)
t1[[3L]]/t2[[3L]]  ### speedup
}
\keyword{optimize}
