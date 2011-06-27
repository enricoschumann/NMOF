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
The function implements a simple Genetic Algorithm. Solutions are coded as bitstrings (zeros and ones). The algorithm starts with a population 
of solutions and aims to improve them iteratively by randomly mixing solutions (crossover) and switching single bits in solutions 
(mutation). These new solutions are typically called offspring or children. At the end of each generation, the algorithm selects from the parents 
and the children so to keep the population size constant.

Selection is pairwise: a given child is compared with a given parent; the better of the two is kept. In this way, the best solution is automatically 
retained in the population.

To allow for constraints, the evaluation works as follows: after new solutions are created, they are (i) repaired,
(ii) evaluated through the objective function, (iii) penalised. Step (ii) is done by a call to \code{OF}; steps (i) and (iii) by calls to 
\code{algo$repair} and \code{algo$pen}. Step (i) and (iii) are optional, so the respective functions default to \code{NULL}. A penalty 
can also be directly written in the \code{OF}, since it amounts to a positive number added to the \sQuote{clean} objective function value. 
It can be advantageous to write a separate penalty function if either only the objective function or only the penalty function can be vectorised.

Conceptually, a \acronym{GA} consists of two loops: one loop across the generations and, in any given generation, one loop across the solutions. 
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
\item{\code{mP}}{optional: the initial population. A matrix of size \code{length(algo$min)} times \code{algo$nP}, or a function that creates 
such a matrix. If a function, it should take no arguments.}
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
# a *very* simple problem:
# match a binary string y
#
size <- 20L  # the length of the string
OF <- function(x, data) sum(abs(x - y))
y <- as.integer(runif(size) > 0.5)
x <- as.integer(runif(size) > 0.5)
data <- list(y = y)
OF(y, data)  # the optimum value is zero
OF(x, data)
algo <- list(nB = size, nP = 20L, nG = 100L, prob = 0.002, 
    printBar = TRUE)
system.time(sol <- GAopt(OF, algo=algo, data = data))
# show differences (if any: marked by a dot)
cat(y, "\n", sol$xbest, "\n", ifelse(y - sol$xbest == 0, " ", "."), 
    "\n", sep = "")
}
\keyword{optimize}

