\name{GAopt}
\alias{GAopt}
\title{
  Optimisation with a Genetic Algorithm
}
\description{
A simple Genetic Algorithm for minimising a function.
}
\usage{
GAopt(OF, algo = list(), ...)
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

The function implements a simple Genetic Algorithm (\acronym{GA}). A
  \acronym{GA} evolves a collection of solutions (the so-called
  population), all of which are coded as vectors containing only zeros
  and ones. (In \code{GAopt}, solutions are of mode \code{logical}.)
  The algorithm starts with randomly-chosen or user-supplied population
  and aims to iteratively improve this population by mixing solutions
  and by switching single bits in solutions, both at random. In each
  iteration, such randomly-changed solutions are compared with the
  original population and better solutions replace inferior
  ones. In \code{GAopt}, the population size is kept constant.

\acronym{GA} language: iterations are called generations; new solutions
are called offspring or children (and the existing solutions, from which
the children are created, are parents); the objective function is called
a fitness function; mixing solutions is a crossover; and randomly
changing solutions is called mutation. The choice which solutions remain in
the population and which ones are discarded is called selection. In
\code{GAopt}, selection is pairwise: a given child is compared with a
given parent; the better of the two is kept.  In this way, the best
solution is automatically retained in the population.

To allow for constraints, the evaluation works as follows: after new
solutions are created, they are (i) repaired, (ii) evaluated through the
objective function, (iii) penalised. Step (ii) is done by a call to
\code{OF}; steps (i) and (iii) by calls to \code{algo$repair} and
\code{algo$pen}. Step (i) and (iii) are optional, so the respective
functions default to \code{NULL}. A penalty can also be directly written
in the \code{OF}, since it amounts to a positive number added to the
\sQuote{clean} objective function value; but a separate function is
often clearer. A separate penalty function is advantagous if either only
the objective function or only the penalty function can be vectorised.

Conceptually a \acronym{GA} consists of two loops: one loop across the
generations and, in any given generation, one loop across the solutions.
This is the default, controlled by the variables \code{algo$loopOF},
\code{algo$loopRepair} and \code{algo$loopPen}, which all default to
\code{TRUE}. But it does not matter in what order the solutions are
evaluated (or repaired or penalised), so the second loop can be
vectorised. The respective \code{algo$loopFun} must then be set to
\code{FALSE}. (See also the examples for \code{\link{DEopt}} and
\code{\link{PSopt}}.)

The evaluation of the objective function in a given generation can even
be distributed. For this, an argument \code{algo$methodOF} needs to be
set; see below for details (and Schumann, 2011, for examples).

All objects that are passed through \code{\dots} will be passed to the
objective function, to the repair function and to the penalty function.

The list \code{algo} contains the following items:
\describe{
\item{\code{nB}}{number of bits per solution. Must be specified.}
\item{\code{nP}}{population size. Defaults to 50. Using default settings may not be a good idea.}
\item{\code{crossover}}{The crossover method. Default is \dQuote{onePoint}; also possible is \dQuote{uniform}.}
\item{\code{prob}}{The probability for switching a single bit. Defaults to 0.01; typically a small number.}
\item{\code{pen}}{a penalty function. Default is \code{NULL} (no penalty).}
\item{\code{repair}}{a repair function. Default is \code{NULL} (no repairing).}
\item{\code{initP}}{optional: the initial population. A logical matrix of size \code{length(algo$nB)} times \code{algo$nP},
or a function that creates such a matrix. If a function, it must take no arguments. If \code{mode(mP)} is not \code{logical},
then \code{storage.mode(mP)} will be tried (and a warning will be issued).}
\item{\code{loopOF}}{logical. Should the \code{OF} be evaluated through a loop? Defaults to \code{TRUE}.}
\item{\code{loopPen}}{logical. Should the penalty function (if specified) be evaluated through a loop? Defaults to \code{TRUE}.}
\item{\code{loopRepair}}{logical. Should the repair function (if specified) be evaluated through a loop? Defaults to \code{TRUE}.}

\item{\code{methodOF}}{\code{loop} (the default), \code{vectorised},
  \code{snow} or \code{multicore}. Setting \code{vectorised} is
  equivalent to having \code{algo$loopOF} set to \code{FALSE} (and
  \code{methodOF} overrides \code{loopOF}). \code{snow} and \code{multicore} use functions
  \code{clusterApply} and \code{mclapply}, respectively. For \code{snow}, an object
  \code{algo$cl} needs to be specified (see below). For
  \code{multicore}, optional arguments can be passed
  through \code{algo$mc.control} (see below).}
\item{\code{cl}}{a cluster object or the number of cores. See
  documentation of package \code{snow}.}
\item{\code{mc.control}}{a list of named elements; optional settings for
  \code{mclapply} (for instance, \code{list(mc.set.seed = FALSE)})}
\item{\code{printDetail}}{If \code{TRUE} (the default), information is printed.}
\item{\code{printBar}}{If \code{TRUE} (the default), a \code{txtProgressBar} is printed.}
\item{\code{storeF}}{If \code{TRUE} (the default), the
  objective function values for every solution in every generation are stored and returned as matrix \code{Fmat}.}
\item{\code{storeSolutions}}{If \code{TRUE}, the solutions (ie, binary strings) in every generation are
    stored and returned as a list \code{P} in list \code{xlist} (see
    Value section below). To check, for instance, the solutions at the end of the
    \code{i}th generation, retrieve \code{xlist[[c(1L, i)]]}. This will be
    a matrix of size \code{algo$nB} times \code{algo$nP}.}
}
}
\value{
A list:
\item{xbest}{the solution (the best member of the population)}
\item{OFvalue}{objective function value of best solution}
\item{popF}{a vector. The objective function values in the final population.}
\item{Fmat}{if \code{algo$storeF} is \code{TRUE}, a matrix of size
  \code{algo$nG} times \code{algo$nP} containing 
  the objective function values of all solutions over the generations;
  else \code{NA}}
\item{xlist}{if \code{algo$storeSolutions} is \code{TRUE}, a list that
  contains a list \code{P} of matrices; else \code{NA}.}
}
\references{
Gilli, M., Maringer, D. and Schumann, E. (2011) \emph{Numerical Methods and Optimization in Finance}. Elsevier.
\url{http://www.elsevierdirect.com/product.jsp?isbn=9780123756626}

  Schumann, E. (2011) Examples and Extensions for the \pkg{NMOF}
  Package. \url{http://enricoschumann.net/files/NMOFex.pdf}
}
\author{
Enrico Schumann
}

\seealso{
\code{\link{DEopt}}, \code{\link{PSopt}}
}
\examples{
## a *very* simple problem (why?):
## match a binary (logical) string y

size <- 20L  ### the length of the string
OF <- function(x, y) sum(x != y)
y <- runif(size) > 0.5
x <- runif(size) > 0.5
OF(y, y)     ### the optimum value is zero
OF(x, y)
algo <- list(nB = size, nP = 20L, nG = 100L, prob = 0.002,
             printBar = TRUE)
sol <- GAopt(OF, algo = algo, y = y)

## show differences (if any: marked by a '^')
cat(as.integer(y), "\n", as.integer(sol$xbest), "\n",
    ifelse(y == sol$xbest , " ", "^"), "\n", sep = "")

algo$nP <- 3L  ### that shouldn't work so well
sol2 <- GAopt(OF, algo = algo, y = y)

## show differences (if any: marked by a '^')
cat(as.integer(y), "\n", as.integer(sol2$xbest), "\n",
    ifelse(y == sol2$xbest , " ", "^"), "\n", sep = "")
}
\keyword{optimize}
