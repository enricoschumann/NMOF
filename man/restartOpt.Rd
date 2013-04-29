\name{restartOpt}
\alias{restartOpt}
\title{
  Restart an Optimisation Algorithm
}
\description{
  The function provides a simple wrapper for the optimisation algorithms in the package.
}
\usage{
restartOpt(fun, n, OF, algo, ...,
           method = c("loop", "multicore", "snow"),
           mc.control = list(), cl = NULL,
           best.only = FALSE)
}
\arguments{
  \item{fun}{
    the optimisation function: \code{DEopt}, \code{GAopt}, \code{LSopt},
    \code{TAopt} or \code{PSopt}
  }
  \item{n}{
    the number of restarts
  }
  \item{OF}{
    the objective function
  }
  \item{algo}{
    the list \code{algo} that is passed to the particular optimisation function
  }
  \item{\dots}{
    additional data that is passed to the particular optimisation function
  }
  \item{method}{
    can be \code{loop} (the default), \code{multicore}
    or \code{snow}. See Details.
  }
  \item{mc.control}{
    a list containing settings that will be passed to \code{mclapply} if
    \code{method} is \code{multicore}. Must be a list of named
    elements. See the documentation of \code{mclapply}.
  }
  \item{cl}{
    default is \code{NULL}. If method \code{snow} is used, this must be
    a cluster object or an integer (the number of cores).}
  \item{best.only}{if \code{TRUE}, only the best run is
    reported. Default is \code{FALSE}.}
}
\details{

  The function returns a list of lists. If a specific starting solution
  is passed, all runs will start from this solution. If this is not
  desired, initial solutions can be created randomly. This is done per
  default in \code{\link{DEopt}}, \code{\link{GAopt}} and
  \code{\link{PSopt}}, but \code{\link{LSopt}} and \code{\link{TAopt}}
  require to specify a starting solution.

  In case of \code{\link{LSopt}} and \code{\link{TAopt}}, the passed
  initial solution \code{algo$x0} is checked with \code{is.function}: if
  \code{TRUE}, the function is evaluated in each single run. For
  \code{\link{DEopt}}, \code{\link{GAopt}} and \code{\link{PSopt}}, the
  initial solution (which also can be a function) is specified with
  \code{algo$initP}.

  The argument \code{method} determines how \code{fun} is
  evaluated. Default is \code{loop}. If \code{method == "multicore"},
  function \code{mclapply} from package \pkg{multicore} is used. Further
  settings for \code{mclapply} can be passed through the list
  \code{mc.control}. If \code{multicore} is chosen but the package is
  not available, then \code{method} will be
  set to \code{loop} and a warning is issued. If \code{method ==
    "snow"}, function \code{clusterApply} from package \pkg{snow} is
  used. In this case, the argument \code{cl} must either be a cluster
  object (see the documentation of \code{clusterApply}) or an
  integer. If an integer, a cluster will be set up via
  \code{makeCluster(c(rep("localhost", cl)), type = "SOCK")}, and
  \code{stopCluster} is called when the function is exited. If
  \code{snow} is chosen but the package is not available or \code{cl} is
  not specified, then \code{method} will be set to \code{loop} and a
  warning is issued. In case that \code{cl} is an cluster object,
  \code{stopCluster} will not be called automatically.

}
\value{

  If \code{best.only} is \code{FALSE} (the default), the function
  returns a list of \code{n} lists. Each of the \code{n} lists stores
  the output of one of the runs.

  If \code{best.only} is \code{TRUE}, only the best restart is
  reported. The returned list has the structure specific to the used
  method.

}
\references{

  Gilli, M., Maringer, D. and Schumann, E. (2011) \emph{Numerical
  Methods and Optimization in Finance}. Elsevier.
  \url{http://www.elsevierdirect.com/product.jsp?isbn=9780123756626}

  Schumann, E. (2012) The \pkg{NMOF} Manual.
  \url{http://enricoschumann.net/NMOF.htm}

}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{DEopt}}, \code{\link{GAopt}}, \code{\link{LSopt}},
  \code{\link{PSopt}}, \code{\link{TAopt}}
}
\examples{
## see example(DEopt)
algo <- list(nP = 50L,
              F = 0.5,
             CR = 0.9,
            min = c(-10, -10),
            max = c( 10,  10),
    printDetail = FALSE,
       printBar = FALSE)

## choose a larger 'n' when you can afford it
algo$nG <- 100L
res100 <- restartOpt(DEopt, n = 5L, OF = tfTrefethen, algo = algo)
res100F <- sapply(res100, `[[`, "OFvalue")

algo$nG <- 200L
res200 <- restartOpt(DEopt, n = 5L, OF = tfTrefethen, algo = algo)
res200F <- sapply(res200, `[[`, "OFvalue")

xx <- pretty(c(res100F, res200F, -3.31))
plot(ecdf(res100F), main = "optimum is -3.306",
     xlim = c(xx[1L], tail(xx, 1L)))
abline(v = -3.3069, col = "red")  ## optimum
lines(ecdf(res200F), col = "blue")
legend(x = "right", box.lty = 0, , lty = 1,
      legend = c("optimum", "100 generations", "200 generations"),
      pch = c(NA, 19, 19), col = c("red", "black", "blue"))

## a 'best-of-N' strategy: given a sample x of objective
## function values, compute the probability that, after N draws,
## we have at least one realisation not worse than X
x <- c(0.1,.3,.5,.5,.6)
bestofN <- function(x, N) {
    nx <- length(x)
    function(X)
        1 - (sum(x > X)/nx)^N
}
bestof2 <- bestofN(x, 2)
bestof5 <- bestofN(x, 5)
bestof2(0.15)
bestof5(0.15)


\dontrun{
## with R >= 2.13.0 and the compiler package
algo$nG <- 100L
system.time(res100 <- restartOpt(DEopt, n = 10L, OF = tfTrefethen, algo = algo))

require("compiler")
enableJIT(3)
system.time(res100 <- restartOpt(DEopt, n = 10L, OF = tfTrefethen, algo = algo))
}
}
