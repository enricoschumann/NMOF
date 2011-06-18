% version 2011-04-28
\name{restartOpt}
\alias{restartOpt}
%\alias{restartOpt2}
\title{
Restart an Optimisation Algorithm
}
\description{
The function provides a simple wrapper for the optimisation algorithms in the package.
}
\usage{
restartOpt(fun, n, OF, algo = NULL, ...)

%restartOpt2(fun, n, OF, algo = NULL, ..., multicore = FALSE)
}
\arguments{
  \item{fun}{
The optimisation function: \code{TAopt}, \code{LSopt}, \code{DEopt}, \code{PSopt}.
}
  \item{n}{
The number of restarts.
}
  \item{OF}{
The objective function.
}
  \item{algo}{
The list \code{algo} that is passed to the particular optimisation function.
}
  \item{\dots}{
Additional data that is passed to the particular optimisation function.
}
%  \item{multicore}{
%Experimental. Uses \code{mclapply} to distribute the restarts. Requires the \pkg{multicore} package.
%}

}
\details{
The function returns a list of lists. If a specific starting solution is passed, all runs will start from the same solution. 
If this is not desired, solutions can to be created randomly. This is done automatically in \code{\link{DEopt}} and \code{\link{PSopt}}. 
For \code{\link{LSopt}} and \code{\link{TAopt}}, the passed initial solution \code{algo$x0} is checked with 
\code{is.function}: if \code{TRUE}, the function is evaluated in each single run.
}
\value{
Returns a list of \code{n} lists. Each of the \code{n} lists stores the output of one of the runs.
}
\references{
Gilli, M., Maringer, D. and Schumann, E. (2011) \emph{Numerical Methods and Optimization in Finance}. Elsevier. 
\url{http://www.elsevierdirect.com/product.jsp?isbn=9780123756626}
}
\author{
Enrico Schumann
}
\note{
This is a pure convenience function. There no error handling.
}

\seealso{
\code{\link{TAopt}}, \code{\link{LSopt}}, \code{\link{DEopt}}, \code{\link{PSopt}}
}
\examples{
# see example(DEopt)
algo <- list(nP = 50L,
              F = 0.5, 
             CR = 0.9, 
            min = c(-10, -10), 
            max = c( 10,  10),
    printDetail = FALSE,
       printBar = FALSE)

# choose a larger 'n' when you can afford it
algo$nG <- 100L
res100 <- restartOpt(DEopt, n = 10L, OF = tfTrefethen, algo = algo)
res100F <- sapply(res100, `[[`, "OFvalue")

algo$nG <- 300L
res300 <- restartOpt(DEopt, n = 10, OF = tfTrefethen, algo = algo)
res300F <- sapply(res300, `[[`, "OFvalue")

xx <- pretty(c(res100F, res300F, -3.31))
plot(ecdf(res100F), main = "optimum is -3.306", 
    xlim = c(xx[1], tail(xx,1)))
abline(v = -3.3069, col = "red")  # optimum
lines(ecdf(res300F), col = "blue")
legend(x = "right", box.lty = 0, , lty = 1,
    legend = c("optimum", "100 generations", "300 generations"),
    pch = c(NA, 19, 19), col = c("red", "black", "blue"))

\dontrun{
# with R >= 2.13.0 and the compiler package
algo$nG <- 100L
system.time(res100 <- restartOpt(DEopt, n = 10, OF = tfTrefethen, algo = algo))

require(compiler)
enableJIT(3)
system.time(res100 <- restartOpt(DEopt, n = 10, OF = tfTrefethen, algo = algo))
}

\dontrun{
# experimental: multicore
require(multicore)
algo$nP <- 100L; algo$nG <- 1000L
myOpt <- function(fun, OF, algo) {
    f <- function(ignore) fun(OF = OF, algo = algo)
    f
} 
DE <- myOpt(DEopt, OF = tfTrefethen, algo = algo)
system.time(res1000a <-   lapply(1:10, DE))
system.time(res1000b <- mclapply(1:10, DE))

res1000Fa <- sapply(res1000a, `[[`, "OFvalue")
res1000Fb <- sapply(res1000b, `[[`, "OFvalue")
plot( ecdf(res1000Fa))
lines(ecdf(res1000Fb), col = "blue")
}

}
