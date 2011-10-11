\name{testFunctions}
\alias{testFunctions}
\alias{tfAckley}
\alias{tfTrefethen}
\alias{tfGriewank}
\alias{tfRastrigin}
\alias{tfRosenbrock}
\alias{tfSchwefel}
\alias{tfTrefethen}

\concept{Test functions for global optimisation}
\title{
Classical Test Functions for Unconstrained Optimisation
}
\description{
A number of functions that have been suggested in the literature as benchmarks for unconstrained optimisation.
}
\usage{
tfAckley(x)
tfGriewank(x)
tfRastrigin(x)
tfRosenbrock(x)
tfSchwefel(x)
tfTrefethen(x)
}

\arguments{
  \item{x}{
    a numeric vector of arguments. See Details.
}
}
\details{
All functions take as argument only one variable, a numeric vector \code{x} whose length determines the dimensionality of the problem.

The \emph{Ackley} function is implemented as
\deqn{\exp(1) + 20 -20 \exp{\left(-0.2 \sqrt{\frac{1}{n}\sum_{i=1}^n x_i^2}\right)} - \exp{\left(\frac{1}{n}\sum_{i=1}^n  \cos(2 \pi x_i)\right)}\,.}{exp(1) + 20 - 20 * exp(-0.2 * sqrt(sum(x^2)/n)) - exp(sum(cos(2 * pi * x))/n)}
The minimum function value is zero; reached at \eqn{x=0}{x = 0}.

The \emph{Griewank} function is given by \deqn{1+\frac{1}{4000} \sum^n_{i=1} x_i^2 - \prod_{i=1}^n \cos \left(\frac{x_i}{\sqrt{i}}\right)\,.}{sum(x^2)/4000 - prod(cos(x/sqrt(1:n))) + 1}
The function is minimised at \eqn{x=0}{x = 0}; its minimum value is zero.

The \emph{Rastrigin} function: \deqn{10n + \sum_{i=1}^n \left(x_i^2 -10\cos(2\pi x_i)\right)\,.}{10*n + sum(x^2 - 10 * cos(2 * pi * x))}
The minimum function value is zero; reached at \eqn{x=0}{x = 0}.

The \emph{Rosenbrock} (or banana) function: \deqn{\sum_{i=1}^{n-1}\left(100 (x_{i+1}-x_i^2)^2 + (1-x_i)^2\right)\,.}{sum(100*(x[2:n] - x[1:(n - 1)]^2)^2 + (1 - x[1:(n - 1)])^2)}
The minimum function value is zero; reached at \eqn{x=1}{x = 1}.

The \emph{Schwefel} function: \deqn{\sum_{i=1}^n \left(-x_i \sin\left(\sqrt{|x_i|}\right)\right)\,.}{sum(-x * sin(sqrt(abs(x))))}
The minimum function value (to about 8 digits) is \eqn{-418.9829n}{-418.9829n}; reached at \eqn{x = 420.9687}{x = 420.9687}.

\emph{Trefethen}'s function takes a two-dimensional \code{x} (here written as \eqn{x}{x} and \eqn{y}{y}); it is defined as
\deqn{\exp(\sin(50x)) + \sin(60 e^y) + \sin(70 \sin(x)) + \sin(\sin(80y)) - \sin(10(x+y)) + \frac{1}{4}(x^2+y^2)\,.}{exp(sin(50 * x)) + sin(60 * exp(y)) + sin(70 * sin(x)) + sin(sin(80 * y)) - sin(10 * (x + y)) + (x^2 + y^2)/4}
The minimum function value is -3.3069; reached at \code{c(-0.0244, 0.2106)}.
}
\value{
The objective function evaluated at \code{x} (a numeric vector of length one).
}
\references{
Gilli, M., Maringer, D. and Schumann, E. (2011) \emph{Numerical Methods and Optimization in Finance}. Elsevier.
\url{http://www.elsevierdirect.com/product.jsp?isbn=9780123756626} (Chapter 10)
}
\author{
Enrico Schumann
}
\section{Warning}{
These test functions are \emph{artificial} problems. It is typically not too helpful to fine-tune a method on such functions. (This would
be like memorising all the answers for a particular multiple-choice test.) The functions' main purpose is checking the numerical
implementation of algorithms.
}


\seealso{
\code{\link{DEopt}}, \code{\link{PSopt}}
}
\examples{
## persp for two-dimensional x

## *Ackley*
n <- 100L; surf <- matrix(NA, n, n)
x1 <- seq(from = -2, to = 2, length.out = n)
for (i in 1L:n) for (j in 1L:n)
    surf[i, j] <- tfAckley(c(x1[i], x1[j]))
persp(x1, x1, -surf, phi = 30, theta = 30, expand = 0.5,
      col = "goldenrod1", shade = 0.2, ticktype = "detailed",
      xlab = "x1", ylab = "x2", zlab = "-f", main = "Ackley (-f)",
      border = NA)

## *Trefethen*
n <- 100L; surf <- matrix(NA, n, n)
x1 <- seq(from = -10, to = 10, length.out = n)
for (i in 1L:n) for (j in 1L:n)
    surf[i, j] <- tfTrefethen(c(x1[i], x1[j]))
persp(x1, x1, -surf, phi = 30, theta = 30, expand = 0.5,
      col = "goldenrod1", shade = 0.2, ticktype = "detailed",
      xlab = "x1", ylab = "x2", zlab = "-f", main = "Trefethen (-f)",
      border = NA)
}
\keyword{optimize}
