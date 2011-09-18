\name{xwGauss}
\alias{xwGauss}
\alias{changeInterval}
\title{
Integration of Gauss-type
}
\description{
Compute nodes and weights for Gauss integration.
}
\usage{
xwGauss(n, method = "legendre")
changeInterval(nodes, weights, oldmin, oldmax, newmin, newmax)
}
\arguments{
  \item{n}{
number of nodes
}
  \item{method}{
currently only \code{legendre} is supported (anything else gives an error)
}
% changeInterval
  \item{nodes}{
the nodes (a numeric vector)
}
  \item{weights}{
the weights (a numeric vector)
}
  \item{oldmin}{
the minimum of the interval (typically as tabulated)
}
  \item{oldmax}{
the maximum of the interval (typically as tabulated)
}
  \item{newmin}{
the desired minimum of the interval
}
  \item{newmax}{
the desired minimum of the interval
}

}
\details{
  \code{xwGauss} computes nodes and weights for integration for the interval -1
  to 1. It uses the method of Golub and Welsch (1969).

  \code{changeInterval} is a utility that transforms nodes and weights
  to an arbitrary interval.

}
\value{
a list with two elements
\item{weights}{a numeric vector}
\item{nodes}{a numeric vector}
}
\references{
Gilli, M., Maringer, D. and Schumann, E. (2011) \emph{Numerical Methods and Optimization in Finance}. Elsevier.
\url{http://www.elsevierdirect.com/product.jsp?isbn=9780123756626}

Golub, G.H. and Welsch, J.H. (1969). Calculation of Gauss Quadrature Rules. \emph{Mathematics of Computation},
\bold{23}(106), pp. 221--230+s1--s10.
}
\author{
Enrico Schumann
}

\seealso{
\code{\link{callHestoncf}}
}
\examples{
## examples from Gilli/Maringer/Schumann (2011), ch. 15

## a test function
f1 <- function(x) exp(-x)
m <- 5; a <- 0; b <- 5
h <- (b - a)/m

## rectangular rule -- left
w <- h; k <- 0:(m-1); x <- a + k * h
sum(w * f1(x))

## rectangular rule -- right
w <- h; k <- 1:m ; x <- a + k * h
sum(w * f1(x))

## midpoint rule
w <- h; k <- 0:(m-1); x <- a + (k + 0.5)*h
sum(w * f1(x))

## trapezoidal rule
w <- h
k <- 1:(m-1)
x <- c(a, a + k*h, b)
aux <- w * f1(x)
sum(aux) - (aux[1] + aux[length(aux)])/2

## R's integrate (from package stats)
integrate(f1, lower = a,upper = b)

## Gauss--Legendre
temp <- xwGauss(m)
temp <- changeInterval(temp$nodes, temp$weights,
                       oldmin = -1, oldmax = 1, newmin =  a, newmax = b)
x <- temp$nodes; w <- temp$weights
sum(w * f1(x))
}

