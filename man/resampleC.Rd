\name{resampleC}
\alias{resampleC}
\title{
  Resample with Specified Rank Correlation
}
\description{
  Resample with replacement from a number of vectors; the sample will
  have a specified rank correlation.
}
\usage{
resampleC(..., size, cormat)
}
\arguments{
  \item{\dots}{
    numeric vectors; they need not have the same length.
  }
  \item{size}{
    an integer: the number of samples to draw
  }
  \item{cormat}{
    the rank correlation matrix
  }
}
\details{
  See Gilli, Maringer and Schumann (2011), Section 7.1.2. The function
  samples with replacement from the vectors passed through
  \code{\dots}. The resulting samples will have an (approximate) rank
  correlation as specified in \code{cormat}.

  The function uses the eigenvalue decomposition to generate the
  correlation; it will not break down in case of a semidefinite
  matrix. If an eigenvalue of \code{cormat} is smaller than zero, a
  warning is issued (but the function proceeds).

}
\value{
  a numeric matrix with \code{size} rows. The columns contain the
  samples; hence, there will be as many columns as vectors passed
  through \code{\dots}.
}
\references{

  Gilli, M., Maringer, D. and Schumann, E. (2011) \emph{Numerical
  Methods and Optimization in Finance}. Elsevier.
  \url{http://www.elsevierdirect.com/product.jsp?isbn=9780123756626}

}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{repairMatrix}}
}
\examples{
## a sample
v1 <- rnorm(20)
v2 <- runif(50)
v3 <- rbinom(100, size = 50, prob = 0.4)

## a correlation matrix
cormat <- array(0.5, dim = c(3, 3))
diag(cormat) <- 1

cor(resampleC(a = v1, b = v2, v3, size = 100, cormat = cormat),
    method = "spearman")
}
\keyword{datagen}
\keyword{distribution}
