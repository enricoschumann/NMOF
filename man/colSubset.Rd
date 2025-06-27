\name{colSubset}
\alias{colSubset}
\title{
  Full-rank Column Subset
}
\description{
  Select a full-rank subset of columns of a matrix.
}
\usage{
colSubset(x)
}
\arguments{
  \item{x}{
    a numeric matrix
}
}
\details{
  Uses \code{\link{qr}}.
}
\value{
  A list:
  \item{columns}{indices of columns}
  \item{multiplier}{a matrix}
}
\references{
  Gilli, M., Maringer, D. and Schumann, E. (2019)
  \emph{Numerical Methods and Optimization in Finance}.
  2nd edition. Elsevier. \doi{10.1016/C2017-0-01621-X}

  Schumann, E. (2023) Financial Optimisation with R (\pkg{NMOF} Manual).
  \url{https://enricoschumann.net/NMOF.htm#NMOFmanual}

}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{repairMatrix}}
}
\examples{
nc <- 3   ## columns
nr <- 10  ## rows
M <- array(rnorm(nr * nc), dim = c(nr, nc))

C <- array(0.5, dim = c(nc, nc))
diag(C) <- 1
M <- M \%*\% chol(C)
M <- M[ ,c(1,1,1,2,3)]
M

(tmp <- colSubset(M))

C <- cor(M[ ,tmp$columns])
nc <- ncol(C)
nr <- 100
X <- array(rnorm(nr*nc), dim = c(nr, nc))
X <- X \%*\% chol(C)
X <- X \%*\% tmp$multiplier
head(X)
cor(X)
}
