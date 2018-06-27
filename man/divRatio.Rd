\name{divRatio}
\alias{divRatio}
\title{
  Diversification Ratio
}
\description{
  Compute the diversification ratio of a portfolio.
}
\usage{
divRatio(w, var)
}
\arguments{
  \item{w}{
     numeric: a vector of weights
}
  \item{var}{
    numeric matrix: the variance--covariance matrix
}
}
\details{

  The function provides an efficient implementation of
  the diversification ratio, suitable for optimisation.

}
\value{
  a numeric vector of length one
}
\references{
  Yves Choueifaty and Yves Coignard (2008) Toward Maximum
  Diversification. 
  \emph{Journal of Portfolio Management} \strong{35}(1), 40--51.
}
\author{
  Enrico Schumann
}
\seealso{
  \code{pm}, \code{drawdown}
}
\examples{
na  <- 10     ## number of assets
rho <- 0.5    ## correlation
v_min <- 0.2  ## minimum vol
v_max <- 0.4  ## maximum vol

## set up a covariance matrix S
C <- array(rho, dim = c(na,na))
diag(C) <- 1
vols <- seq(v_min, v_max, length.out = na)
S <- outer(vols, vols) * C

w <- rep(1/na, na)  ## weights
divRatio(w, S)

}
