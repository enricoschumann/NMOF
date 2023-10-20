\name{minMAD}
\alias{minMAD}
\title{
  Compute Minimum Mean--Absolute-Deviation Portfolios
}
\description{
  Compute minimum mean--absolute-deviation portfolios.
}
\usage{
minMAD(R, wmin = 0, wmax = 1,
       min.return = NULL, m = NULL, demean = TRUE,
       method = "lp",
       groups = NULL, groups.wmin = NULL, groups.wmax = NULL,
       Rglpk.control = list())
}
\arguments{
  \item{R}{
    a matrix of return scenarios: each column represents one asset;
    each row represents one scenario
  }
  \item{wmin}{
    minimum weight
  }
  \item{wmax}{
    maximum weight
  }
  \item{min.return}{
    a minimum required return; ignored if \code{NULL}
  }
  \item{m}{
    a vector of expected returns. If NULL, but  \code{min.return}
    is not \code{NULL}, then column means are used as expected
    returns.
}
  \item{demean}{
    logical. If \code{TRUE}, the columns of \code{R} are demeaned,
    corresponding to an objective function xxxx
}
  \item{method}{
    string. Supported are \code{lp} and \code{ls}.
}
  \item{groups}{
    group definitions
}
  \item{groups.wmin}{
    list of vectors
}
  \item{groups.wmax}{
    list of vectors
}
  \item{Rglpk.control}{
    a list
}
}
\details{

  Compute the minimum mean--absolute-deviation portfolio for a
  given scenario set.

  The function uses \code{\link[Rglpk]{Rglpk_solve_LP}} from package
  \pkg{Rglpk}.

}
\value{
  a vector of portfolio weights
}
\references{

  Konno, H. and Yamazaki, H. (1991) Mean-Absolute Deviation
  Portfolio Optimization Model and Its Applications to Tokyo
  Stock Market. \emph{Management Science}. \strong{37} (5),
  519--531.

}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{minvar}}, \code{\link{minCVaR}}
}
\examples{
na <- 10
ns <- 1000
R <- randomReturns(na = na, ns = ns,
                   sd = 0.01, rho = 0.8, mean = 0.0005)

minMAD(R = R)
minvar(var(R))
}
