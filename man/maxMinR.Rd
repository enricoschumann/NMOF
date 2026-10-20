\name{maxMinR}
\alias{maxMinR}
\title{
  Compute Maximum Minimum-Return Portfolios
}
\description{
  Compute maximum minimum-return portfolios.
}
\usage{
maxMinR(R, wmin = 0, wmax = 1,
        min.return = NULL, m = NULL,
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
    minimum weight; currently ignored
  }
  \item{wmax}{
    maximum weight; currently ignored
  }
  \item{min.return}{
    a minimum required return; ignored if \code{NULL}
  }
  \item{m}{
    a vector of expected returns. If \code{NULL}, but
    \code{min.return} is not \code{NULL}, then column
    means are used as expected returns.
  }
  \item{method}{
    string. Supported are \code{lp} and \code{ls}.
    \code{lp} uses package \pkg{Rglpk}, and \code{ls} (local search)
    uses a \code{\link{LSopt}}
  }
  \item{groups}{
    group definitions; currently ignored
  }
  \item{groups.wmin}{
    list of vectors; currently ignored
  }
  \item{groups.wmax}{
    list of vectors; currently ignored
  }
  \item{Rglpk.control}{
    a list
  }
}
\details{

  Compute the maximum minimum-return portfolio for a given
  scenario set.

  The function uses \code{\link[Rglpk]{Rglpk_solve_LP}} from
  package \pkg{Rglpk}.

}
\value{
  a vector of portfolio weights
}
\references{

  Young, M.R. (1998) A Minimax Portfolio Selection Rule with
  Linear Programming Solution. \emph{Management Science}.
  \strong{44} (5), 673--683.

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

maxMinR(R = R)
minvar(var(R))
}
