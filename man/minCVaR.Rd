\name{minCVaR}
\alias{minCVaR}
\concept{portfolio selection}
\title{
  Minimum Conditional-Value-at-Risk (CVaR) Portfolios
}
\description{
  Compute minimum-CVaR portfolios, subject to lower and upper bounds
  on weights.
}
\usage{
minCVaR(R, q = 0.1, wmin = 0, wmax = 1,
        min.return = NULL, m = NULL, 
        method = "Rglpk",
        groups = NULL, groups.wmin = NULL, groups.wmax = NULL,
        Rglpk.control = list())
}
\arguments{
  \item{R}{the scenario matrix: a numeric (real) matrix}

  \item{q}{the Value-at-Risk level: a number between 0 and 0.5}

  \item{wmin}{
    numeric: a lower bound on weights. May also be a vector that holds
    specific bounds for each asset.
}
\item{wmax}{
      numeric: an upper bound on weights. May also be a vector that holds
    specific bounds for each asset.
}

\item{m}{vector of expected returns. Only used if \code{min.return} is
  specified.}

\item{min.return}{
    minimal required return. If \code{m} is not specified, the column
    means of \code{R} are used.
}
\item{method}{
    character. Currently, only \code{"Rglpk"} is supported.
  }
  \item{groups}{
    a list of group definitions
  }
  \item{groups.wmin}{
    a numeric vector
  }
  \item{groups.wmax}{%
    a numeric vector
  }
  \item{Rglpk.control}{
    a list: settings passed to \code{\link[Rglpk]{Rglpk_solve_LP}}
  }
}
\details{%

  Compute the minimum CVaR portfolio for a given scenario set.
  The default method uses the formulation as a Linear Programme,
  as described in Rockafellar/Uryasev (2000).

  The function uses \code{\link[Rglpk]{Rglpk_solve_LP}} from package
  \pkg{Rglpk}.
}
\value{a numeric vector (the portfolio weights); attached is an
  attribute whose name matches the \code{method} name

}
\references{Gilli, M., Maringer, D. and Schumann, E. (2019) \emph{Numerical
  Methods and Optimization in Finance}. 2nd edition. Elsevier.
  \url{https://www.elsevier.com/books/numerical-methods-and-optimization-in-finance/gilli/978-0-12-815065-8}

  Rockafellar, R. T. and Uryasev, S. (2000).
  Optimization of Conditional Value-at-Risk.
  Journal of Risk.
  \strong{2} (3), 21--41.

  Schumann, E. (2020) Minimising Conditional Value-at-Risk (CVaR).
  \url{http://enricoschumann.net/notes/minimising-conditional-var.html}
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{minvar}}
}
\examples{
if (requireNamespace("Rglpk")) {

  ns <- 5000  ## number of scenarios
  na <- 20    ## nunber of assets
  R <- randomReturns(na, ns, sd = 0.01, rho = 0.5)

  minCVaR(R, 0.25)

}
}
