\name{trackingPortfolio}
\alias{trackingPortfolio}
\concept{index tracking}
\concept{style analysis}
\concept{portfolio selection}
\title{
  Compute a Tracking Portfolio
}
\description{
  Computes a portfolio similar to a benchmark, e.g. for tracking the
  benchmark's performance or identifying factors.
}
\usage{
trackingPortfolio(var, wmin = 0, wmax = 1,
                  method = "qp", objective = "variance", R,
                  ls.algo = list())
}
\arguments{

  \item{var}{the covariance matrix: a numeric (real),
    symmetric matrix. The first asset is the benchmark.}

  \item{R}{a matrix of returns: each colums holds the
    returns of one asset; each rows holds the returns for
    one observation. The first asset is the benchmark.}

  \item{wmin}{numeric: a lower bound on weights. May also
    be a vector that holds specific bounds for each asset.}

  \item{wmax}{numeric: an upper bound on weights. May also
    be a vector that holds specific bounds for each asset.}

  \item{method}{character. Currently, \code{"qp"} and
    \code{"ls"} are supported.}

  \item{objective}{character. Currently, \code{"variance"}
    and \code{"sum.of.squares"} are supported.}

  \item{ls.algo}{a list of named elements, for settings for
    method \sQuote{\code{ls}}; see Details}

}
\details{

  With method \code{"qp"}, the function uses
  \code{\link[quadprog]{solve.QP}} from package
  \pkg{quadprog}. Because of the algorithm that
  \code{\link[quadprog]{solve.QP}} uses, \code{var} has to
  be positive definite (i.e. must be of full rank).

  With method \code{"ls"}, the function uses
  \code{\link{LSopt}}. Settings can be passed via
  \code{ls.algo}, which corresponds to
  \code{\link{LSopt}}'s argument \code{algo}. Default
  settings are 2000 iterations and \code{printBar},
  \code{printDetail} set to \code{FALSE}.

  \code{R} is needed only when \code{objective} is
  \code{"sum.of.squares"} or \code{method} is
  \sQuote{\code{ls}}. (See Examples.)

}
\value{
  a numeric vector (the portfolio weights)
}
\references{
  Gilli, M., Maringer, D. and Schumann, E. (2019)
  \emph{Numerical Methods and Optimization in Finance}.
  2nd edition. Elsevier. \doi{10.1016/C2017-0-01621-X}

  Schumann, E. (2023) Financial Optimisation with R (\pkg{NMOF} Manual).
  \url{http://enricoschumann.net/NMOF.htm#NMOFmanual}

  Schumann, E. (2020) Return-based tracking portfolios.
  \url{http://enricoschumann.net/notes/return-based-tracking-portfolios.html}

  Sharpe, W. F. (1992) Asset Allocation: Management Style and
  Performance Measurement. Journal of Portfolio Management.
  \strong{18} (2), 7--19.
  \url{https://web.stanford.edu/~wfsharpe/art/sa/sa.htm}
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{minvar}}
}
\examples{
if (requireNamespace("quadprog")) {
    ns <- 120
    R <- randomReturns(na = 1 + 20,
                       ns = ns,
                       sd = 0.03,
                       mean = 0.005,
                       rho = 0.7)

    var <- cov(R)

    sol.qp <- trackingPortfolio(var, wmax = 0.4)
    sol.ls <- trackingPortfolio(var = var, R = R, wmax = 0.4, method = "ls")
    data.frame(QP = round(100*sol.qp, 1),
               LS = round(100*sol.ls, 1))

    sol.qp <- trackingPortfolio(var, R = R, wmax = 0.4,
                                objective = "sum.of.squares")
    sol.ls <- trackingPortfolio(var = var, R = R, wmax = 0.4, method = "ls",
                                objective = "sum.of.squares")
    data.frame(QP = round(100*sol.qp, 1),
               LS = round(100*sol.ls, 1))


    ## same as 'sol.qp' above
    sol.qp.R   <- trackingPortfolio(R = R,
                                    wmax = 0.4,
                                    objective = "sum.of.squares")
    sol.qp.var <- trackingPortfolio(var = crossprod(R),
                                    wmax = 0.4,
                                    objective = "variance")
    ## ==> should be the same
    all.equal(sol.qp.R, sol.qp.var)
}
}
