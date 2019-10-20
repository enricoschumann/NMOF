\name{mvFrontier}
\alias{mvFrontier}
\alias{mvPortfolio}
\title{
  Computing Mean--Variance Efficient Portfolios
}
\description{
  Compute mean--variance efficient portfolios and efficient frontiers.
}
\usage{
mvFrontier(m, var, wmin = 0, wmax = 1, n = 50, rf = NA)
mvPortfolio(m, var, min.return, wmin = 0, wmax = 1)
}
\arguments{
  \item{m}{
    vector of expected returns
}
  \item{var}{
    expected variance--covariance matrix
}
  \item{wmin}{
    numeric: minimum weights
}
  \item{wmax}{
    numeric: maximum weights
}
  \item{n}{
    number of points on the efficient frontier
}
  \item{min.return}{
    minimal required return
}
  \item{rf}{
    risk-free rate
}
}
\details{

   \code{mvPortfolio} computes a single mean--variance efficient
   portfolio, using package \pkg{quadprog}.

   \code{mvFrontier} computes returns, volatilities and
   compositions for portfolios along an efficient frontier.
   If \code{rf} is not \code{NA}, cash is included as an asset.

}
\value{

  For \code{mvPortfolio}, a numeric vector of weights.

  For \code{mvFrontier}, a list of three components:

  \item{return}{returns of portfolios}
  \item{volatility}{volatilities of portfolios}
  \item{weights}{A matrix of portfolio weights.
    Each column holds the weights for one portfolio on the
    frontier. If \code{rf} is specified, an additional row
    is added, providing the cash weight.}

  The \emph{i}-th portfolio on the frontier corresponds
  to the \emph{i}-th elements of \code{return} and
  \code{volatility}, and the \emph{i}-th column of
  \code{portfolio}.

}
\references{
  Gilli, M., Maringer, D. and Schumann, E. (2019) \emph{Numerical
  Methods and Optimization in Finance}. 2nd edition. Elsevier.
  \url{https://www.elsevier.com/books/numerical-methods-and-optimization-in-finance/gilli/978-0-12-815065-8}

  Schumann, E. (2019) Financial Optimisation with R (\pkg{NMOF} Manual).
  \url{http://enricoschumann.net/NMOF.htm#NMOFmanual}
}
\author{
    Enrico Schumann
}
\seealso{
  \code{\link{minvar}}
}
\examples{
na <- 4
vols <- c(0.10, 0.15, 0.20,0.22)
m <- c(0.06, 0.12, 0.09, 0.07)
const_cor <- function(rho, na) {
    C <- array(rho, dim = c(na, na))
    diag(C) <- 1
    C
}
var <- diag(vols) %*% const_cor(0.5, na) %*% diag(vols)

wmax <- 2.5           # maximum holding size
wmin <- 0.0          # minimum holding size
rf <- 0.02

p1 <- mvFrontier(m, var, wmin = wmin, wmax = wmax, n = 50)
p2 <- mvFrontier(m, var, wmin = wmin, wmax = wmax, n = 50, rf = rf)
plot(p1$volatility, p1$return, pch = 19, cex = 0.5, type = "o",
     xlab = "Expected volatility",
     ylab = "Expected return")
lines(p2$volatility, p2$return, col = grey(0.5))
abline(v = 0, h = rf)
}
