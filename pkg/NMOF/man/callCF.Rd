\name{callCF}
\alias{callCF}
\alias{cfBSM}
\alias{cfHeston}
\alias{cfBates}
\alias{cfMerton}
\alias{cfVG}
\title{Price a Plain-Vanilla Call with the Characteristic Function}
\description{
  Price a European plain-vanilla call with the characteric function.
}
\usage{
callCF(cf, S, X, tau, r, q = 0, ...,
       implVol = FALSE, uniroot.control = list(), uniroot.info = FALSE)
cfBSM(om, S, tau, r, q, v)
cfMerton(om, S, tau, r, q, v, lambda, muJ, vJ)
cfBates(om, S, tau, r, q, v0, vT, rho, k, sigma, lambda, muJ, vJ)
cfHeston(om, S, tau, r, q, v0, vT, rho, k, sigma)
cfVG(om, S, tau, r, q, nu, theta, sigma)
}
\arguments{
  \item{cf}{characteristic function}
  \item{S}{spot}
  \item{X}{strike}
  \item{tau}{time to maturity}
  \item{r}{the interest rate}
  \item{q}{the dividend rate}
  \item{\dots}{arguments passed to the characteristic function}
  \item{implVol}{logical: compute implied vol?}
  \item{uniroot.control}{A list. If there are elements named
    \code{interval}, \code{tol} or \code{maxiter}, these are passed to
    \code{uniroot}. Any other elements of the list are ignored.}
  \item{uniroot.info}{logical; default is \code{FALSE}. If \code{TRUE},
    the function will return the information returned by
    \code{uniroot}. See paragraph Value below.}
  \item{om}{a (usually complex) argument}
  \item{v0}{a numeric vector of length one}
  \item{vT}{a numeric vector of length one}
  \item{v}{a numeric vector of length one}
  \item{rho}{a numeric vector of length one}
  \item{k}{a numeric vector of length one}
  \item{sigma}{a numeric vector of length one}
  \item{lambda}{a numeric vector of length one}
  \item{muJ}{a numeric vector of length one}
  \item{vJ}{a numeric vector of length one}
  \item{nu}{a numeric vector of length one}
  \item{theta}{a numeric vector of length one}
}
\details{

  The function computes the value of a plain vanilla European call under
  different models, using the representation of Bakshi/Madan. Put values
  can be computed through put--call-parity.

  If \code{implVol} is \code{TRUE}, the function will compute the implied
  volatility necessary to obtain the same price under
  Black--Scholes--Merton. The implied volatility is computed with
  \code{\link{uniroot}} from the \pkg{stats} package (the default search
  interval is \code{c(0.00001, 2)}; it can be changed through
  \code{uniroot.control}).

}
\value{
  Returns the value of the call (numeric) under the respective model or,
  if \code{implVol == TRUE}, a list of the value and the implied
  volatility. (If, in addition, \code{uniroot.info} is \code{TRUE}, the
  information provided by \code{\link{uniroot}} is also returned.)
 
}
\references{
  Bates, David S. (1996) Jumps and Stochastic Volatility: Exchange Rate
  Processes Implicit in Deutsche Mark Options. \emph{Review of
    Financial Studies} \strong{9}(1), 69--107.

  Gilli, M., Maringer, D. and Schumann, E. (2011) \emph{Numerical
    Methods and Optimization in Finance}. Elsevier.
  \url{http://www.elsevierdirect.com/product.jsp?isbn=9780123756626}

  Heston, S.L. (1993) A Closed-Form Solution for Options with Stochastic
  Volatility with Applications to Bonds and Currency options.
  \emph{Review of Financial Studies} \strong{6}(2), 327--343.

}
\author{
  Enrico Schumann
}

\seealso{
  \code{\link{callHestoncf}}
}
\examples{
S <- 100; X <- 100; tau <- 1
r <- 0.02; q <- 0.08
v0 <- 0.2^2  ## variance, not volatility
vT <- 0.2^2  ## variance, not volatility
v <- vT
rho <- -0.3; k <- .2
sigma <- 0.3

## jump parameters (Merton and Bates)
lambda <- 0.1
muJ <- -0.2
vJ <- 0.1^2

## get Heston price and BSM implied volatility
callHestoncf(S, X, tau, r, q, v0, vT, rho, k, sigma, implVol = FALSE)
callCF(cf = cfHeston, S=S, X=X, tau=tau, r=r, q = q,
       v0 = v0, vT = vT, rho = rho, k = k, sigma = sigma, implVol = FALSE)

## Black-Scholes-Merton
callCF(cf = cfBSM, S=S, X=X, tau = tau, r = r, q = q,
       v = v, implVol = TRUE)

## Bates
callCF(cf = cfBates, S = S, X = X, tau = tau, r = r, q = q,
       v0 = v0, vT = vT, rho = rho, k = k, sigma = sigma,
       lambda = lambda, muJ = muJ, vJ = vJ, implVol = FALSE)

## Merton
callCF(cf = cfMerton, S = S, X = X, tau = tau, r = r, q = q,
       v = v, lambda = lambda, muJ = muJ, vJ = vJ, implVol = FALSE)

## variance gamma
nu <- 0.1; theta <- -0.1; sigma <- 0.15
callCF(cf = cfVG, S = S, X = X, tau = tau, r = r, q = q,
       nu = nu, theta = theta, sigma = sigma, implVol = FALSE)
}
