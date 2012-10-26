\name{callHestoncf}
\alias{callHestoncf}
\title{
Price of a European Call under the Heston Model
}
\description{
Computes the price of a European Call under the Heston model (and the
equivalent Black--Scholes--Merton volatility)
}
\usage{
callHestoncf(S, X, tau, r, q, v0, vT, rho, k, sigma, implVol = FALSE)
}
\arguments{
  \item{S}{current stock price}
  \item{X}{strike price}
  \item{tau}{time to maturity}
  \item{r}{risk-free rate}
  \item{q}{dividend rate}
  \item{v0}{current variance}
  \item{vT}{long-run variance}
  \item{rho}{correlation between spot and variance}
  \item{k}{speed of mean-reversion}
  \item{sigma}{volatility of variance. A value smaller than 0.01 is replaced with 0.01.}
  \item{implVol}{compute equivalent Black--Scholes--Merton volatility? Default is FALSE.}
}
\details{
The function computes the value of a plain vanilla European call under
the Heston model. Put values can be computed through put--call-parity.
If \code{implVol} is \code{TRUE}, the function will compute the
implied volatility necessary to obtain the same price under
Black--Scholes--Merton. The implied volatility is computed with
\code{\link{uniroot}} from the \pkg{stats} package.

Note that the function takes variances as inputs (not volatilities).
}
\value{
  Returns the value of the call (numeric) under the Heston model or, if
  \code{implVol} is \code{TRUE}, a list of the value and the implied
  volatility.
}
\note{
  If \code{implVol} is \code{TRUE}, the function will return a list with
  elements named \code{value} and \code{impliedVol}. Prior to version
  0.26-3, the first element was named \code{callPrice}.
}
\references{
Gilli, M., Maringer, D. and Schumann, E. (2011) \emph{Numerical Methods
  and Optimization in Finance}. Elsevier.
\url{http://www.elsevierdirect.com/product.jsp?isbn=9780123756626}

Heston, S.L. (1993) A Closed-Form Solution for Options with Stochastic
Volatility with Applications to Bonds and Currency options.
\emph{Review of Financial Studies} \strong{6}(2), 327--343.
}

\author{
Enrico Schumann
}

\seealso{
\code{\link{callCF}}\code{\link{EuropeanCall}}
}
\examples{
S <- 100; X <- 100; tau <- 1; r <- 0.02; q <- 0.01
v0  <- 0.2^2  ## variance, not volatility
vT  <- 0.2^2  ## variance, not volatility
rho <- -0.7; k <- 0.2; sigma <- 0.5

## get Heston price and BSM implied volatility
result <- callHestoncf(S = S, X = X, tau = tau, r = r, q = q,
                       v0 = v0, vT = vT, rho = rho, k = k,
                       sigma = sigma, implVol = TRUE)

## Heston price
result[[1L]]

## price BSM with implied volatility
vol <- result[[2L]]
d1 <- (log(S/X) + (r - q + vol^2 / 2)*tau) / (vol*sqrt(tau))
d2 <- d1 - vol*sqrt(tau)
callBSM <- S * exp(-q * tau) * pnorm(d1) -
           X * exp(-r * tau) * pnorm(d2)
callBSM  ## should be (about) the same as result[[1L]]
}

