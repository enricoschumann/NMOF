\name{callCF}
\alias{callCF}
\alias{cfBSM}
\alias{cfHeston}
\alias{cfBates}
\alias{cfMerton}
\alias{cfVG}

\title{
Price a Plain-Vanilla Call with the Characteristic Function
}
\description{
Price a European plain-vanilla call with the characteric function.
}
\usage{
callCF(cf, S, X, tau, r, q = 0, tauD = 0, D = 0, ..., implVol = FALSE)

cfBSM(om, S, tau, r, q, v)

cfMerton(om, S, tau, r, q, v, lambda, muJ, vJ)

cfBates(om, S, tau, r, q, v0, vT, rho, k, sigma, lambda, muJ, vJ)

cfHeston(om, S, tau, r, q, v0, vT, rho, k, sigma)

cfVG(om, S, tau, r, q, nu, theta, sigma)
}
\arguments{
  \item{cf}{
characteristic function
}
  \item{S}{
spot
}
  \item{X}{
strike
}
  \item{tau}{
time to maturity
}
  \item{r}{
rate
}
  \item{q}{
the dividend rate
}
  \item{tauD}{
time to div. \strong{not implemented}
}
  \item{D}{
a vector of dividends. \strong{not implemented}
}
  \item{\dots}{
arguments passed to the characteristic function
}
  \item{implVol}{
compute implied vol?}
\item{om}{a (usually complex) argument}
\item{v0}{a}
\item{vT}{a}
\item{v}{a}
\item{rho}{a}
\item{k}{a}
\item{sigma}{a}
\item{lambda}{a}
\item{muJ}{a}
\item{vJ}{a}
\item{nu}{a}
\item{theta}{a}
}
\details{
\emph{(Experimental.)}
  
The function computes the value of a plain vanilla European call under
different models, using the representation of Bakshi/Madan. Put values
can be computed through put--call-parity. If \code{implVol} is
\code{TRUE}, the function will compute the implied volatility necessary
to obtain the same price under Black--Scholes--Merton. The implied
volatility is computed with \code{\link{uniroot}} from the \pkg{stats}
package.

}
\value{
Returns the value of the call (numeric) under the respective model or,
if \code{implVol == TRUE}, a list of the value and the implied
volatility.
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
\code{\link{callHestoncf}}
}
\examples{
S <- 100; X <- 100; tau <- 1
r   <- 0.02
q   <- 0.08
v0  <- 0.2^2  # variance, not volatility
vT  <- 0.2^2  # 
v <- vT
rho <- -0.3
k   <- .2
sigma <- 0.3
#
lambda <- 0.1
muJ <- -0.2
vJ <- 0.1^2

## get Heston price and BSM implied volatility
callHestoncf(S, X, tau, r, q, v0, vT, rho, k, sigma, implVol = FALSE)
callCF(cf = cfHeston, S=S, X=X, tau=tau, r=r, q = q, tauD = 0, D = 0, 
       v0 = v0, vT = vT, rho = rho, k = k, sigma = sigma, implVol = FALSE)

callCF(cf = cfBSM, S=S, X=X, tau = tau, r=r, q = q, tauD = 0, D = 0, 
       v = v, implVol = TRUE)

callCF(cf = cfBates, S = S, X = X, tau = tau, 
    r = r, q = q, tauD = 0, D = 0, 
    v0 = v0, vT = vT, rho = rho, k = k, sigma = sigma,
    lambda = lambda, muJ = muJ, vJ = vJ, implVol = FALSE)

callCF(cf = cfMerton, S = S, X = X, tau = tau, 
    r = r, q = q, tauD = 0, D = 0, 
    v = v, lambda = lambda, muJ = muJ, vJ = vJ, implVol = FALSE)

## variance gamma
nu <- 0.1; theta <- -0.1; sigma <- 0.15
callCF(cf = cfVG, S = S, X = X, tau = tau, 
       r = r, q = q, tauD = 0, D = 0, 
       nu = nu, theta = theta, sigma = sigma, implVol = FALSE)
}
