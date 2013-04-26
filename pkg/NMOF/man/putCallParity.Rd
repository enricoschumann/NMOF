\name{putCallParity}
\alias{putCallParity}
\title{
  Put-Call Parity
}
\description{
  Put--call parity
}
\usage{
  putCallParity(what, call, put, S, X, tau, r, q = 0, tauD = 0, D = 0)
}
\arguments{
  \item{what}{character: what to compute. Currently only \code{call} or \code{put} are supported.}
  \item{call}{call price}
  \item{put}{put price}
  \item{S}{underlier}
  \item{X}{strike}
  \item{tau}{time to expiry}
  \item{r}{interest rate}
  \item{q}{dividend rate}
  \item{tauD}{numeric vector: time to dividend}
  \item{D}{numeric vector: dividends}    
}
\details{
  Put--call parity only works for European options. The function is
  vectorised (like \code{\link{vanillaOptionEuropean}}), except for
  dividends.
}
\value{
  Numeric vector.
}
\references{

  Gilli, M., Maringer, D. and Schumann, E. (2011) \emph{Numerical
  Methods and Optimization in Finance}. Elsevier.
  \url{http://www.elsevierdirect.com/product.jsp?isbn=9780123756626}

}
\author{
  Enrico Schumann
}
\examples{
S <- 100; X <- 100; tau <- 1; r <- 0.02; q <- 0.0;
vol <- 0.3; D <- 20; tauD <- 0.5 
call <- vanillaOptionEuropean(S, X, tau, r, q, vol^2,
                              tauD = tauD, D = D, type = "call")$value
put  <- vanillaOptionEuropean(S, X, tau, r, q, vol^2,
                              tauD = tauD, D = D, type = "put")$value

## recover the call from the put (et vice versa)
all.equal(call, putCallParity("call", put = put,  S=S, X=X, tau=tau,
                              r=r, q=q, tauD=tauD, D=D))
all.equal(put, putCallParity("put",  call = call, S=S, X=X, tau=tau,
                             r=r, q=q, tauD=tauD, D=D))


## Black--Scholes--Merton with with 'callCF'
S <- 100; X <- 90; tau <- 1; r <- 0.02; q <- 0.08
v <- 0.2^2  ## variance, not volatility

(ccf <- callCF(cf = cfBSM, S = S, X = X, tau = tau, r = r, q = q,
                 v = v, implVol = TRUE))
all.equal(ccf$value, 
          vanillaOptionEuropean(S, X, tau, r, q, v, type = "call")$value)
all.equal(
  putCallParity("put", call=ccf$value, S=S, X=X, tau=tau, r=r, q=q),
  vanillaOptionEuropean(S, X, tau, r, q, v, type = "put")$value)
}
