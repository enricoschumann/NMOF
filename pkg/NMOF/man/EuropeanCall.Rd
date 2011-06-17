\name{EuropeanCall}
\alias{EuropeanCall}
\alias{EuropeanCallBE}
\title{
Computing Prices of European Calls with a Binomial Tree
}
\description{
Computes the fair value of a European Call with the binomial tree of Cox, Ross and Rubinstein.
}
\usage{
EuropeanCall(S0, X, r, tau, sigma, M = 101)
EuropeanCallBE(S0, X, r, tau, sigma, M = 101)
}
\arguments{
  \item{S0}{current stock price
}
  \item{X}{strike price
}
  \item{r}{risk-free rate
}
  \item{tau}{time to maturity
}
  \item{sigma}{volatility
}
  \item{M}{number of time steps
}
}
\details{
Prices a European Call with the tree approach of Cox, Ross, Rubinstein. 

The algorithm in \code{EuropeanCallBE} does not construct and traverse a tree, but computes the
terminal prices via a binomial expansion (see Higham, 2002).

}
\value{
Returns the value of the call (\code{numeric}).
}
\references{
Gilli, M., Maringer, D. and Schumann, E. (2011) \emph{Numerical Methods and Optimization in Finance}. Elsevier. 
\url{http://www.elsevierdirect.com/product.jsp?isbn=9780123756626}

M. Gilli and Schumann, E. (2010) Implementing Binomial Trees. \acronym{COMISEF} Working Paper Series No. 008. 
\url{http://comisef.eu/?q=working_papers}

Higham, D. (2002) Nine Ways to Implement the Binomial Method for Option Valuation in MATLAB. \emph{SIAM Review}, \bold{44}(4), pp. 661--677. 
\url{http://personal.strath.ac.uk/d.j.higham/papers/binom.pdf} .  
}
\author{
Enrico Schumann
}

\seealso{
\code{\link{callHestoncf}}
}
\examples{
## price
EuropeanCall(S0 = 100, X = 100, r = 0.02, tau = 1, sigma = 0.20, M = 50)
EuropeanCallBE(S0 = 100, X = 100, r = 0.02, tau = 1, sigma = 0.20, M = 50)

## a Greek: delta 
h <- 1e-8
C1 <- EuropeanCall(S0 = 100 + h, X = 100, r = 0.02, tau = 1, 
                   sigma = 0.20, M = 50)
C2 <- EuropeanCall(S0 = 100    , X = 100, r = 0.02, tau = 1, 
                   sigma = 0.20, M = 50)
(C1 - C2) / h
}

