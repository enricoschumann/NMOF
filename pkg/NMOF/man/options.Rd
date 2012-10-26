\name{vanillaOptionEuropean}
\alias{vanillaOptionEuropean}
\alias{vanillaOptionAmerican}
\alias{vanillaOptionImpliedVol}
\title{
  Pricing Plain-Vanilla Options (European and American)
}
\description{
  Functions to calculate the theoretical prices and (some) Greeks for plain vanilla options.
}
\usage{
vanillaOptionEuropean(S, X, tau, r, q, v, tauD = 0, D = 0,
                      type = "call", greeks = TRUE)
vanillaOptionAmerican(S, X, tau, r, q, v, tauD = 0, D = 0,
                      type = "call", greeks = TRUE, M = 101)

vanillaOptionImpliedVol(exercise = "european", price, S, X, tau, r,
                        q = 0, tauD = 0, D = 0, type = "call",
                        M = 101,
                        uniroot.control = list(), uniroot.info = FALSE)
}
\arguments{
  \item{S}{
    spot
}
  \item{X}{
    strike
}
  \item{tau}{
    time-to-maturity in years
}
  \item{r}{
    risk-free rate
}
  \item{q}{
    continuous dividend yield, see Details.
}
  \item{v}{
    variance (volatility squared)
}
  \item{tauD}{
    vector of times to dividends in years
}
  \item{D}{
    vector of dividends (in currency units); default is no dividends.
}
  \item{type}{
    call or put; default is call.
}
  \item{greeks}{
    compute Greeks? Defaults to \code{TRUE}. But see Details fpr
                        American options.
  }
  \item{M}{
    number of time steps in the tree
  }
  \item{exercise}{
    \code{european} (default) or \code{american}
  }
  \item{price}{
    numeric; the observed price to be recovered through choice of volatility.
  }
  \item{uniroot.control}{A list. If there are elements named
    \code{interval}, \code{tol} or \code{maxiter}, these are passed to
    \code{uniroot}. Any other elements of the list are ignored.}
  \item{uniroot.info}{logical; default is \code{FALSE}. If \code{TRUE},
    the function will return the information returned by
    \code{uniroot}. See paragraph Value below.}

}

\details{

For European options the formula of Messrs Black, Scholes and Merton is
used. It can be used for equities (set \code{q} equal to the dividend
yield), futures (Black, 1976; set \code{q} equal to \code{r}),
currencies (Garman and Kohlhagen, 1983; set \code{q} equal to the
foreign risk-free rate). For future-style options (eg, the German Bund
future), set \code{q} and \code{r} equal to zero.

The Greeks here are in their raw (\sQuote{textbook}) form. In practice,
they are often adjusted: theta is negative and is divided by 365. Vega
and rho are divided by 100 to give the sensitivity for one percentage
point move in volatility/the interest rate.

For European options the Greeks are computed through the respective
analytic expressions. For American options only Delta, Gamma and Theta
are computed.

For the European-type options, the function understands vectors of
inputs, except for dividends. American options are priced via a
Cox-Ross-Rubinstein tree; no vectorisation is possible here.

The implied volatility is computed with \code{\link{uniroot}} from the
\pkg{stats} package (the default search interval is \code{c(0.00001,
2)}; it can be changed through \code{uniroot.control}).

Dividends (\code{D}) are modelled via the escrowed-dividend model.

}
\value{
  Returns the price (a numeric vector of length one) if \code{greeks} is
  \code{FALSE}, else returns a list.
}
\note{
  If \code{greeks} is (are?) \code{TRUE}, the function will return a list with
  named elements (\code{value}, \code{delta} and so on). Prior to version
  0.26-3, the first element was named \code{price}.
}
\references{
Gilli, Maringer, Schumann, 2011.
}
\author{
Enrico Schumann
}
\seealso{
\code{\link{EuropeanCall}}, code{\link{callCF}}
}
\examples{
S <- 100; X <- 100; tau <- 1; r <- 0.02; q <- 0.06; vol <- 0.3
unlist(vanillaOptionEuropean(S, X, tau, r, q, vol^2, type = "put"))

S <- 100; X <- 110; tau <- 1; r <- 0.1; q <- 0.06; vol <- 0.3; type <- "put"
unlist(vanillaOptionAmerican(S, X, tau, r, q, vol^2, type = type,
                             greeks = TRUE))

## compute rho for 1\% move
h <- 0.01
(vanillaOptionAmerican(S, X, tau, r + h, q, vol^2,
    type = type, greeks = FALSE) -
 vanillaOptionAmerican(S, X, tau, r, q, vol^2,
    type = type, greeks = FALSE)) / (h*100)

## compute vega for 1\% move
h <- 0.01
(vanillaOptionAmerican(S, X, tau, r, q,(vol + h)^2,
    type = type, greeks = FALSE) -
 vanillaOptionAmerican(S, X, tau, r, q, vol^2,
    type = type, greeks = FALSE)) / (h*100)



S <- 100; X <- 100
tau <- 1; r <- 0.05; q <- 0.00
D <- c(1,2); tauD <- c(0.3,.6)
type <- "put"
v <- 0.245^2  ## variance, not volatility

p <- vanillaOptionEuropean(S = S, X = X, tau, r, q, v = v,
                           tauD = tauD, D = D, type = type, greeks = FALSE)
vanillaOptionImpliedVol(exercise = "european", price = p,
     S = S, X = X, tau = tau, r = r, q = q, tauD = tauD, D = D, type = type)

p <- vanillaOptionAmerican(S = S, X = X, tau, r, q, v = v,
     tauD = tauD, D = D, type = type, greeks = FALSE)
vanillaOptionImpliedVol(exercise = "american", price = p,
     S = S, X = X, tau = tau, r = r, q = q, tauD = tauD, D = D, type =
     type, uniroot.control = list(interval = c(0.01, 0.5)))


## compute implied q
S <- 100; X <- 100
tau <- 1; r <- 0.05; q <- 0.072
v <- 0.22^2  ## variance, not volatility

call <- vanillaOptionEuropean(S=S, X = X, tau=tau, r=r, q=q, v=v,
         type = "call", greeks = FALSE)
put  <- vanillaOptionEuropean(S=S, X = X, tau=tau, r=r, q=q, v=v,
         type = "put", greeks = FALSE)

# ... the simple way
-(log(call + X * exp(-tau*r) - put) - log(S)) / tau

# ... the complicated way :-)
volDiffCreate <- function(exercise, call, put, S, X, tau, r) {
    f <- function(q) {
        cc <- vanillaOptionImpliedVol(exercise = exercise, price = call,
            S = S, X = X, tau = tau, r = r, q = q, type = "call")
        pp <- vanillaOptionImpliedVol(exercise = exercise, price = put,
            S = S, X = X, tau = tau, r = r, q = q, type = "put")
        abs(cc - pp)
    }
    f
}
f <- volDiffCreate(exercise = "european",
                   call = call, put = put, S = S, X = X, tau = tau, r)
optimise(f,interval = c(0, 0.2))$minimum
}

