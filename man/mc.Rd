\name{mc}
\alias{gbm}
\alias{gbb}
\title{
    Pricing Options via Simulation
}
\description{

    Functions to calculate the theoretical prices of options through
    simulation.

}
\usage{
gbm(npaths, timesteps, r, v, tau, S0)
gbb(npaths, timesteps, S0, ST, v, tau)
}
\arguments{
  \item{npaths}{
    the number of paths
  }
  \item{timesteps}{
    timesteps per path
  }
  \item{r}{
    the mean per unit of time
  }
  \item{v}{
    the variance per unit of time
  }
  \item{tau}{
    time
  }
  \item{S0}{
    initial value
  }
  \item{ST}{
    final value of Brownian bridge
  }
}
\details{
  
\code{gbm} generates sample paths of geometric Brownian motion.

\code{gbb} generates sample paths of a Brownian bridge by first creating
paths of Brownian motion \code{W} from time \code{0} to time \code{T},
with \code{W_0} equal to zero.  Then, at each \code{t}, it subtracts t/T
* W_T and adds S0*(1-t/T)+ST*(t/T).
      
}
\value{

    A matrix of sample paths; each column contains the price path of an
    asset. Even with only a single time-step, the matrix will have two
    rows (the first row is \code{S0}).
    
}
\references{

  Gilli, M., Maringer, D. and Schumann, E. (2011) \emph{Numerical
    Methods and Optimization in Finance}. Elsevier.
    \url{http://www.elsevierdirect.com/product.jsp?isbn=9780123756626}

}
\author{
  Enrico Schumann
}
\seealso{
    \code{\link{vanillaOptionEuropean}}
}
\examples{
## price a European option
## ... parameters
npaths <- 5000   ## increase number to get more precise results
timesteps <- 1
S0   <- 100
ST   <- 100
tau  <- 1
r <- 0.01
v   <- 0.25^2

## ... create paths
paths <- gbm(npaths, timesteps, r, v, tau, S0 = S0)

## ... a helper function
mc <- function(paths, payoff, ...)
    payoff(paths, ...)

## ... a payoff function (European call)
payoff <- function(paths, X, r, tau)
    exp(-r * tau) * mean(pmax(paths[NROW(paths), ] - X, 0))

## ... compute and check
mc(paths, payoff, X = 100, r = r, tau = tau)
vanillaOptionEuropean(S0, X = 100, tau = tau, r = r, v = v)$value


## compute delta via forward difference
## (see Gilli/Maringer/Schumann, ch. 9) 
h <- 1e-6                 ## a small number
rnorm(1)                  ## make sure RNG is initialised
rnd.seed <- .Random.seed  ## store current seed
paths1 <- gbm(npaths, timesteps, r, v, tau, S0 = S0)
.Random.seed <- rnd.seed
paths2 <- gbm(npaths, timesteps, r, v, tau, S0 = S0 + h)

delta.mc <- (mc(paths2, payoff, X = 100, r = r, tau = tau)- 
             mc(paths1, payoff, X = 100, r = r, tau = tau))/h
delta <- vanillaOptionEuropean(S0, X = 100, tau = tau,
                               r = r, v = v)$delta
delta.mc - delta 
}


