\name{mc}
\alias{mc}
\alias{gbm}
\alias{gbb}
\title{
    Option Pricing via Monte-Carlo Simulation
}
\description{

    Functions to calculate the theoretical prices of options through
    simulation.

}
\usage{
gbm(npaths, timesteps, r, v, tau, S0,
    exp.result = TRUE, antithetic = FALSE)
gbb(npaths, timesteps, S0, ST, v, tau,
    log = FALSE, exp.result = TRUE)
}
\arguments{
  \item{npaths}{
    the number of paths
  }
  \item{timesteps}{
    timesteps per path
  }

  \item{r}{the mean per unit of time
  }

  \item{v}{the variance per unit of time
  }

  \item{tau}{time
  }

  \item{S0}{initial value
  }

  \item{ST}{final value of Brownian bridge
  }

  \item{log}{logical: construct bridge from log series?
  }

  \item{exp.result}{logical: compute \code{\link{exp}} of the final
    path, or return log values?}

  \item{antithetic}{%
    logical: if \code{TRUE}, random numbers for only
    \code{npaths/2} are drawn, and the random numbers
    are mirrored
  }
}
\details{

\code{gbm} generates sample paths of geometric Brownian motion.

\code{gbb} generates sample paths of a Brownian bridge by first creating
paths of Brownian motion \code{W} from time \code{0} to time \code{T},
with \code{W_0} equal to zero.  Then, at each \code{t}, it subtracts \code{t/T
* W_T} and adds \code{S0*(1-t/T)+ST*(t/T)}.

}
\value{

    A matrix of sample paths; each column contains the price path of an
    asset. Even with only a single time-step, the matrix will have two
    rows (the first row is \code{S0}).

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




## a fanplot
steps <- 100
paths <- results <- gbm(1000, steps, r = 0, v = 0.2^2,
                        tau = 1, S0 = 100)

levels <- seq(0.01, 0.49, length.out = 20)
greys  <- seq(0.9,  0.50, length.out = length(levels))

## start with an empty plot ...
plot(0:steps, rep(100, steps+1), ylim = range(paths),
     xlab = "", ylab = "", lty = 0, type = "l")

## ... and add polygons
for (level in levels) {

    l <- apply(paths, 1, quantile, level)
    u <- apply(paths, 1, quantile, 1 - level)
    col <- grey(greys[level == levels])
    polygon(c(0:steps, steps:0), c(l, rev(u)),
            col = col, border = NA)

    ## add border lines
    ## lines(0:steps, l, col = grey(0.4))
    ## lines(0:steps, u, col = grey(0.4))
}

}
\keyword{datagen}
\keyword{distribution}
