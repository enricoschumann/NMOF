\name{MonteCarlo}
\alias{gBrownianMotion}
\alias{gBrownianBridge}
\alias{mc}
\title{
    Pricing Options via Simulation
}
\description{

    Functions to calculate the theoretical prices of options through
    simulation.

}
\usage{
gBrownianMotion(npaths, timesteps, mean, sd, tau, S0)
gBrownianBridge(npaths, timesteps, S0, ST, sd, tau)
mc(paths, payoff, \dots)
}
\arguments{
  \item{npaths}{
    the number of paths
}
  \item{timesteps}{
      timesteps per path
}
  \item{mean}{
x
}
  \item{sd}{
x
}
  \item{tau}{
  x
}
  \item{S0}{

}
  \item{ST}{

  }
  \item{paths}{
       a numeric matrix; each column contains the price path of an asset
}
  \item{payoff}{
       a function
}

}

\details{

generate a brownian motion W from 0 to T

1) at each t, subtract t/T * W_T
2) at each t, add a(1-t/T)+b(t/T)  

    
    
}
\value{

    a matrix
    
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
npaths <- 20
timesteps <- 3
sd <- 0.5
S0 <- 100
ST <- 100
tau <- 1
mean <- 0.01
sd <- 0.25

gBrownianMotion(npaths, timesteps, mean, sd, tau, S0=100)
    
}

