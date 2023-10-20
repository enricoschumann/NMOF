\name{CPPI}
\alias{CPPI}
\title{
  Constant-Proportion Portfolio Insurance
}
\description{

  Simulate constant-proportion portfolio insurance
  (CPPI) for a given price path.

}
\usage{
CPPI(S, multiplier, floor, r, tau = 1, gap = 1)
}
\arguments{
  \item{S}{
    numeric: price path of risky asset
}
  \item{multiplier}{
    numeric
}
  \item{floor}{
    numeric: a percentage, should be smaller than 1
}
  \item{r}{
    numeric: interest rate (per time period tau)
}
  \item{tau}{
    numeric: time periods
}
  \item{gap}{
    numeric: how often to rebalance. 1 means every
    timestep, 2 means every second timestep, and so on.
}
}
\details{

  Based on Dietmar Maringer's MATLAB code (function
  CPPIgap, Listing 9.1).

  See Gilli, Maringer and Schumann, 2011, chapter 9.

}
\value{
A list:
\item{V}{normalised value (always starts at 1)}
\item{C}{cushion}
\item{B}{bond investment}
\item{F}{floor}
\item{E}{exposure}
\item{N}{units of risky asset}
\item{S}{price path}
}
\references{
  Chapter 9 of Gilli, M., Maringer, D. and Schumann, E. (2019)
  \emph{Numerical Methods and Optimization in Finance}.
  2nd edition. Elsevier. \doi{10.1016/C2017-0-01621-X}

  Schumann, E. (2023) Financial Optimisation with R (\pkg{NMOF} Manual).
  \url{http://enricoschumann.net/NMOF.htm#NMOFmanual}
}
\author{
  Original MATLAB code: Dietmar Maringer. \R
  implementation: Enrico Schumann.
}
\examples{
tau <- 2
S <- gbm(npaths = 1, timesteps = tau*256,
         r = 0.02, v = 0.2^2, tau = tau, S0 = 100)

## rebalancing every day
sol <- CPPI(S, multiplier = 5, floor = 0.9, r = 0.01,
            tau = tau, gap = 1)
par(mfrow = c(3,1), mar = c(3,3,1,1))
plot(0:(length(S)-1), S,     type = "s", main = "stock price")
plot(0:(length(S)-1), sol$V, type = "s", main = "value")
plot(0:(length(S)-1), 100*sol$E/sol$V, type = "s",
     main = "\% invested in risky asset")

## rebalancing every 5th day
sol <- CPPI(S, multiplier = 5, floor = 0.9, r = 0.01,
            tau = tau, gap = 5)
par(mfrow = c(3,1), mar = c(3,3,1,1))
plot(0:(length(S)-1), S,     type = "s", main = "stock price")
plot(0:(length(S)-1), sol$V, type = "s", main = "value")
plot(0:(length(S)-1), 100*sol$E/sol$V, type = "s",
     main = "\% invested in risky asset")
}
