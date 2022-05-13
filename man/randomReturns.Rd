\name{randomReturns}
\alias{randomReturns}
\title{
  Create a Random Returns
}
\description{
  Create a matrix of random returns.
}
\usage{
randomReturns(na, ns, sd, mean = 0, rho = 0, exact = FALSE)
}
\arguments{
  \item{na}{%
    number of assets
  }
  \item{ns}{%
    number of return scenarios
  }
  \item{sd}{%
    the standard deviation: either a single number or a vector
    of length \code{na}
  }
  \item{mean}{%
    the mean return: either a single number or a vector
    of length \code{na}
  }
  \item{rho}{%
    correlation: either a scalar (i.e. a constant pairwise correlation)
    or a correlation matrix
  }
  \item{exact}{%
    logical: if \code{TRUE}, return a random matrix whose
    column means, standard deviations and correlations match the
    specified values exactly (up to numerical precision)
  }
}
\details{

  The function corresponds to the function \code{random_returns},
  described in the second edition of \acronym{NMOF} (the book).

}
\value{

  a \code{\link{numeric}} \code{\link{matrix}} of size \code{na} times
  \code{ns}

}
\references{

  Gilli, M., Maringer, D. and Schumann, E. (2019) \emph{Numerical
    Methods and Optimization in Finance}, 2nd edition. Elsevier.
    \url{https://www.elsevier.com/books/numerical-methods-and-optimization-in-finance/gilli/978-0-12-815065-8}

  Schumann, E. (2021) Financial Optimisation with R (\pkg{NMOF} Manual).
  \url{http://enricoschumann.net/NMOF.htm#NMOFmanual}

}
\author{
  Enrico Schumann
}
\note{

  The function corresponds to the function \code{random_returns},
  described in the second edition of \acronym{NMOF} (the book).

}
\seealso{
  \code{\link{mc}}
}
\examples{
if (requireNamespace("quadprog")) {
    ## a small experiment: when computing minimum-variance portfolios
    ## for correlated assets, how many large positions are in the portfolio?

    na <- 100  ## number of assets
    inc <-  5  ## minimum of assets to include

    n <- numeric(10)
    for (i in seq_along(n)) {
        R <- randomReturns(na = na,
                           ns = 500,
                           sd = seq(.2/.16, .5/.16, length.out = 100),
                           rho = 0.5)
        n[i] <- sum(minvar(cov(R), wmax = 1/inc)> 0.01)
    }
    summary(n)
}
}
