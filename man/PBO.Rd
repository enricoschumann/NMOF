\name{PBO}
\alias{PBO}
\title{
  Probability of Backtest Overfitting
}
\description{
  Estimate probability of backtest overfitting, as described
  in Bailey et al. 2017

}
\usage{
PBO(M, s = 12, fun = colMeans,
    threshold = 0, ..., loop.fun = TRUE)
}
\arguments{
  \item{M}{
    a numeric matrix
  }
  \item{s}{
    number of submatrices
  }
  \item{fun}{
    a function
  }
  \item{threshold}{
    a scalar
  }
  \item{\dots}{
    additional arguments passed to \code{fun}
  }
  \item{loop.fun}{%

    logical: if \code{TRUE}, \code{fun} is called separately
    for every column of \code{M}; if \code{\link{FALSE}},
    \code{fun} will be passed the whole matrix \code{M}

  }
}
\details{

  \code{fun} is called with \code{M} as its first argument.


}
\value{
  A list:
  \item{pbo}{probability}
  \item{lambda}{vector of logits}
  \item{in.sample}{numeric}
  \item{out.of.sample}{numeric}

}
\references{
  Bailey, D. H. et al. (2017).
  The probability of backtest overfitting. Journal of Computational Finance
  \strong{20} (4), 39--69. \doi{10.21314/JCF.2016.322}
}
\author{
  Enrico Schumann
}
%% \note{
%% %%  ~~further notes~~
%% }

%% %% ~Make other sections like Warning with \section{Warning }{....} ~

%% \seealso{
%% %% ~~objects to See Also as \code{\link{help}}, ~~~
%% }
\examples{
\donttest{
## might run a few seconds
N <- 5000
T <- 480
M <- array(rnorm(N*T, sd = 0.01), dim = c(T, N))
p <- PBO(M, s = 12, fun = colMeans, loop.fun = FALSE)
str(p)
}
}
