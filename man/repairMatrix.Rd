\name{repairMatrix}
\alias{repairMatrix}
\title{
Repair an Indefinite Correlation Matrix
}
\description{
The function \sQuote{repairs} an indefinite correlation matrix by replacing its negative eigenvalues by zero.
}
\usage{
repairMatrix(C)
}
\arguments{
  \item{C}{a correlation matrix}
}
\details{

  The function \sQuote{repairs} a correlation matrix: it replaces
  negative eigenvalues with \code{eps} and rescales the matrix such that
  all elements on the main diagonal become unity again.

}
\value{
Returns a numeric matrix.
}
\references{

  Gilli, M., Maringer, D. and Schumann, E. (2011) \emph{Numerical
  Methods and Optimization in Finance}. Elsevier.
  \url{http://www.elsevierdirect.com/product.jsp?isbn=9780123756626}

  Rebonato, R. and Jaeckel, P. (1999) The most general methodology to
  create a valid correlation matrix for risk management and option
  pricing purposes.

}
\author{
Enrico Schumann
}
\note{

  This function may help to cure a numerical problem, but it will rarely
  help to cure an empirical problem. (Garbage in, garbage out.)

  See also the function \code{nearPD} in the \pkg{Matrix} package.

}
\examples{
## example: build a portfolio of three assets
C <- c(1,.9,.9,.9,1,.2,.9,.2,1)
dim(C) <- c(3L, 3L)
eigen(C, only.values = TRUE)

vols <- c(.3, .3, .3)      ## volatilities
S <- C * outer(vols,vols)  ## covariance matrix
w <- c(-1, 1, 1)           ## a portfolio
w \%*\% S \%*\% w          ## variance of portfolio is negative!
sqrt(as.complex(w \%*\% S \%*\% w))

S <- repairMatrix(C) * outer(vols,vols)
w \%*\% S \%*\% w          ## more reasonable
sqrt(w \%*\% S \%*\% w)
}

