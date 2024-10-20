\name{fundData}
\alias{fundData}
\docType{data}
\title{
Mutual Fund Returns
}
\description{
A matrix of 500 rows (return scenarios) and 200 columns (mutual
funds). The elements in the matrix are weekly returns.
}
\usage{fundData}
\format{A plain numeric matrix.}
\details{
The scenarios were created with a bootstrapping technique. The data set
is only meant to provide example data on which to test algorithms.
}
\source{
Schumann, E. (2010) \emph{Essays on Practical Financial Optimisation},
(chapter 4), PhD thesis, University of Geneva.
}
\references{

  Gilli, M., Maringer, D. and Schumann, E. (2019)
  \emph{Numerical Methods and Optimization in Finance}.
  2nd edition. Elsevier. \doi{10.1016/C2017-0-01621-X}

  Schumann, E. (2023) Financial Optimisation with R (\pkg{NMOF} Manual).
  \url{https://enricoschumann.net/NMOF.htm#NMOFmanual}
}
\examples{
apply(fundData, 2, summary)
}
\keyword{datasets}
