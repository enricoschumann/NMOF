\name{fundData}
\alias{fundData}
\docType{data}
\title{
Mutual Fund Returns
}
\description{
A matrix of 500 rows (return scenarios) and 200 columns (mutual funds).
}
\usage{fundData}
\format{A plain numeric matrix.
}
\details{
The scenarios were created with a bootstrapping technique. The data set is only meant to provide example data on which to test algorithms. 
}
\source{
Schumann, E. (2010) \emph{Essays on Practical Financial Optimisation}, (chapter 4), PhD thesis, University of Geneva.
}
\references{
Gilli, M., Maringer, D. and Schumann, E. (2011) \emph{Numerical Methods and Optimization in Finance}. Elsevier. 
\url{http://www.elsevierdirect.com/product.jsp?isbn=9780123756626}
}
\examples{
apply(fundData, 2, summary)
}
\keyword{datasets}
