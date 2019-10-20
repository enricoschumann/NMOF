\name{NMOF-internal}
\alias{NMOF-internal}
\alias{makeInteger}
\alias{anyNA}
\alias{checkList}
\alias{Chapters1}
\alias{Chapters2}
\alias{due}
\title{Internal NMOF functions}
\description{
  Several internal functions, not exported from the \pkg{NMOF} name space.
}
\usage{
makeInteger(x, label, min = 1L)
anyNA(x)
checkList(passedList, defaultList, label = "'algo'")
mRU(m,n)
mRN(m,n)
mcList(mc.control)
repair1c(x, up, lo)
Chapters1
Chapters2
due(D, tauD, tau, q)
}
\details{
  \code{makeInteger} coerces the scalar \code{x} to integer. If the
  result is \code{NA} or smaller than \code{min}, an error is issued.

  \code{anyNA} gives \code{TRUE} if \code{any(is.na(x))}, else
  \code{FALSE}. If  \code{x} is a function or \code{NULL}, it also
  gives \code{FALSE}.

  \code{checkList} will issue an error if \code{passedList} contains any
  \code{NA} elements. It will give a warning if any elements in
  \code{passedList} are unnamed, or if an element in
  \code{names(passedList)} is not found in \code{names(defaultList)}.

  \code{mRU} and \code{mRN} create matrices (of size \code{m} times
  \code{n}) of uniform/Gaussian variates.

  \code{mcList} takes a list of named elements and \sQuote{merges} them with
  the default settings of \code{mclapply} from package \pkg{parallel}.

  \code{repair1c} is described in the vignette on \sQuote{Repairing
    Solutions}.

  \code{Chapters[12]} is a vector of length 15, giving the chapter names as
  printed in the book. (Can be accessed with \code{\link{showChapterNames}}.)

  \code{due} (dividends until expiry) returns a list with named
  components \code{tauD} and \code{D}: all dividends for which
  time-to-payment is greater than zero, but less than or equal to
  time-to-expiry.
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
\keyword{internal}
