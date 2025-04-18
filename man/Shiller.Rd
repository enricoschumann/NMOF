\name{Shiller}
\alias{Shiller}
\title{
  Download Robert Shiller's Data
}
\description{

  Download the data provided by Robert Shiller
  and transform them into a data frame.

}
\usage{
Shiller(dest.dir,
        url = NULL)
}
\arguments{
  \item{dest.dir}{
    character: a path to a directory
}
  \item{url}{
    the data URL
}
}
\details{

  The function downloads US stock-market data provided
  by Robert Shiller which he used in his book
  \sQuote{Irrational Exhuberance}.  Since the data are
  provided in Excel format, package \pkg{readxl} is
  required.

  The downloaded Excel gets a date prefix (today in
  format \code{YYYYMMDD}) and is stored in directory
  \code{dest.dir}. Before any download is attempted,
  the function checks whether a file with today's
  prefix exist in \code{dest.dir}; if yes, the file is
  used.

}
\value{
a \code{\link{data.frame}}:
\item{Date}{end of month}
\item{Price}{numeric}
\item{Dividend}{numeric}
\item{Earnings}{numeric}
\item{CPI}{numeric}
\item{Long Rate}{numeric}
\item{Real Price}{numeric}
\item{Real Dividend}{numeric}
\item{Real Earnings}{numeric}
\item{CAPE}{numeric}
}
\references{

  Gilli, M., Maringer, D. and Schumann, E. (2019)
  \emph{Numerical Methods and Optimization in Finance}.
  2nd edition. Elsevier. \doi{10.1016/C2017-0-01621-X}

  Schumann, E. (2023) Financial Optimisation with R (\pkg{NMOF} Manual).
  \url{https://enricoschumann.net/NMOF.htm#NMOFmanual}

  Shiller, R.J. (2015) \emph{Irrational Exhuberance}.
  Princeton University Press. 3rd edition.
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{French}}
}
\examples{
\dontrun{
archive.dir <- "~/Downloads/Shiller"
if (!dir.exists(archive.dir))
    dir.create(archive.dir)
Shiller(archive.dir)
}
}
