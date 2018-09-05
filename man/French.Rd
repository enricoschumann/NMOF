\name{French}
\alias{French}
\title{
  Download Datasets from Kenneth French's Data Library
}
\description{

  Download data provided by Kenneth French.

}
\usage{
French(dest.dir, dataset = "variance",
       weighting = "equal", frequency = "monthly",
       price.series = FALSE, na.rm = TRUE)
}
\arguments{
  \item{dest.dir}{
    character: a path to a directory
}
  \item{dataset}{
    a character string
}
  \item{weighting}{
    a character string: equal or value
}
  \item{frequency}{
    a character string: daily or monthly
}
  \item{price.series}{
    logical
}
  \item{na.rm}{
    logical
}
}
\details{

  The function downloads data provided by Kenneth
  French at
  \url{http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html}.

  The download file gets a date prefix (today in
  format \code{YYYYMMDD}) and is stored in directory
  \code{dest.dir}. Before any download is attempted,
  the function checks whether a file with today's
  prefix exist in \code{dest.dir}; if yes, the file
  used.      
  
}
\value{
a \code{\link{data.frame}}
%% a \code{\link{data.frame}}:
%% \item{Date}{end of month}
%% \item{Price}{numeric}
%% \item{Dividend}{numeric}
%% \item{Earnings}{numeric}
%% \item{CPI}{numeric}
%% \item{Long Rate}{numeric}
%% \item{Real Price}{numeric}
%% \item{Real Dividend}{numeric}
%% \item{Real Earnings}{numeric}
%% \item{CAPE}{numeric}
}
%% \references{

%%   Shiller, R.J. (2015) \emph{Irrational
%%   Exhuberance}. Princeton University Press. 3rd
%%   edition.
  
%% }
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{Shiller}}
}
\examples{
\dontrun{
archive.dir <- "~/Downloads/French"
if (!dir.exists(archive.dir))
    dir.create(archive.dir)
French(archive.dir)
}
}
