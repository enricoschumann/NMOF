\name{French}
\alias{French}
\title{
  Download Datasets from Kenneth French's Data Library
}
\description{

  Download datasets from Kenneth French's Data Library.

}
\usage{
French(dest.dir, dataset = "F-F_Research_Data_Factors_CSV.zip",
       weighting = "value", frequency = "monthly",
       price.series = FALSE, na.rm = FALSE)
}
\arguments{
  \item{dest.dir}{
    character: a path to a directory
}
  \item{dataset}{
    a character string: the file name
}
  \item{weighting}{
    a character string: \code{"equal"} or \code{"value"}
}
  \item{frequency}{

    a character string: \code{daily}, \code{monthly} or
    \code{annual}. Whether it is used or ignored
    depends on the particular dataset.

}
  \item{price.series}{
    logical: convert the returns series into prices series?
}
  \item{na.rm}{
    logical: remove missing values in the calculation
    of price series?
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

  Missing values are usually coded as \code{-99} or
  similar values in the original data files. These
  numeric values are replaced by \code{\link{NA}}.

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
French(archive.dir, "F-F_Research_Data_Factors_CSV.zip")
}
}
