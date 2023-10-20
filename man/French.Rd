\name{French}
\alias{French}
\title{
  Download Datasets from Kenneth French's Data Library
}
\description{

  Download datasets from Kenneth French's Data Library.

}
\usage{
French(dest.dir,
       dataset = "F-F_Research_Data_Factors_CSV.zip",
       weighting = "value", frequency = "monthly",
       price.series = FALSE, na.rm = FALSE,
       adjust.frequency = TRUE)
}
\arguments{
  \item{dest.dir}{
    character: a path to a directory
  }
  \item{dataset}{

    a character string: the CSV file name. Also
    supported are the keywords \sQuote{\code{market}}
    and \sQuote{\code{rf}}.

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

  logical: convert the returns series into prices
  series?

}
\item{na.rm}{

  logical: remove missing values in the calculation of
  price series?

}
\item{adjust.frequency}{

  logical: if \code{TRUE}, frequency is switched to
  \sQuote{\code{"daily"}} when the word
  \sQuote{\code{"daily"}} appears in the dataset's name

}
}
\details{

  The function downloads data provided by Kenneth
  French at
  \url{http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html}.
  The download file gets a date prefix (current date in
  format \code{YYYYMMDD}) and is stored in directory
  \code{dest.dir}. Before any download is attempted,
  the function checks whether a file with today's
  prefix exist in \code{dest.dir}; if yes, the file is
  used.

  In the original data files, missing values are
  coded as \code{-99} or similar. These
  numeric values are replaced by \code{\link{NA}}.

  Calling the function without any arguments will print
  the names of the supported datasets (and return them insivibly).

}
\value{

  A \code{\link{data.frame}}, with contents depending on the
  particular dataset.  If the download failes, the function
  evaluates to \code{\link{NULL}}.

}
\references{
  Gilli, M., Maringer, D. and Schumann, E. (2019)
  \emph{Numerical Methods and Optimization in Finance}.
  2nd edition. Elsevier. \doi{10.1016/C2017-0-01621-X}

  Schumann, E. (2023) Financial Optimisation with R (\pkg{NMOF} Manual).
  \url{http://enricoschumann.net/NMOF.htm#NMOFmanual}
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{Shiller}}
}
\examples{
## list all supported files
French()

\donttest{
  ## fetch names of files from Kenneth French's website
  try({
      txt <- readLines(paste0("https://mba.tuck.dartmouth.edu/pages/",
                              "faculty/ken.french/data_library.html"))
      csv <- txt[grep("ftp/.*CSV.zip", txt, ignore.case = TRUE)]
      gsub(".*ftp/(.*?CSV.zip).*", "\\1", csv, ignore.case = TRUE)
    })
}

\dontrun{
archive.dir <- "~/Downloads/French"
if (!dir.exists(archive.dir))
    dir.create(archive.dir)
French(archive.dir, "F-F_Research_Data_Factors_CSV.zip")
}
}
