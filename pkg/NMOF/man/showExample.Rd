\name{showExample}
\alias{showExample}
\alias{showChapterNames}
\title{
  Display examples
}
\description{
  Display the code examples from \sQuote{Numerical Methods and
    Optimization and Finance}.
}
\usage{
showExample(file = "", chapter = NULL, showfile = TRUE, includepaths =
FALSE, ...)
showChapterNames()
}
\arguments{
  \item{file}{
    a character vector of length one. See Details.
  }
  \item{chapter}{
    optional: a character vector of length one, giving the chapter name
    (see Details), or an integer, indicating a chapter number. Default
    is \code{NULL}: look in all chapters.
  }
  \item{showfile}{
    Should the file be displayed with \code{\link{file.show}}? Defaults
    to \code{TRUE}. A file will be displayed if only a single file is
    identified by \code{file} and \code{chapter}.
  }
  \item{includepaths}{
    Should the file paths be displayed? Defaults to \code{FALSE}.
  }
  \item{\dots}{
    Arguments passed to \code{\link{grepl}}; see Details.
  }
}
\details{
  The function will try to match the specified file argument against the
  available file names via \code{grepl(file, filenames, ...)}. If
  \code{chapter} is specified, a second match is performed,
  \code{grepl(chapter, chapternames, ...)}. The \code{chapternames} are
  those in the book (eg, \sQuote{Modeling dependencies}). The selected
  files are then those for which file name and chapter name could be matched.

  \code{showChapterNames} returns
}
\value{

  \code{showExample} returns a \code{\link{data.frame}} of at least two
  character vectors, Chapter and File. If \code{includepaths} is
  \code{TRUE}, Paths are included. If no file is found, the
  \code{\link{data.frame}} has zero rows. If a single file is identified
  and \code{showfile} is \code{TRUE}, the function has the side effect
  of displaying that file.

  \code{showChapterNames} returns a character vector of length 15: the
  names of book's chapters.

}
\references{

  Gilli, M., Maringer, D. and Schumann, E. (2011) \emph{Numerical
    Methods and Optimization in Finance}. Elsevier.
  \url{http://www.elsevierdirect.com/product.jsp?isbn=9780123756626}
}
\author{
  Enrico Schumann
}
\note{
  The files can also be downloaded from \url{http://nmof.net} .
}
\examples{
showExample("equations.R")

showExample("example", chapter = "portfolio")
showExample("example", chapter = "Portfolio")
showExample("example", chapter = "portfolio", ignore.case = TRUE)
showExample("example", chapter = 13)
showExample("example", chapter = showChapterNames()[13L])

}
