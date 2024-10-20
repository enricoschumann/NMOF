\name{showExample}
\alias{showExample}
\alias{showChapterNames}
\title{
  Display Code Examples
}
\description{
  Display the code examples from
  \sQuote{Numerical Methods and Optimization and Finance}.
}
\usage{
showExample(file = "", chapter = NULL, showfile = TRUE,
            includepaths = FALSE, edition = 2, search,
            ..., ignore.case = TRUE)
showChapterNames(edition = 2)
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
    to \code{TRUE}. A file will be displayed only if one single file
    only is identified by \code{file} and \code{chapter}.

  }
  \item{includepaths}{

    Should the file paths be displayed? Defaults to \code{FALSE}.

  }
  \item{\dots}{

    Arguments passed to \code{\link{grepl}}; see Details.

  }
  \item{edition}{

    an integer: \code{1} and \code{2} are supported

  }
  \item{search}{

    a regular expression: search in the code files. Not supported yet.

  }
  \item{ignore.case}{

    passed to \code{grepl}; see Examples. Default is \code{TRUE} (which
    is much more helpful than the default \code{FALSE} before package
    version 2)

  }
}
\details{

  \code{showExample} matches the specified file
  argument against the available file names via
  \code{grepl(file, all.filenames, ignore.case =
  ignore.case, ...)}. If \code{chapter} is specified, a
  second match is performed, \code{grepl(chapter,
  all.chapternames, ignore.case = ignore.case, ...)}. The
  \code{chapternames} are those in the book (e.g.,
  \sQuote{Modeling dependencies}). The selected files
  are then those for which file name and chapter name
  could be matched.

}
\value{

  \code{showExample} returns a \code{\link{data.frame}} of at least two
  character vectors, Chapter and File. If \code{includepaths} is
  \code{TRUE}, Paths are included. If no file is found, the
  \code{\link{data.frame}} has zero rows. If a single file is identified
  and \code{showfile} is \code{TRUE}, the function has the side effect
  of displaying that file.

  \code{showChapterNames} returns a character vector: the
  names of the book's chapters.

}
\references{

  Gilli, M., Maringer, D. and Schumann, E. (2011)
  \emph{Numerical Methods and Optimization in Finance}.
  Elsevier. \doi{10.1016/C2009-0-30569-3}

  Gilli, M., Maringer, D. and Schumann, E. (2019)
  \emph{Numerical Methods and Optimization in Finance}.
  2nd edition. Elsevier. \doi{10.1016/C2017-0-01621-X}

  Schumann, E. (2023) Financial Optimisation with R (\pkg{NMOF} Manual).
  \url{https://enricoschumann.net/NMOF.htm#NMOFmanual}

}
\author{
  Enrico Schumann
}
\note{

  The behaviour of the function changed slightly with
  version 2.0 to accommodate the code examples of the
  second edition of the book. Specifically, the
  function gained an argument \code{edition}, which
  defaults to \code{2}. Also, the default for
  \code{ignore.case} was changed to \code{TRUE}.  To
  get back the old behaviour of the function, set
  \code{edition} to \code{1} and \code{ignore.case} to
  \code{FALSE}.

  The code files can also be downloaded from
  \url{https://gitlab.com/NMOF} .

}
\examples{
## list all files
showExample()  ## 2nd edition is default
showExample(edition = 1)

## list specific files
showExample("Appendix")
showExample("Backtesting")
showExample("Heuristics")

showExample("tutorial") ## matches against filename
showExample(chapter = 13)
showExample(chapter = "tutorial")

## show where a file is installed
showExample(chapter = "portfolio", includepaths = TRUE)



## first edition
showExample("equations.R", edition = 1)
showExample("example", chapter = "portfolio", edition = 1)

showExample("example", chapter = 13, edition = 1)
showExample("example", chapter = showChapterNames(1)[13L], edition = 1)
}
