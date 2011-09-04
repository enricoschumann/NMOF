\name{qTable}
\alias{qTable}
\title{
Prepare LaTeX Table with Quartile Plots
}
\description{
The function returns the skeleton of a table that contains the median,
minimum and maximum of the columns of a matrix \code{X}. For each column, a quartile plot
is added.
}
\usage{
qTable(X, xmin = NULL, xmax = NULL, labels = NULL, at = NULL,
       unitlength = "5cm", linethickness = NULL,
       cnames = colnames(X), circlesize = 0.01,
       xoffset = 0, yoffset = 0, dec = 2, filename = NULL)
}
\arguments{
  \item{X}{
a numeric matrix (or an object that can be coerced to a numeric matrix
with \code{as.matrix})
}
  \item{xmin}{
optional: the minimum for the x-axis. See Details.
}
  \item{xmax}{
optional: the maximum for the x-axis. See Details.
}
  \item{labels}{
optional: labels for the x-axis.
}
  \item{at}{
optional: where to put labels.
}
  \item{unitlength}{
the unitlength for LaTeX's \code{picture} environment. See Details.
}
  \item{linethickness}{
the linethickness for LaTeX's \code{picture} environment. See Details.
}
  \item{cnames}{
the column names of \code{X}
}
  \item{circlesize}{
the size of the circle in LaTeX's \code{picture} environment
}
  \item{xoffset}{
defaults to 0. See Details.
}
  \item{yoffset}{
defaults to 0. See Details.
}
  \item{dec}{
the number of decimals
}
  \item{filename}{
if provided, output is \code{cat} into a file
}
}
\details{
The function creates a one-column character matrix that can be put into
a LaTeX file (the matrix holds a tabular). It relies on LaTeX's
\code{picture} environment and should work for
LaTeX and pdfLaTeX. Note that the tabular needs generally be refined, depending on the settings and the data.

The tabular one every row for every column of \code{X} (and header
and footer rows). A given row contains the median, the minimum and the
maximum of the column; it also includes a \code{picture} environment the
shows a quartile plot of the distribution of the elements in that column.

A number of parameters can be passed to LaTeX's \code{picture} environment:
\code{unitlength}, \code{xoffset}, \code{yoffset},
\code{linethickness}. Sizes and lengths are functions of
\code{unitlength}. The whole tabular environment is put into curly
brackets so that the settings do not change settings elsewhere in
the LaTeX document.

If \code{xmin}, \code{xmax}, \code{labels} and \code{at} are not specified, they are computed
through a call to \code{\link{pretty}} from the \pkg{base} package. If
limits are specified, then both \code{xmin} and \code{xmax} must be set; if labels are
used, then both \code{labels} and \code{at} must be specified. 

The vignette `\code{qTableEx}' shows some examples.
}
\value{
A matrix of mode character. If \code{filename} is specified then
\code{qTable} will have the side effect of writing a textfile with a
LaTeX tabular.
}
\references{
Gilli, M., Maringer, D. and Schumann, E. (2011) \emph{Numerical Methods and Optimization in Finance}. Elsevier.
\url{http://www.elsevierdirect.com/product.jsp?isbn=9780123756626}

Tufte, E. (2001) \emph{The Visual Display of Quantitative Information}. 2nd edition, Graphics Press.
}
\author{
Enrico Schumann
}
\note{
\code{qTable} returns a raw draft of a table for LaTeX. Please, spend some time on making it pretty.
}

\examples{
x <- rnorm(100, mean = 0, sd = 2)
y <- rnorm(100, mean = 1, sd = 2)
z <- rnorm(100, mean = 1, sd = 0.5)
X <- cbind(x, y, z)
res <- qTable(X)
print(res)
cat(res)

\dontrun{
## create a simple LaTeX file 'test.tex'
#\documentclass{article}
#\begin{document}
#\input{res.tex}
#\end{document}

res <- qTable(X, filename = "res.tex", yoffset = -0.025, unitlength = "5cm",
       circlesize = 0.0125, xmin = -10, xmax = 10, dec = 2)
}
}

