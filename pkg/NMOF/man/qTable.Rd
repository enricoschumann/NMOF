\name{qTable}
\alias{qTable}
\title{
Prepare LaTeX Table with Quartile Plots
}
\description{
The function returns the skeleton of a table that contains the median, minimum and maximum of a matrix \code{X}. For each column, a quartile plot 
is added.
}
\usage{
qTable(X, xmin = NULL, xmax = NULL, labels = NULL, at = NULL, 
       unitlength = "1cm", linethickness = NULL, 
       cnames = colnames(X), circlesize = 0.01, 
       xoffset = 0, yoffset = 0, dec = 2, filename = NULL)
}
\arguments{
  \item{X}{
a numeric matrix (or an object that can be coerced to a numeric matrix)
}
  \item{xmin}{
optional: the minimum for the x-axis
}
  \item{xmax}{
optional: the maximum for the x-axis
}
  \item{labels}{
optional: labels for the x-axis. If not \code{NULL}, then \code{xmin}, \code{xmax} and \code{at} must be provided.
}
  \item{at}{
optional: where to put labels. If not \code{NULL}, then \code{xmin}, \code{xmax} and \code{labels} must be provided.
}
  \item{unitlength}{
the unitlength for LaTeX's \code{picture} environment
}
  \item{linethickness}{
the linethickness for LaTeX's \code{picture} environment
}
  \item{cnames}{
the column names of \code{X}
}
  \item{circlesize}{
the size of the circle in LaTeX's \code{picture} environment
}
  \item{xoffset}{
defaults to 0
}
  \item{yoffset}{
defaults to 0
}
  \item{dec}{
the number of decimals
}
  \item{filename}{
print to file?
}
}
\details{
The function creates a character matrix that can be put into a LaTeX file (the matrix holds a tabular). 
It relies on LaTeX's picture environment and should work for LaTeX and pdfLaTeX. 
Note that the tabular needs generally be refined, depending on the settings and the data. 
}
\value{
A matrix of mode character. If \code{filename} is specified then \code{qTable} will have the side effect of writing a textfile with a 
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

