\name{minvar}
\alias{minvar}
\concept{portfolio selection}
\title{
  Minimum-Variance Portfolios
}
\description{
  Compute minimum-variance portfolios, subject to lower and upper bounds
  on weights.
}
\usage{
minvar(var, wmin = 0, wmax = 1, method = "qp",
       groups = NULL, groups.wmin = NULL, groups.wmax = NULL)
}
\arguments{
  \item{var}{
    the covariance matrix: a numeric (real), symmetric matrix
}
  \item{wmin}{
    numeric: a lower bound on weights. May also be a vector that holds
    specific bounds for each asset.
}
\item{wmax}{
      numeric: an upper bound on weights. May also be a vector that holds
    specific bounds for each asset.
}
  \item{method}{
    character. Currently, only \code{"qp"} is supported.
  }
  \item{groups}{
    a list of group definitions
  }
  \item{groups.wmin}{
    a numeric vector
  }
  \item{groups.wmax}{
    a numeric vector
  }
}
\details{

  For method \code{"qp"}, the function uses
  \code{\link[quadprog]{solve.QP}} from package
  \pkg{quadprog}. Because of the algorithm that
  \code{\link[quadprog]{solve.QP}} uses, \code{var} has to be positive
  definite (i.e. must be of full rank).

}
\value{
  a numeric vector (the portfolio weights) with an attribute
  \code{variance} (the portfolio's variance)
}
\references{
  Gilli, M., Maringer, D. and Schumann, E. (2019)
  \emph{Numerical Methods and Optimization in Finance}.
  2nd edition. Elsevier. \doi{10.1016/C2017-0-01621-X}

  Schumann, E. (2023) Financial Optimisation with R (\pkg{NMOF} Manual).
  \url{https://enricoschumann.net/NMOF.htm#NMOFmanual}

  Schumann, E. (2012) Computing the global minimum-variance
  portfolio. \url{https://enricoschumann.net/R/minvar.htm}
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{TAopt}}
}
\examples{
## variance-covariance matrix from daily returns, 1 Jan 2014 -- 31 Dec 2013, of
## cleaned data set at https://enricoschumann.net/data/gilli_accuracy.html

if (requireNamespace("quadprog")) {

    var <- structure(c(0.000988087100677907, -0.0000179669410403153, 0.000368923882626859,
                       0.000208303611101873, 0.000262742052359594, -0.0000179669410403153,
                       0.00171852167358765, 0.0000857467457561209, 0.0000215059246610556,
                       0.0000283532159921211, 0.000368923882626859, 0.0000857467457561209,
                       0.00075871953281751, 0.000194002299424151, 0.000188824454515841,
                       0.000208303611101873, 0.0000215059246610556, 0.000194002299424151,
                       0.000265780633005374, 0.000132611196599808, 0.000262742052359594,
                       0.0000283532159921211, 0.000188824454515841, 0.000132611196599808,
                       0.00025948420130626),
                     .Dim = c(5L, 5L),
                     .Dimnames = list(c("CBK.DE", "VOW.DE", "CON.DE", "LIN.DE", "MUV2.DE"),
                                      c("CBK.DE", "VOW.DE", "CON.DE", "LIN.DE", "MUV2.DE")))

    ##            CBK.DE     VOW.DE    CON.DE    LIN.DE   MUV2.DE
    ## CBK.DE   0.000988 -0.0000180 0.0003689 0.0002083 0.0002627
    ## VOW.DE  -0.000018  0.0017185 0.0000857 0.0000215 0.0000284
    ## CON.DE   0.000369  0.0000857 0.0007587 0.0001940 0.0001888
    ## LIN.DE   0.000208  0.0000215 0.0001940 0.0002658 0.0001326
    ## MUV2.DE  0.000263  0.0000284 0.0001888 0.0001326 0.0002595
    ##

    minvar(var, wmin = 0, wmax = 0.5)

    minvar(var,
           wmin = c(0.1,0,0,0,0), ## enforce at least 10\% weight in CBK.DE
           wmax = 0.5)

    minvar(var, wmin = -Inf, wmax = Inf)   ## no bounds
    ## [1] -0.0467  0.0900  0.0117  0.4534  0.4916

    minvar(var, wmin = -Inf, wmax = 0.45)  ## no lower bounds
    ## [1] -0.0284  0.0977  0.0307  0.4500  0.4500

    minvar(var, wmin =  0.1, wmax = Inf)   ## no upper bounds
    ## [1] 0.100 0.100 0.100 0.363 0.337

    ## group constraints:
    ##   group 1 consists of asset 1 only,   and must have weight [0.25,0.30]
    ##   group 2 consists of assets 4 and 5, and must have weight [0.10,0.20]

    ##   => unconstrained
    minvar(var, wmin = 0, wmax = 0.40)
    ## [1] 0.0097 0.1149 0.0754 0.4000 0.4000

    ##   => with group constraints
    minvar(var, wmin = 0, wmax = 0.40,
           groups = list(1, 4:5),
           groups.wmin = c(0.25, 0.1),
           groups.wmax = c(0.30, 0.2))
    ## [1] 0.250 0.217 0.333 0.149 0.051
}
}
