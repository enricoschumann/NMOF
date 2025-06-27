# Numerical Methods and Optimisation in Finance

Functions, examples and data from the first and the
second edition of "Numerical Methods and Optimization
in Finance" by
[M. Gilli](https://www.unige.ch/gsem/en/research/faculty/honorary-professors/manfred-gilli/),
[D. Maringer](https://wwz.unibas.ch/de/personen/dietmar-maringer/)
and [E. Schumann](https://enricoschumann.net/)
(2019, ISBN:978-0128150658).  The package provides
implementations of optimisation heuristics
(Differential Evolution, Genetic Algorithms, Particle
Swarm Optimisation, Simulated Annealing and Threshold
Accepting), and other optimisation tools, such as grid
search and greedy search.  There are also functions for
the valuation of financial instruments, such as bonds
and options, and functions that help with stochastic
simulations.



## Installing the package

The latest build of the package is always available from
[https://enricoschumann.net/R/packages/NMOF/](https://enricoschumann.net/R/packages/NMOF/).
A stable version is available from
[CRAN](https://cran.r-project.org/package=NMOF).

To install the package from within an R session, type:

    install.packages('NMOF')  ## CRAN version
    install.packages('NMOF',  ## development version
                     repos = c('https://enricoschumann.net/R',
                               getOption('repos')))



## News, feedback and discussion

New package releases and other news related to the book or the
package are announced on the
[NMOF-news mailing list](https://lists.r-forge.r-project.org/cgi-bin/mailman/listinfo/nmof-news).

An [RSS feed of the package NEWS file](https://enricoschumann.net/R/packages/NMOF/NMOF_news.xml)
is also available.

Applications, as long as they are finance-related, should be
discussed on the [R-SIG-Finance mailing list](https://stat.ethz.ch/mailman/listinfo/r-sig-finance).

Please send bug reports or suggestions directly to the
package maintainer, for instance by using =bug.report=.

    library("utils")
    bug.report("[NMOF] Unexpected behaviour in function XXX",
               maintainer("NMOF"), package = "NMOF")



## References

Manfred Gilli, Dietmar Maringer and Enrico Schumann.
[Numerical Methods and Optimization in Finance](https://enricoschumann.net/NMOF.htm).
Academic Press, 2019.
