\name{NEWS}
\title{News for Package 'NMOF'}
\section{Changes in NMOF version 0.18-1 (2011-09-21)}{
  \itemize{
    \item fixed: 'restartOpt' with 'multicore = TRUE' did not correctly 
      pass '...' arguments to 'fun'
    \item fixed: typo in message issued by 'gridSearch'
    \item new file 'multicore' added in subdirectory tests; it contains a number 
      of timing tests to see if the 'multicore' option works. (The tests are 
      not run when the package is built.)
    \item minor updates in documentation
  }
}
\section{Changes in NMOF version 0.18-0 (2011-09-20)}{
  \itemize{
    \item 'restartOpt' now has option 'multicore'
    \item the 'method' option of function 'xwGauss' can now take on values 
      'hermite' and 'laguerre'
    \item documentation updated 
    \item new examples added in 'Examples and Extensions for the NMOF package'
      (see subdirectory NMOFex)
    \item test cases added for 'restartOpt' and 'xwGauss' 
      (see subdirectory unitTests)
  }
}
\section{Changes in NMOF version 0.17-0 (2011-09-18)}{
  \itemize{
    \item functions for Gaussian integration added: 'xwGauss' computes nodes and
      weights; 'changeInterval' transforms the nodes/weights to arbitrary 
      intervals. Currently, only method 'legendre' is supported.
    \item documentation updated
    \item more tests added for opt-functions (for returned objects 'Fmat'
      and 'xlist')
  }
}
\section{Changes in NMOF version 0.16-2 (2011-09-17)}{
  \itemize{
    \item all opt-functions now have arguments 'storeF' and 'storeSolutions'
      (passed through 'algo'). If the latter is TRUE, intermediary results
      will be stored in a list 'xlist' that is returned by the function
      (what exactly 'xlist' contains, depends on the specific function)
    \item minor revision in vignette 'Robust Regression with
      Particle Swarm Optimisation and Differential Evolution'
    \item NEWS file can now be found under inst/NEWS.Rd
    \item internal changes: a number of test utilities have been added
      (not exported); see, for instance, ?checkList
  }
}
\section{Changes in NMOF version 0.16-1 (2011-09-13)}{
  \itemize{
    \item fixed: 'bracketing' had not been added to the NAMESPACE file (now it is)
    \item tests for 'bracketing' added (see directory unitTests)
  }
}
\section{Changes in NMOF version 0.16-0 (2011-09-12)}{
  \itemize{
    \item function 'bracketing' added
    \item minor changes in function 'MA' (added checks)
  }
}
\section{Changes in NMOF version 0.15-6 (2011-09-11)}{
  \itemize{
    \item vignette for 'qTable' added/corrected
  }
}
\section{Changes in NMOF version 0.15-5 (2011-09-04)}{
  \itemize{
    \item 'qTable' revised (user-specified labels are now possible)
    \item (very) short vignette for 'qTable' added
    \item minor changes in code/documentation/examples
  }
}
\section{Changes in NMOF version 0.15-4}{
  \itemize{
    \item experimental: 'DEopt' takes further arguments 'storeF' and 'storeSolutions'
      (through list 'algo'). If 'storeF' is FALSE, objective function values are
      not stored ('Fmat' is set to NA; see ?DEopt). If 'storeSolutions' is TRUE,
      all population matrices are stored for every generation. This should be
      helpful for diagnostic checking (and demonstration purposes), but may use
      up a lot of memory for larger problems; hence default is FALSE.
      (These settings will either be removed from 'DEopt' or be added to all
      opt-functions in one of the next package versions.)
  }
}
\section{Changes in NMOF version 0.15-3}{
  \itemize{
    \item new vignette ('Vectorised objective functions') added
    \item more safeguards added to 'gridSearch'
  }
}
\section{Changes in NMOF version 0.15-2}{
  \itemize{
    \item function 'gridSearch' now takes a list 'mc.control' as an argument;
      it allows to pass settings to 'mclapply'
    \item more tests were added (see unitTests.R)
    \item minor changes in code/documentation/examples
  }
}
\section{Changes in NMOF version 0.15-1}{
  \itemize{
    \item more tests were added (see unitTests.R)
    \item minor changes in code/documentation/examples
  }
}
\section{Changes in NMOF version 0.15-0}{
  \itemize{
    \item function 'gridSearch' can now evaluate a function that expects
      a list or a vector as its first argument
    \item test cases for 'gridSearch' added  (in unitTests/unitTests.R)
    \item 'DEopt' can take a vector 'F' to set specific step sizes for each
      decision variable (this actually worked before but is now documented
      and tested)
    \item 'PSopt' can take a function 'changeV' and a logical 'loopChangeV'
      (passed via the list 'algo'); 'changeV' allows to impose restrictions
      on the velocity (see the code in directory NMOFex for an example)
    \item more checks were added in opt-functions
    \item small changes in the vignettes (in particular, seeds are set now)
    \item minor changes in documentation/examples
  }
}
\section{Changes in NMOF version 0.14-2}{
  \itemize{
    \item experimental: 'gridSearch' now an has argument 'multicore'; if TRUE,
      the function calls 'mclapply' (from the multicore package) to
      evaluate argument 'fun'. Currently added only for cross-platform
      testing (in particular, to see how R CMD check reacts under Windows).
    \item directory NMOFex was updated
    \item small correction in output of 'TAopt'
  }
}
\section{Changes in NMOF version 0.14-1}{
  \itemize{
    \item test cases for portfolio optimisation added (in unitTests/unitTests.R)
  }
}
\section{Changes in NMOF version 0.14-0}{
  \itemize{
    \item function 'gridSearch' was added
  }
}
\section{Changes in NMOF version 0.13-6}{
  \itemize{
    \item an initial population in 'DEopt', 'GAopt' and 'PSopt' is now passed
      through 'algo$initP' (before, a population could be passed via 'algo$mP')
    \item directory NMOFex was updated
    \item documentation was updated
  }
}
\section{Changes in NMOF version 0.13-5}{
  \itemize{
    \item directory NMOFex was updated
    \item internal changes in several functions
    \item GAopt.Rd was updated
  }
}
\section{Changes in NMOF version 0.13-4}{
  \itemize{
    \item directory NMOFex was updated
    \item minor internal changes in several functions
  }
}
\section{Changes in NMOF version 0.13-3}{
  \itemize{
    \item directory NMOFex was added; it contains the code for
      'Examples and Extensions for the NMOF package'
    \item documentation updated; minor changes in examples and vignettes
  }
}
\section{Changes in NMOF version 0.13-2}{
  \itemize{
    \item documentation, checks, unit tests added for function 'GAopt'
  }
}
\section{Changes in NMOF version 0.13-1}{
  \itemize{
    \item the population in 'GAopt' is now of mode logical (mistaking a logical
      vector for a numeric one is less harmful than the other way around)
  }
}
\section{Changes in NMOF version 0.13-0}{
  \itemize{
    \item function 'GAopt' added (minimisation by a Genetic Algorithm)
    \item several unit tests added
    \item documentation revised (thanks to Manfred Gilli for comments)
  }
}
\section{Changes in NMOF version 0.12-9}{
  \itemize{
    \item fewer simulation runs in vignettes to reduce build time
  }
}
\section{Changes in NMOF version 0.12-1}{
  \itemize{
    \item documentation has been revised
  }
}
\section{Changes in NMOF version 0.11-1}{
  \itemize{
    \item a number of test functions have been added (see ?testFunctions)
    \item new examples added for 'DEopt' and 'TAopt'
    \item vignettes have been revised
  }
}
\section{Changes in NMOF version 0.10-1}{
  \itemize{
    \item opt-functions now have 'printBar' and 'printDetail' option (set via 'algo')
  }
}
