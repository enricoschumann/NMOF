restartOpt <- function(fun, n, OF, algo, ...,
                       multicore = FALSE, mc.control = list()) {
    n <- makeInteger(n, "'n'", 1L)

    ## check if multicore is available and prepare settings
    if (multicore) {
        ## check if 'multicore' is available
        if (!suppressWarnings(require("multicore", quietly = TRUE))) {
            multicore <- FALSE
            warning("package 'multicore' not available")
        }
        ## make settings
        mc.settings <- list(mc.preschedule = TRUE,
                            mc.set.seed = TRUE,
                            mc.silent = FALSE,
                            mc.cores = getOption("cores"),
                            mc.cleanup = TRUE)
        mc.settings[names(mc.control)] <- mc.control
    }

    ## necessary to match the '...' for lapply and fun
    makefun <- function(fun, OF, algo, ...)
        f <- function(ignore) fun(OF = OF, algo = algo, ...)
    fun2 <- makefun(fun, OF, algo, ...)

    if (multicore) {
        allResults <- mclapply(seq_len(n), fun2,
                               mc.preschedule = mc.settings$mc.preschedule,
                               mc.set.seed = mc.settings$mc.set.seed,
                               mc.silent = mc.settings$mc.silent,
                               mc.cores = mc.settings$mc.cores,
                               mc.cleanup = mc.settings$mc.cleanup)

    } else {
        allResults <- lapply(seq_len(n), fun2)
    }
    allResults
}
