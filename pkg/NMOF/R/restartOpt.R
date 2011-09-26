restartOpt <- function(fun, n, OF, algo, ...,
                       method = c("loop", "multicore", "snow"),
                       mc.control = list(),
                       cl = NULL) {
    n <- makeInteger(n, "'n'", 1L)

    ## check if multicore is available and prepare settings
    method <- tolower(method[1L])
    if (method == "multicore") {
        if (!suppressWarnings(require("multicore", quietly = TRUE))) {
            method <- "loop"
            warning("package 'multicore' not available: use method 'loop'")
        }
    } else if (method == "snow") {
        if (!suppressWarnings(require("snow", quietly = TRUE))) {
            method <- "loop"
            warning("package 'snow' not available: use method 'loop'")
        } else if (is.null(cl)) {
            method <- "loop"
            warning("no cluster 'cl' passed for method 'snow'")
        }
    }

    ## necessary to match the '...' for lapply and fun
    force(fun)
    force(OF)
    force(algo)
    junk <- list(...)
    makefun <- function(fun, OF, algo, ...) {
        function(ignore) fun(OF = OF, algo = algo, ...)
    }
    fun2 <- makefun(fun, OF, algo, ...)

    if (method == "multicore") {
        mc.settings <- mcList(mc.control)
        allResults <- mclapply(seq_len(n), fun2,
                               mc.preschedule = mc.settings$mc.preschedule,
                               mc.set.seed = mc.settings$mc.set.seed,
                               mc.silent = mc.settings$mc.silent,
                               mc.cores = mc.settings$mc.cores,
                               mc.cleanup = mc.settings$mc.cleanup)

        if (!suppressWarnings(require("multicore", quietly = TRUE))) {
            method <- "loop"
            warning("package 'multicore' not available: use method 'loop'")
        }
    } else if (method == "snow"){
        if (is.numeric(cl)) {
            cl <- makeCluster(c(rep("localhost", cl)),  type = "SOCK")
            on.exit(stopCluster(cl))
        }
        #junk <- try(parLapply(cl, 1:10, fun, OF=OF, algo=algo, ...),
        #            silent=TRUE)
        allResults <- clusterApply(cl, seq_len(n), fun2)
    } else {
        allResults <- lapply(seq_len(n), fun2)
    }
    allResults
}
