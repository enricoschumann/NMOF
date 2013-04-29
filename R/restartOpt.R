restartOpt <- function(fun, n, OF, algo, ...,
                       method = c("loop", "multicore", "snow"),
                       mc.control = list(),
                       cl = NULL, best.only = FALSE) {
    n <- makeInteger(n, "'n'", 1L)
    method <- tolower(method[1L])
    force(fun)
    force(OF)
    force(algo)
    tmp <- list(...)  ## force does not work
    fun2 <- function(ignore)
        fun(OF = OF, algo = algo, ...)
    if (!is.null(cl))
        method <- "snow"
    if (method == "multicore") {
        if (!suppressWarnings(require("parallel", quietly = TRUE))) {
            message("package 'parallel' is used")
        } else if (!suppressWarnings(require("multicore", quietly = TRUE))) {
            method <- "loop"
            warning("package 'multicore' not available: use method 'loop'")
        }
    } else if (method == "snow") {
        if (!suppressWarnings(require("snow", quietly = TRUE))) {
            method <- "loop"
            warning("package 'snow' not available: use method 'loop'")
        } else if (is.null(cl)) {
            method <- "loop"
            warning("no cluster 'cl' passed for method 'snow': use method 'loop'")
        }
    }
    if (method == "multicore") {
        mc.settings <- mcList(mc.control)
        allResults <- mclapply(seq_len(n), fun2,
                               mc.preschedule = mc.settings$mc.preschedule,
                               mc.set.seed = mc.settings$mc.set.seed,
                               mc.silent = mc.settings$mc.silent,
                               mc.cores = mc.settings$mc.cores,
                               mc.cleanup = mc.settings$mc.cleanup)
    } else if (method == "snow"){
        if (is.numeric(cl)) {
            cl <- makeCluster(c(rep("localhost", cl)),  type = "SOCK")
            on.exit(stopCluster(cl))
        }
        allResults <- clusterApply(cl, seq_len(n), fun2)
    } else {
        allResults <- lapply(seq_len(n), fun2)
    }

    if (best.only) {
        tmp <- sapply(allResults, `[[`, "OFvalue")
        i <- which.min(tmp)
        if (length(i) > 1L)
            warning("several 'best' runs")
        allResults <- allResults[[i]]
    }
    allResults
}
