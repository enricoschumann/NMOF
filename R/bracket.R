bracketing <- function(fun, interval, ...,
                       lower = min(interval),
                       upper = max(interval),
                       n = 20L,
                       method = c("loop",
                       "vectorised", "multicore", "snow"),
                       mc.control = list(),
                       cl = NULL) {

    n <- makeInteger(n, "'n'", 2)
    method <- tolower(method[1L])
    if (method == "vectorize"  ||
        method == "vectorized" ||
        method == "vectorise")
        method <- "vectorised"
    if (!is.null(cl)) method <- "snow"
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
            warning("no cluster 'cl' passed for method 'snow'")
        }
    }
    if (!missing(interval)) {
        if (length(interval) > 2L)
            warning("'interval' is of length > 2")
        lower <- min(interval)
        upper <- max(interval)
    }
    if (!is.numeric(lower) || !is.numeric(upper) || lower >=upper)
        stop("'lower' must be smaller than 'upper'")

    xs <- seq(from = lower, to = upper, length.out = n)
    switch(method,
           loop = {
               fn <- numeric(n)
               for (i in seq_len(n))
                   fn[i] <- fun(xs[i], ...)
           },
           vectorised = fn <- fun(xs, ...),
           multicore = {
               mc.settings <- mcList(mc.control)
               fn <- mclapply(xs, fun, ...,
                              mc.preschedule = mc.settings$mc.preschedule,
                              mc.set.seed = mc.settings$mc.set.seed,
                              mc.silent = mc.settings$mc.silent,
                              mc.cores = mc.settings$mc.cores,
                              mc.cleanup = mc.settings$mc.cleanup)
               fn <- unlist(fn)
           },
           snow = {
               if (is.numeric(cl)) {
                   cl <- makeCluster(c(rep("localhost", cl)), type = "SOCK")
                   on.exit(stopCluster(cl))
               }
               fn <- clusterApply(cl, xs, fun, ...)
               fn <- unlist(fn)
           }
           ) ## end switch

    ## find diffs
    iSigns <- which(fn[-n] * fn[-1L] < 0)
    cbind(xs[iSigns], xs[iSigns+1L])
}
