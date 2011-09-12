bracketing <- function(fun, interval, ...,
                       lower = min(interval),
                       upper = max(interval),
                       n = 20L,
                       method = c("loop", "vectorise", "multicore"),
                       mc.control = list()) {

    n <- as.integer(n)
    if (n < 2L)
        stop("'n' must be at least 2")

    method <- method[1L]
    if (tolower(method) == "vectorize")
        method <- "vectorise"
    if (method == "multicore"){
        if (!suppressWarnings(require("multicore", quietly = TRUE))) {
            method <- "loop"
            warning("package 'multicore' not available: use method 'loop'")
        }
        mc.settings <- list(mc.preschedule = TRUE, mc.set.seed = TRUE,
                            mc.silent = FALSE, mc.cores = getOption("cores"),
                            mc.cleanup = TRUE)
        mc.settings[names(mc.control)] <- mc.control
    }

    lower <- min(interval)
    upper <- max(interval)

    xs <- seq(from = lower,to = upper, length.out = n)
    switch(tolower(method),
           loop = {
               fn <- numeric(n)
               for (i in seq_len(n))
                   fn[i] <- fun(xs[i], ...)
           },
           vectorise = fn <- fun(xs, ...),
           multicore = {
               fn <- mclapply(lstLevels, fun, ...,
                              mc.preschedule = mc.settings$mc.preschedule,
                              mc.set.seed = mc.settings$mc.set.seed,
                              mc.silent = mc.settings$mc.silent,
                              mc.cores = mc.settings$mc.cores,
                              mc.cleanup = mc.settings$mc.cleanup)
           }
           ) ## end switch

    ## find diffs
    iSigns <- which(fn[-n] * fn[-1L] < 0)
    cbind(xs[iSigns], xs[iSigns+1L])
}
