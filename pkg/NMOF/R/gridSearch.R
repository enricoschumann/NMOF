gridSearch <- function(fun, levels, ..., lower, upper,
                       npar = 1L, n = 5L,
                       printDetail = TRUE,
                       method = c("loop", "multicore", "snow"),
                       mc.control = list(),
                       cl = NULL,
                       keepNames = FALSE, asList = FALSE) {

    if (keepNames)
        warning("'keepNames' is not supported yet")

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
            warning("no cluster 'cl' passed for method 'snow': use method 'loop'")
        }
    }

    n <- makeInteger(n, "'n'", 2L)

    if (missing(levels) && !missing(lower) && !missing(upper)) {
        lower <- as.numeric(lower)
        upper <- as.numeric(upper)
        if (length(lower) > 1L && length(upper) == 1L) {
            upper <- rep(upper, length(lower))
        } else if (length(lower) == 1L && length(upper) > 1L) {
            lower <- rep(lower, length(upper))
        } else if (length(lower) == 1L && length(upper) == 1L) {
            lower <- rep(lower, npar)
            upper <- rep(upper, npar)
        } else if (length(lower) < 1L || length(upper) < 1L) {
            stop("'lower' and 'upper' must have non-zero length")
        }
        if (length(lower) != length(upper))
            stop("'lower' and 'upper' must be of same length")
        if (any(lower > upper))
            stop("'lower' must not be greater than 'upper'")
        npar <- length(lower)
        levels <- vector("list", length = length(lower))
        for (i in seq_len(npar))
            levels[[i]] <- seq(lower[[i]], upper[[i]], length.out = max(n,2L))
    }
    np <- length(levels)
    res <- vector("list", np)
    rep.fac <- 1L
    nl <- sapply(levels, length)
    nlp <- prod(nl)
    if (printDetail) {
        if (np < 5L)
            msg <- paste(nl, collapse=", ")
        else
            msg <- paste(c(nl[seq_len(4L)],"..."), collapse=", ")
        message(np, " variables with ", msg, " levels: ",
                nlp, " function evaluations required.")
    }
    for (i in seq_len(np)) {
        x <- levels[[i]]
        nx <- length(x)
        nlp <- nlp/nx
        res[[i]] <- x[rep.int(rep.int(seq_len(nx), rep.int(rep.fac, nx)), nlp)]
        rep.fac <- rep.fac * nx
    }
    nlp <- prod(nl)
    lstLevels <- vector("list", length = nlp)
    for (r in seq_len(nlp)) {
        lstLevels[[r]] <- if (asList)
            as.list(as.numeric(sapply(res,`[[`, r))) else
        as.numeric(sapply(res,`[[`, r))
    }
    if (method == "multicore") {
        mc.settings <- mcList(mc.control)
        results <- mclapply(lstLevels, fun, ...,
                            mc.preschedule = mc.settings$mc.preschedule,
                            mc.set.seed = mc.settings$mc.set.seed,
                            mc.silent = mc.settings$mc.silent,
                            mc.cores = mc.settings$mc.cores,
                            mc.cleanup = mc.settings$mc.cleanup
                            )
    } else if (method == "snow") {
        if (is.numeric(cl)) {
            cl <- makeCluster(c(rep("localhost", cl)),  type = "SOCK")
            on.exit(stopCluster(cl))
        }
        results <- clusterApply(cl, lstLevels, fun, ...)
    } else {
        ## loop
        results <- lapply(lstLevels, fun, ...)
    }
    results <- unlist(results)
    i <- try(which.min(results))

    ## what to return
    if (inherits(i, "try-error") || any(is.na(i)) || length(i) == 0L) {
        ## function evaluated to NA, or results are empty
        warning("cannot compute minimum (NA values in results, ...)")
        list(minfun = NA, minlevels = NA,
             values = results, levels = lstLevels)
    } else {
        ## (apparently) no problems
        list(minfun = results[i], minlevels = lstLevels[[i]],
             values = results, levels = lstLevels)
    }
}
