gridSearch <- function(fun, levels, ..., lower, upper, npar = 1L, n = 5L, 
    printDetail = TRUE, multicore = FALSE, 
    keepNames = FALSE, asList = FALSE) {
    
    if (keepNames) 
        warning("'keepNames' is not supported yet")
    
    if (multicore && 
        !suppressWarnings(require("multicore", quietly = TRUE))) {
        multicore <- FALSE
        warning("package 'multicore' not available")
    }
    
    if (n < 2L) {
        warning("'n' changed to 2")
        n <- 2L
    }
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
            nlp, " function evalutations required.")
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
    if (multicore) {
        results <- mclapply(lstLevels,fun,...)
    } else {
        results <- lapply(lstLevels,fun,...)
    }
    results <- unlist(results)
    i <- which.min(results)
    list(minfun = results[i], minlevels = lstLevels[[i]], 
        values = results, levels = lstLevels)    
}
