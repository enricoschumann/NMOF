gridSearch <- function(fun, levels, ..., lower, upper, npar, n = 5L, 
    printDetail = TRUE, multicore = FALSE) {
    
    if (multicore && 
        !suppressWarnings(require("multicore", quietly = TRUE))) {
        multicore <- FALSE
        warning("package 'multicore not available")
    }
    
    if (!missing(lower) && !missing(upper)) {
        lower <- as.numeric(lower)
        upper <- as.numeric(upper)
        if (length(lower) > 1L && length(upper) == 1L) {  
            upper <- rep(upper, length(lower))
        } else if (length(lower) == 1L && length(upper) > 1L) {
            lower <- rep(lower, length(upper))
        }
    }
    
    if (missing(levels) && length(lower) > 1L) {
        if (length(lower) != length(upper))
            stop("'lower' and 'upper' must be of same length")
        if (!missing(npar))
            warning("'npar' is ignored")
        npar <- length(lower)
        if (length(lower) != length(upper))
            stop("'lower' and 'upper' must be of same length")
        temp <- array(NA, dim = c(n,npar))
        for (i in seq_len(npar))
            temp[ ,i] <- seq(lower[i], upper[i], length.out = n)       
        levels <- as.list(as.data.frame(temp))
    } 
    dfLevels <- expand.grid(levels, KEEP.OUT.ATTRS = FALSE, 
        stringsAsFactors = FALSE)
    lstLevels <- vector("list", length = nrow(dfLevels))
    for (r in seq_len(nrow(dfLevels))) 
        lstLevels[[r]] <- as.numeric(dfLevels[r, ])
    if (multicore) {
        results <- mclapply(lstLevels,fun)
    } else {
        results <- lapply(lstLevels,fun)
    }
    results <- unlist(results)
    i <- which.min(results)
    list(minfun = results[i], minlevels = lstLevels[[i]], 
        values = results, levels = lstLevels)    
}
