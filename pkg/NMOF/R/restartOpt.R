restartOpt <- function(fun, n, OF, algo, ...) {
    n <- as.integer(n)
    stopifnot(n > 0L)
    allResults <- vector("list", n)
    for (i in seq_len(n)) allResults[[i]] <- fun(OF, algo, ...)        
    allResults
}


#restartOpt2 <- function (fun, n, OF, algo = NULL, ..., 
#    multicore = FALSE) {
#    n <- as.integer(n)
#    stopifnot(n > 0L)
#    if(!multicore) {
#        allResults <- vector("list", n)
#        for (i in 1L:n) 
#            allResults[[i]] <- fun(OF, algo, ...)
#    } else {
#        isMC <- suppressWarnings(require(multicore, quietly = TRUE))
#        if (!isMC) 
#            stop("cannot attach package multicore")
#        makeFun <- function(fun, OF, algo, ...)
#            function(...) fun(OF, algo, ...)
#        fun2 <- makeFun(fun, OF, algo, ...)
#        allResults <- lapply(1L:n, fun2, ...)
#    }
#    allResults
#}


