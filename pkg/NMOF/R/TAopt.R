TAopt <- function(OF, algo = list(), ...) {
    # defaults
    algoD = list(
        nD = 2000L,  # random steps for computing thresholds 
        nT = 10L,    # number of thresholds
        nS = 1000L,  # steps per threshold
        q = 0.5, 
        x0 = NULL, 
        vT = NULL,
        neighbour = NULL,
        printDetail = TRUE,
        printBar = TRUE,
        stepUp = 0L,
        scale = 1)
    
    # user *must* specify the following
    if (is.null(algo$neighbour)){
        stop("specify a neighbourhood function") }
    if (!is.function(algo$neighbour)){
        stop("'algo$neighbour' must be a function") }
    if (is.null(algo$x0)){
        stop("specify start solution 'algo$x0'") }
    
    OF1 <- function(x) OF(x, ...)
    N1 <- function(x) algo$neighbour(x, ...)
    algoD[names(algo)] <- algo; algo <- algoD
    
    nT <- as.integer(algo$nT)
    if (nT < 1)
        stop("'nT' must be a positive integer")
    
    # evaluate x0 if function
    if(is.function(algo$x0)) 
        x0 <- algo$x0() else x0 <- algo$x0
    
    printDetail <- algo$printDetail
    printBar <- algo$printBar
    if (printBar && printDetail) printBar <- FALSE 
    if (printDetail) 
        cat("\nThreshold Accepting.\n")
    # compute thresholds
    if (is.null(algo$vT)) {
        if (printDetail) {
            cat("\nComputing thresholds ... ")
            gc(FALSE)
            startTime <- proc.time()
        }
        if (printBar)
            whatGen <- txtProgressBar (min = 1, max = algo$nD, style = 3)
        xc  <- x0
        xcF <- OF1(xc)
        diffF <- numeric(algo$nD)
        diffF[] <- NA 
        for(i in seq_len(algo$nD)){
            if(printBar) 
                setTxtProgressBar(whatGen, value = i)
            xn  <- N1(xc)
            xnF <- OF1(xn)
            diffF[i] <- abs(xcF - xnF)
            xc  <- xn
            xcF <- xnF
        }
        vT <- algo$q * ( ((nT-1L):0L) / nT )
        vT <- quantile(diffF, vT,na.rm = FALSE)
        vT[nT] <- 0 # set last threshold to zero
        if (printBar)
            close(whatGen)
        if (printDetail) {
            cat("OK.")
            endTime <- proc.time()
            cat("\nEstimated remaining running time:", 
                as.numeric(endTime[3L] - startTime[3L]) /
                    algo$nD * nT * algo$nS * (algo$stepUp + 1L), 
                "secs.\n\n")
            flush.console()
        }
    } else vT <- algo$vT
    if (algo$stepUp > 0) 
        vT <- rep(vT, as.integer(algo$stepUp) + 1L)
    if (algo$scale < 0) {
        scale <- 0
        warning("'scale' set to 0 ('scale' must be nonnegative)")
    } else {
        scale <- algo$scale    
    }
    vT <- vT * scale
    nT <- length(vT)
    # evaluate initial solution
    xc <- x0; xcF <- OF1(xc)
    xbest <- xc; xbestF <- xcF 
    Fmat <- array(NA, dim = c(algo$nS * nT, 2L))
    counter <- 0L
    # ------------------------------------------------------------------	
    # main algorithm
    # ------------------------------------------------------------------
    if (printDetail) {
        cat("\nRunning Threshold Accepting...\n")
        cat("Initial solution: ", prettyNum(xbestF),"\n")
        flush.console()
    }
    if (printBar) 
        whatGen <- txtProgressBar (min = 1, max = nT*algo$nS, style = 3)
    for (t in seq_len(nT)){        
        for (s in seq_len(algo$nS)){
            xn <- N1(xc)
            xnF <- OF1(xn)
            if (xnF <= (xcF + vT[t])) {
                xc <- xn
                xcF <- xnF
                if (xnF <= xbestF) {
                    xbest <- xn
                    xbestF <- xnF
                }
            }
            counter <- counter + 1L
            if(printBar) 
                setTxtProgressBar(whatGen, value = counter)
            Fmat[counter, 1L] <- xnF  # proposed solution
            Fmat[counter, 2L] <- xcF  # accepted solution (cummin = xbestF)
        }
        if (printDetail){
            cat("Best solution (threshold ", 
                t, "/", nT, "): ", 
                prettyNum(xbestF),"\n", sep = "")
            flush.console()
        }
    }
    if (printDetail)
        cat("Finished.\nBest solution overall: ", prettyNum(xbestF), "\n", 
            sep = "")
    if (printBar) 
        close(whatGen)
    # return best solution
    list(xbest = xbest, OFvalue = xbestF, Fmat = Fmat, vT = vT)
}
