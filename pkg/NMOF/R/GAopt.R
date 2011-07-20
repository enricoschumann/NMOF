GAopt <- function (OF, algo = list(), ...) {
    algoD <- list(nB = NA, nP = 50L, nG = 300L, prob = 0.01,
        pen = NULL, repair = NULL, 
        loopOF = TRUE, loopPen = TRUE, loopRepair = TRUE, 
        printDetail = TRUE, printBar = TRUE, initP = NULL,
        crossover = c("onePoint", "uniform")
    )
    algoD[names(algo)] <- algo
    algo <- algoD
    printDetail <- algo$printDetail
    printBar <- algo$printBar
    OF1 <- function(x) OF(x, ...)
    Pe1 <- function(x) algo$pen(x, ...)
    Re1 <- function(x) algo$repair(x, ...)
    if (is.na(algo$nB) || is.null(algo$nB))
        stop("'nB' must be specified")
    if (algo$prob > 1 || algo$prob < 0) 
        stop("'prob' must be between 0 and 1")
    nB <- as.integer(algo$nB)
    nP <- as.integer(algo$nP)
    nG <- as.integer(algo$nG)
    crossover <- algo$crossover[1L]
    crossOver1 <- FALSE; crossOver2 <- FALSE
    if (crossover == "onePoint") {
        crossOver <- function(x,y) {
            cutoff <- sample.int(nB-1L, 1L) + 1L
            ii <- cutoff:nB
            x[ii] <- y[ii]
            x
        }
        crossOver1 <- TRUE
    } else if (crossover == "uniform") {
        crossOver <- function(x,y) {
            ii <- runif(nB) > 0.5 
            x[ii] <- y[ii]
            x
        }
        crossOver2 <- TRUE
    } else {
        stop("unknown crossover type")   
    }
    switch <- function(x) !x
    shift <- function(x) c(x[nP], x[1L:(nP - 1L)])
    mRU <- function(m, n) array(runif(m * n), dim = c(m, n))
    mRN <- function(m, n) array(rnorm(m * n), dim = c(m, n))
    
    vF <- numeric(nP)
    vF[] <- NA
    vP <- vF
    vFc <- vF
    Fmat <- array(NaN, c(nG, nP))
    # create population
    if (is.null(algo$initP)) {
        mP <- array(sample.int(2L, nB * nP, replace = TRUE) - 1L, dim = c(nB,nP))
        storage.mode(mP) <- "logical"
    } else {
        if (is.function(algo$initP)) 
            mP <- algo$initP()
        else 
            mP <- algo$initP
        
        if (mode(mP) != "logical") {
            storage.mode(mP) <- "logical"
            warning("'mP' is not of mode logical. 'storage.mode(mP)' will be tried")
        }
    }
    if (!is.null(algo$repair)) {
        if (algo$loopRepair) {
            for (s in seq_len(nP)) mP[, s] <- Re1(mP[, s])
        } else {
            mP <- Re1(mP)
        }
    }
    if (algo$loopOF) {
        for (s in seq_len(nP)) vF[s] <- OF1(mP[, s])
    } else {
        vF <- OF1(mP)
    }
    if (!is.null(algo$pen)) {
        if (algo$loopPen) {
            for (s in seq_len(nP)) vP[s] <- Pe1(mP[, s])
        } else {
            vP <- Pe1(mP)
        }
        vF <- vF + vP
    }
    if (printBar) 
        whatGen <- txtProgressBar(min = 1, max = nG, style = 3)
    if (printDetail) 
        cat("\nGenetic Algorithm.\n")
    for (g in seq_len(nG)) {
        if (printBar) 
            setTxtProgressBar(whatGen, value = g)
        
        # create children ...
        mC <- array(NA, dim = dim(mP))
        
        # ... through crossover
        o <- sample.int(nP)
        oo <- shift(o)
        if (crossOver1) {
            for (s in seq_len(nP))
                mC[ ,s] <- crossOver(mP[ ,o[s]],mP[ ,oo[s]])
        } else if (crossOver2) {
            mC <- crossOver(mP[ ,o],mP[ ,oo])
        }
        
        # ... through mutation
        mutate <- runif(nB*nP) < algo$prob
        mC[mutate] <- switch(mC[mutate])
        
        if (!is.null(algo$repair)) {
            if (algo$loopRepair) {
                for (s in seq_len(nP)) mC[ ,s] <- Re1(mC[ ,s])
            } else {
                mC <- Re1(mC)
            }
        }
        if (algo$loopOF) {
            for (s in seq_len(nP)) vFc[s] <- OF1(mC[, s])
        } else {
            vFc <- OF1(mC)
        }
        if (!is.null(algo$pen)) {
            if (algo$loopPen) {
                for (s in seq_len(nP)) vP[s] <- Pe1(mC[, s])
            } else {
                vP <- Pe1(mC)
            }
            vFc <- vFc + vP
        }
        # pairwise comparison
        logik <- vFc < vF
        mP[, logik] <- mC[, logik]
        vF[logik] <- vFc[logik]
        Fmat[g, ] <- vF
    }
    if (printBar) 
        close(whatGen)
    if (printDetail) 
        cat("\nStandard deviation of OF in final population is ", 
            prettyNum(sd(vF)), ".\n\n", sep = "")
    sGbest <- min(vF)
    sgbest <- which.min(vF)[1L]
    list(xbest = mP[, sgbest], OFvalue = sGbest, popF = vF, Fmat = Fmat)
}