GAopt <- function (OF, algo = list(), ...) {
    algoD <- list(nB = 100L, nP = 50L, nG = 300L, 
        prob = 0.1,
        pen = NULL, repair = NULL, 
        loopOF = TRUE, loopPen = TRUE, loopRepair = TRUE, 
        printDetail = TRUE, printBar = TRUE, mP = NULL)
    algoD[names(algo)] <- algo
    algo <- algoD
    printDetail <- algo$printDetail
    printBar <- algo$printBar
    OF1 <- function(x) OF(x, ...)
    Pe1 <- function(x) algo$pen(x, ...)
    Re1 <- function(x) algo$repair(x, ...)
    nP <- algo$nP
    crossOver <- function(x,y) {
        cutoff <- sample.int(algo$nB-1L, 1L) + 1L
        ii <- cutoff:algo$nB
        x[ii] <- y[ii]
        x
    }
    switch <- function(x) !x
    shift <- function(x) c(x[nP], x[1L:(nP - 1L)])
    mRU <- function(m, n) array(runif(m * n), dim = c(m, n))
    mRN <- function(m, n) array(rnorm(m * n), dim = c(m, n))
    
    vF <- numeric(nP)
    vF[] <- NA
    vP <- vF
    vFc <- vF
    Fmat <- array(NaN, c(algo$nG, nP))
    # create population
    if (is.null(algo$mP)) {
        mP <- array(sample.int(2L, algo$nB * nP, replace = TRUE) - 1L, 
            dim = c(algo$nB,nP))
        storage.mode(mP) <- "logical"
    } else {
        if (is.function(algo$mP)) 
            mP <- algo$mP()
        else 
            mP <- algo$mP
        if (mode(mP) != "logical") {
            storage.mode(mP) <- "logical"
            warning("'mP' is not of mode logical. 'storage.mode(mP)' will be tried")
        }
    }
    if (!is.null(algo$repair)) {
        if (algo$loopRepair) {
            for (s in 1L:nP) mP[, s] <- Re1(mP[, s])
        }
        else {
            mP <- Re1(mP)
        }
    }
    if (algo$loopOF) {
        for (s in 1L:nP) vF[s] <- OF1(mP[, s])
    } else {
        vF <- OF1(mP)
    }
    if (!is.null(algo$pen)) {
        if (algo$loopPen) {
            for (s in 1L:nP) vP[s] <- Pe1(mP[, s])
        }
        else {
            vP <- Pe1(mP)
        }
        vF <- vF + vP
    }
    if (printBar) 
        whatGen <- txtProgressBar(min = 1, max = algo$nG, style = 3)
    if (printDetail) 
        cat("\nGenetic Algorithm.\n")
    for (g in 1L:algo$nG) {
        if (printBar) 
            setTxtProgressBar(whatGen, value = g)
        
        # create children
        mC <- array(NA, dim = dim(mP))
        # ... crossover
        o <- sample.int(nP)
        oo <- shift(o)
        for (s in 1L:nP)
            mC[ ,s] <- crossOver(mP[ ,o[s]],mP[ ,oo[s]])
        # mutate
        mutate <- runif(algo$nB*nP) < algo$prob
        mC[mutate] <- switch(mC[mutate])
        
        if (!is.null(algo$repair)) {
            if (algo$loopRepair) {
                for (s in 1L:nP) mC[ ,s] <- Re1(mC[ ,s])
            }
            else {
                mC <- Re1(mC)
            }
        }
        if (algo$loopOF) {
            for (s in 1L:nP) vFc[s] <- OF1(mC[, s])
        }
        else {
            vFc <- OF1(mC)
        }
        if (!is.null(algo$pen)) {
            if (algo$loopPen) {
                for (s in 1L:nP) vP[s] <- Pe1(mC[, s])
            }
            else {
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