DEopt <- function(OF, algo = list(), ...) {
    # ------------------------------------------------------------------    
    # default settings and checks
    # ------------------------------------------------------------------
    algoD <- list(
        nP = 50L, nG = 300L, 
        F = 0.5, CR = 0.9,
        min = NULL, max = NULL, 
        pen = NULL, repair = NULL, 
        loopOF = TRUE, loopPen = TRUE, loopRepair = TRUE, 
        printDetail = TRUE, printBar = TRUE,
        mP = NULL)
    algoD[names(algo)] <- algo  
    algo <- algoD
    vmax <- as.vector(algo$max)
    vmin <- as.vector(algo$min)
    if(is.null(vmin))
        stop("specify 'min' vector") 
    if(is.null(vmax))
        stop("specify 'max' vector") 
    if(length(vmax) != length(vmin))
        stop("max/min have different lengths") 
    if (!is.vector(vmax)) 
        stop("'max' must be a vector")
    if (!is.vector(vmin)) 
        stop("'min' must be a vector")
    if (any(vmin > vmax)) 
        stop("(at least) some max < min")  
    printDetail <- algo$printDetail
    printBar <- algo$printBar
    OF1 <- function(x) OF(x, ...)
    Pe1 <- function(x) algo$pen(x, ...)
    Re1 <- function(x) algo$repair(x, ...)
    nP <- algo$nP
    # ------------------------------------------------------------------
    # auxiliary functions
    # ------------------------------------------------------------------
    shift <- function(x) c(x[nP], x[1L:(nP - 1L)])
    mRU <- function(m, n) array(runif(m * n), dim = c(m, n)) 
    mRN <- function(m, n) array(rnorm(m * n), dim = c(m, n)) 
    
    # ------------------------------------------------------------------	
    # main algorithm
    # ------------------------------------------------------------------
    # set up initial population
    d <- length(vmax)
    vF <- numeric(nP) 
    vF[] <- NA; vPv <- vF; vFv <- vF 
    Fmat <- array(NaN,c(algo$nG, nP))
    if (is.null(algo$mP)) {
        mP <- vmin + diag(vmax - vmin) %*% mRU(d, nP)
    } else {
        if (is.function(algo$mP))
            mP <- algo$mP() else mP <- algo$mP         
    }
    # evaluate initial population
    # 1) repair
    if(!is.null(algo$repair) ){
        if(algo$loopRepair){
            for(s in 1L:nP) mP[ ,s] <- Re1(mP[ ,s])
        } else {
            mP <- Re1(mP)
        }
    }
    # 2) evaluate
    if(algo$loopOF){
        for(s in 1L:nP) vF[s] <- OF1(mP[ ,s])
    }else{	
        vF <- OF1(mP)
    }
    # 3) penalise
    if( !is.null(algo$pen) ) {
        if(algo$loopPen){
            for(s in 1L:nP) vPv[s] <- Pe1(mP[,s])
        } else {
            vPv <- Pe1(mP)
        }
        vF <- vF + vPv
    }
    
    if (printBar)
        whatGen <- txtProgressBar (min = 1, max = algo$nG, style = 3)
    if (printDetail)
        cat('\nDifferential Evolution.\n')
    
    for (g in 1L:algo$nG) {
        if(printBar) 
            setTxtProgressBar(whatGen, value = g)
        # update population
        vI <- sample(1L:nP, nP)
        R1 <- shift(vI)
        R2 <- shift(R1)
        R3 <- shift(R2)
        
        # prelim. update
        mPv <- mP[ ,R1] + algo$F * (mP[ ,R2] - mP[ ,R3])
        mI <- mRU(d, nP) > algo$CR
        mPv[mI] <- mP[mI]
        
        # evaluate updated population
        if (!is.null(algo$repair)) {
            if(algo$loopRepair){
                for(s in 1L:nP) mPv[ ,s] <- Re1(mPv[ ,s])
            }else{
                mPv <- Re1(mPv)
            }
        }
        if (algo$loopOF) {
            for(s in 1L:nP) vFv[s] <- OF1(mPv[ ,s])
        } else {	
            vFv <- OF1(mPv)
        }
        if (!is.null(algo$pen)) {
            if(algo$loopPen){
                for(s in 1L:nP) vPv[s] <- Pe1(mPv[ ,s])
            } else {
                vPv <- Pe1(mPv)
            }
            vFv <- vFv + vPv			
        }
        # find improvements
        logik <- vFv < vF
        mP[ ,logik] <- mPv[ ,logik]
        vF[logik] <- vFv[logik]
        Fmat[g, ] <- vF
    } # g in 1:nG
    if (printBar)
        close(whatGen)
    if (printDetail)
        cat("\nStandard deviation of OF in final population is ", 
            prettyNum(sd(vF)), ".\n\n", sep = "")
    
    # return best solution
    sGbest <- min(vF); sgbest <- which.min(vF)[1L]
    list(xbest = mP[ ,sgbest], OFvalue = sGbest, 
        popF = vF, Fmat = Fmat)
}
