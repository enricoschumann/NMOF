PSopt <- function(OF, algo = list(), ...) {
    # ------------------------------------------------------------------
    # defaults
    # ------------------------------------------------------------------
    algoD <- list(
        nP = 100L, nG = 500L,
        c1 = 1, c2 = 1,
        iner = 0.9, initV = 1, maxV = 1,
        min = NULL, max = NULL,
        pen = NULL, repair = NULL,
        loopOF = TRUE, loopPen = TRUE, loopRepair = TRUE,
        printDetail = TRUE, printBar = TRUE,
        mP = NULL)
    algoD[names(algo)] <- algo	
    algo <- algoD
    vmax <- as.vector(algo$max)
    vmin <- as.vector(algo$min)
    nP <- as.integer(algo$nP)
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
    # ------------------------------------------------------------------
    # auxiliary functions
    # ------------------------------------------------------------------
    mRU <- function(m,n) array(runif(m*n), dim = c(m,n)) 
    mRN <- function(m,n) array(rnorm(m*n), dim = c(m,n)) 
    pmax2 <- function(x1,x2) ( (x1 + x2) + abs(x1 - x2) ) / 2
    pmin2 <- function(x1,x2) ( (x1 + x2) - abs(x1 - x2) ) / 2    
    
    Fmat <- array(NaN, c(algo$nG, nP))
    # set up initial population and velocity
    d <- length(vmax); vF <- numeric(nP); vF[] <- NA; vPv <- vF
    if (is.null(algo$mP)) {
        mP <- vmin + diag(vmax - vmin) %*% mRU(d, nP)
    } else {
        if (is.function(algo$mP))
            mP <- algo$mP() else mP <- algo$mP
        if (any(dim(mP) != c(d, nP)))
            stop("supplied population has wrong dimension")
        
    }
    mV <- algo$initV * mRN(d,nP)
    # evaluate initial population
    if(!is.null(algo$repair)) {
        if(algo$loopRepair) {
            for(s in seq_len(nP)) mP[,s] <- Re1(mP[,s])
        }else{
            mP <- Re1(mP)
        }
    }
    if(algo$loopOF) {
        for(s in seq_len(nP)) vF[s] <- OF1(mP[,s])
    } else {	
        vF <- OF1(mP)
    }
    if(!is.null(algo$pen)) {
        if(algo$loopPen){
            for(s in seq_len(nP)) vPv[s] <- Pe1(mP[,s])
        } else {
            vPv <- Pe1(mP)
        }
        vF <- vF + vPv
    }
    
    # set up best solutions
    mPbest <- mP                    # matrix of 'personally best' solutions
    vFbest <- vF                    # vector of OF of best solutions
    sGbest <- min(vFbest)           # scalar: best OF-value
    sgbest <- which.min(vFbest)[1]	# scalar: best solution (counter)
    
    # start iterations
    if (printDetail)
        cat('\nParticle Swarm Optimisation.\n')
    if (printBar)
        whatGen <- txtProgressBar (min = 1, max = algo$nG, style = 3)
    for (g in seq_len(algo$nG)) {
        if(printBar) 
            setTxtProgressBar(whatGen, value = g)
        # update population
        mDV <-  algo$c1 * mRU(d,nP) * (mPbest - mP) + 
                algo$c2 * mRU(d,nP) * (mPbest[ ,sgbest] - mP)
        mV  <- algo$iner * mV + mDV
        mV <- pmin2(mV, algo$maxV)
        mV <- pmax2(mV,-algo$maxV)
        mP  <- mP + mV
        
        # evaluate updated population
        if (!is.null(algo$repair)) {
            if (algo$loopRepair){
                for (s in seq_len(nP)) mP[,s] <- Re1(mP[ ,s])
            } else {
                mPv <- Re1(mPv)
            }
        }
        if (algo$loopOF) {
            for (s in seq_len(nP)) vF[s] <- OF1(mP[ ,s])
        } else {	
            vF <- OF1(mP)
        }
        if(!is.null(algo$pen)) {
            if (algo$loopPen){
                for (s in seq_len(nP)) vPv[s] <- Pe1(mP[ ,s])
            } else {
                vPv <- Pe1(mP)
            }
            vF <- vF + vPv
        }
        # find improvements
        logik <- vF < vFbest        
        mPbest[,logik] <- mP[ ,logik]
        vFbest[logik] <- vF[logik]
        # find best solution
        if (min(vF) < sGbest) {
            sGbest <- min(vF)
            sgbest <- which.min(vF)[1]
        }
        Fmat[g, ] <- vFbest
    } # end generations
    if (printDetail)
        cat("\nStandard deviation of OF in final population is ",
            prettyNum(sd(vF)), ".\n\n", sep = "")       
    if (printBar)
        close(whatGen)
    # return best solution
    list(xbest = mPbest[,sgbest], OFvalue = sGbest, 
        popF = vFbest, Fmat = Fmat)
}