DEopt <- function(OF, algo = list(), ...) {
	
	# defaults for list 'algo'
	algoD <- list(
			nP = 50L, nG = 300L, F = 0.5, CR = 0.9,
			min = NULL, max = NULL, pen = NULL, repair = NULL, 
			loopOF = TRUE, loopPen = TRUE, loopRepair = TRUE, 
			printDetail = TRUE, printBar = TRUE,
			initP = NULL, 
			storeF = TRUE,
			storeSolutions = FALSE)
	
	# checks for list 'algo'
	if ("" %in% names(algo)) 
		warning("'algo' contained unnamed elements")
	unusedOptions <- setdiff(names(algo), names(algoD))
	unusedOptions <- setdiff(unusedOptions, "")
	if (length(unusedOptions)) 
		warning("unknown names in 'algo': ", 
				paste(unusedOptions, collapse = ", "))
	
	# add defaults
	algoD[names(algo)] <- algo  
	
	# check min/max
	vmax <- as.vector(algoD$max)
	vmin <- as.vector(algoD$min)
	if(is.null(vmin))
		stop("specify 'min' vector") 
	if(is.null(vmax))
		stop("specify 'max' vector") 
	if(length(vmax) != length(vmin))
		stop("'max' and 'min' have different lengths") 
	if (!is.vector(vmax)) 
		stop("'max' must be a vector")
	if (!is.vector(vmin)) 
		stop("'min' must be a vector")
	if (any(vmin > vmax)) 
		stop("(at least) some 'max' < 'min'")  
	
	# get number of decision variables
	d <- length(vmax)
	
	# check other elements
	if ( algoD$CR > 1 || algoD$CR < 0 ) 
		stop("'CR' must be between 0 and 1")
	if ( any(algoD$F > 1) || any(algoD$F < 0) ) 
		warning("'F' is typically between 0 and 1")
	if ( length(algoD$F) > 1L && length(algoD$F) != d ) { 
		warning("only first element of 'F' will be used")
		F <- algoD$F[1L]
	}
	F <- algoD$F[1L]
	
	printDetail <- algoD$printDetail
	printBar <- algoD$printBar
	
	# check integers
	nP <- as.integer(algoD$nP)
	nG <- as.integer(algoD$nG)
	if (nP < 1L) 
		stop("'algo$nP' must be a nonnegative integer")
	if (nG < 1L) 
		stop("'algo$nG' must be a nonnegative integer")
	
	OF1 <- function(x) OF(x, ...)
	Pe1 <- function(x) algoD$pen(x, ...)
	Re1 <- function(x) algoD$repair(x, ...)
	
	# auxiliary functions
	shift <- function(x) c(x[nP], x[seq_len(nP - 1L)])
	mRU <- function(m, n) array(runif(m * n), dim = c(m, n)) 
		
	
	snP <- seq_len(nP)
	
	# MAIN ALGORITHM
	# set up initial population
	vF <- numeric(nP); vF[] <- NA
	vPv <- vF; vFv <- vF 
	if (algoD$storeF)
		Fmat <- array(NA, c(nG, nP)) else Fmat <- NA
	if (algoD$storeSolutions)
		Plist <- list() else Plist <- NA
	
	if (is.null(algoD$initP)) {
		mP <- vmin + diag(vmax - vmin) %*% mRU(d, nP)
	} else {
		if (is.function(algoD$initP))
			mP <- algoD$initP() else mP <- algoD$initP
		if (any(dim(mP) != c(d,nP)))
			stop("supplied population has wrong dimension")
	}
	# evaluate initial population
	# 1) repair
	if (!is.null(algoD$repair) ){
		if(algoD$loopRepair){
			for (s in snP) mP[ ,s] <- Re1(mP[ ,s])
		} else {
			mP <- Re1(mP)
		}
	}
	# 2) evaluate
	if (algoD$loopOF){
		for (s in snP) vF[s] <- OF1(mP[ ,s])
	} else {	
		vF <- OF1(mP)
	}
	# 3) penalise
	if (!is.null(algoD$pen)) {
		if (algoD$loopPen) {
			for (s in snP) vPv[s] <- Pe1(mP[ ,s])
		} else {
			vPv <- Pe1(mP)
		}
		vF <- vF + vPv
	}
	
	if (printBar)
		whatGen <- txtProgressBar (min = 1, max = nG, style = 3)
	if (printDetail)
		cat('\nDifferential Evolution.\n')
	
	for (g in seq_len(nG)) {
		if(printBar) 
			setTxtProgressBar(whatGen, value = g)
		# update population
		vI <- sample.int(nP)
		R1 <- shift(vI)
		R2 <- shift(R1)
		R3 <- shift(R2)
		
		# prelim. update
		mPv <- mP[ ,R1] + F * (mP[ ,R2] - mP[ ,R3])
		vI <- runif(d*nP) > algoD$CR
		mPv[vI] <- mP[vI]
		
		# evaluate updated population
		if (!is.null(algoD$repair)) {
			if (algoD$loopRepair) {
				for (s in snP) mPv[ ,s] <- Re1(mPv[ ,s])
			} else {
				mPv <- Re1(mPv)
			}
		}
		if (algoD$loopOF) {
			for (s in snP) vFv[s] <- OF1(mPv[ ,s])
		} else {	
			vFv <- OF1(mPv)
		}
		if (!is.null(algoD$pen)) {
			if (algoD$loopPen){
				for (s in snP) vPv[s] <- Pe1(mPv[ ,s])
			} else {
				vPv <- Pe1(mPv)
			}
			vFv <- vFv + vPv			
		}
		# find improvements
		logik <- vFv < vF
		mP[ ,logik] <- mPv[ ,logik]
		vF[logik] <- vFv[logik]
		if (algoD$storeF)
			Fmat[g, ] <- vF
		if (algoD$storeSolutions)
			Plist[[g]] <- mP
		
	} # end of generations 
	
	if (printBar)
		close(whatGen)
	if (printDetail)
		cat("\nStandard deviation of OF in final population is ", 
				prettyNum(sd(vF)), ".\n\n", sep = "")
	
	# return best solution
	sGbest <- min(vF); sgbest <- which.min(vF)[1L]
	list(xbest = mP[ ,sgbest], OFvalue = sGbest, 
			popF = vF, Fmat = Fmat, Plist = Plist)
}
