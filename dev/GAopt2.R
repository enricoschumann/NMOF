GAopt2 <- function (OF, algo = list(), ...) {
    algoD <- list(nB = NA,              ## bits per solution
                  nP = 50L,             ## population size
                  nG = 300L,            ## number of generations
                  prob = 0.01,          ## probability of mutation
                  pen    = NULL,        ## penalty function
                  repair = NULL,        ## repair function
                  loopOF     = TRUE,
                  loopPen    = TRUE,
                  loopRepair = TRUE,
                  parOF      = FALSE,   ## FALSE,'snow','multicore'
                  parPen     = FALSE,   ## FALSE,'snow','multicore'
                  parRepair  = FALSE,   ## FALSE,'snow','multicore'
                  cl = NULL,            ##
                  mc.control = list(),  ##
                  printDetail = TRUE,
                  printBar = TRUE,
                  initP = NULL,
                  storeF = TRUE,
                  storeSolutions = FALSE,
                  crossover = c("onePoint", "uniform")
                  )

    ## called for its side effect: gives warning/error
    NMOF:::checkList(algo, algoD)

    ## add default settings
    algoD[names(algo)] <- algo

    if (is.null(algoD$nB))
        stop("'algo$nB' must be specified")
    if (algoD$prob > 1 || algoD$prob < 0)
        stop("'algo$prob' must be between 0 and 1")
    nB <- NMOF:::makeInteger(algoD$nB, 'algo$nB')
    nP <- NMOF:::makeInteger(algoD$nP, 'algo$nP')
    nG <- NMOF:::makeInteger(algoD$nG, 'algo$nG')

    cl <- algoD$cl
    lP <- vector(mode = "list", length = nP)  ## length nP

    ## prepare OF, rep, penalty
    if (algoD$loopOF)
        methodOF <- "loop" else methodOF <- "vec"
    if (algoD$loopPen)
        methodPen <- "loop" else methodPen <- "vec"
    if (algoD$loopRepair)
        methodRepair <- "loop" else methodRepair <- "vec"

    if (algoD$parOF != FALSE  ||
        algoD$parPen != FALSE ||
        algoD$parRepair != FALSE) {
            loopfun <- function(x, f, ...) {
                ns <- ncol(x)
                fv <- numeric(ns)
                for (i in seq_len(ns))
                    fv[i] <- f(x[ ,i], ...)
                fv
            }
    }

    parInit <- FALSE
    if (algoD$parOF == "snow") {
        if (!suppressWarnings(require("snow", quietly = TRUE))) {
            warning("package 'snow' not available")
            methodOF <- paste(methodOF, ".seq", sep = "")
        } else if (is.null(cl)) {
            warning("no cluster 'cl' passed for method 'snow'")
            methodOF <- paste(methodOF, ".seq", sep = "")
        } else {
            methodOF <- paste(methodOF, ".snow", sep = "")
            if (is.numeric(cl)) {
                cl <- makeCluster(c(rep("localhost", cl)), type = "SOCK")
                on.exit(stopCluster(cl))
            }
            splt <- splitIndices(nP, length(cl))
            listP <- vector(mode = "list", length = length(cl))
            parInit <- TRUE
        }
    } else if (algoD$parOF == "multicore") {
        if (!suppressWarnings(require("multicore", quietly = TRUE))) {
            warning("package 'multicore' not available")
            methodOF <- paste(methodOF, ".seq", sep = "")
        } else {
            methodOF <- paste(methodOF, ".mc", sep = "")
            mc.settings <- mcList(algoD$mc.control)
            parInit <- TRUE
        }
    }

    if (identical(parInit, FALSE) && algoD$parPen == "snow") {
        if (!suppressWarnings(require("snow", quietly = TRUE))) {
            warning("package 'snow' not available")
            methodPen <- paste(methodPen, ".seq", sep = "")
        } else if (is.null(cl)) {
            warning("no cluster 'cl' passed for method 'snow'")
            methodPen <- paste(methodPen, ".seq", sep = "")
        } else {
            methodPen <- paste(methodPen, ".snow", sep = "")
            if (is.numeric(cl)) {
                cl <- makeCluster(c(rep("localhost", cl)), type = "SOCK")
                on.exit(stopCluster(cl))
            }
            splt <- splitIndices(nP, length(cl))
            listP <- vector(mode = "list", length = length(cl))
            parInit <- TRUE
        }
    } else if (algoD$parOF == "multicore") {
        if (!suppressWarnings(require("multicore", quietly = TRUE))) {
            warning("package 'multicore' not available")
            methodPen <- paste(methodPen, ".seq", sep = "")
        } else {
            methodPen <- paste(methodOF, ".mc", sep = "")
            mc.settings <- mcList(algoD$mc.control)
            parInit <- TRUE
        }
    }

    if (identical(parInit, FALSE) && algoD$parRepair == "snow") {
        if (!suppressWarnings(require("snow", quietly = TRUE))) {
            warning("package 'snow' not available")
            methodRepair <- paste(methodRepair, ".seq", sep = "")
        } else if (is.null(cl)) {
            warning("no cluster 'cl' passed for method 'snow'")
            methodRepair <- paste(methodRepair, ".seq", sep = "")
        } else {
            methodRepair <- paste(methodRepair, ".snow", sep = "")
            if (is.numeric(cl)) {
                cl <- makeCluster(c(rep("localhost", cl)), type = "SOCK")
                on.exit(stopCluster(cl))
            }
            splt <- splitIndices(nP, length(cl))
            listP <- vector(mode = "list", length = length(cl))
            parInit <- TRUE
        }
    } else if (algoD$parOF == "multicore") {
        if (!suppressWarnings(require("multicore", quietly = TRUE))) {
            warning("package 'multicore' not available")
            methodOF <- paste(methodRepair, ".seq", sep = "")
        } else {
            methodOF <- paste(methodRepair, ".mc", sep = "")
            mc.settings <- mcList(algoD$mc.control)
            parInit <- TRUE
        }
    }

    types <- c("loop.seq", "loop.snow", "loop.mc",
                "vec.seq",  "vec.snow",  "vec.mc")
    methodOF <- match(methodOF, types)
    methodPen <- match(methodPen, types)
    methodRepair <- match(methodRepair, types)

    printDetail <- algoD$printDetail
    printBar <- algoD$printBar
    if (printBar && printDetail > 1L)
        printBar <- FALSE

    if (!is.function(OF))
        stop("'OF' must be a function")
    OF1 <- function(x)
        OF(x, ...)
    Pe1 <- function(x)
        algoD$pen(x, ...)
    Re1 <- function(x)
        algoD$repair(x, ...)

    crossover <- tolower(algoD$crossover[1L])
    crossOver1 <- FALSE
    crossOver2 <- FALSE
    if (crossover == "onepoint") {
        crossOver <- function(x, y) {
            cutoff <- sample.int(nB-1L, 1L) + 1L
            ii <- cutoff:nB
            x[ii] <- y[ii]
            x
        }
        crossOver1 <- TRUE
    } else if (crossover == "uniform") {
        crossOver <- function(x, y) {
            ii <- runif(nB) > 0.5
            x[ii] <- y[ii]
            x
        }
        crossOver2 <- TRUE
    } else {
        stop("unknown crossover type")
    }

    snP <- seq_len(nP)
    snP1 <- c(nP, snP[-nP])

    vF <- numeric(nP)
    vF[] <- NA
    vFc <- vP <- vF

    if (algoD$storeF)
        Fmat <- array(NA, c(nG, nP)) else Fmat <- NA
    if (algoD$storeSolutions)
        xlist <- list(P = vector("list", length = nG)) else xlist <- NA

    ## create population
    if (is.null(algoD$initP)) {
        mP <- array(sample.int(2L, nB * nP, replace = TRUE) - 1L,
                    dim = c(nB, nP))
        storage.mode(mP) <- "logical"
    } else {
        if (is.function(algoD$initP))
            mP <- algoD$initP()
        else
            mP <- algoD$initP
        if (mode(mP) != "logical") {
            storage.mode(mP) <- "logical"
            warning("'initP' is not of mode logical; ",
                    "'storage.mode(initP)' will be tried")
        }
    }

    ## repair initial population
    if (!is.null(algoD$repair)) {
        if (algoD$loopRepair) {
            for (s in snP)
                mP[, s] <- Re1(mP[, s])
        } else mP <- Re1(mP)
    }

    switch(methodOF,
       {  ## loop sequential
           for (s in snP)
               vF[s] <- OF1(mP[, s])
       },
       {  ## loop snow
           for (s in snP)
               lP[[s]] <- mP[ ,s]
           vF <- unlist(clusterApply(cl, lP, OF, ...))
       },
       {  ## loop mc
           for (s in snP)
               lP[[s]] <- mP[ ,s]
           vF <- unlist(mclapply(lP, OF, ...,
                                 mc.preschedule = mc.settings$mc.preschedule,
                                 mc.set.seed = mc.settings$mc.set.seed,
                                 mc.silent = mc.settings$mc.silent,
                                 mc.cores = mc.settings$mc.cores,
                                 mc.cleanup = mc.settings$mc.cleanup))
       },
       {  ## vec sequential
           vF <- OF1(mP)
       },
       {  ## vec snow
           for (s in seq_len(length(cl)))
               listP[[s]] <- mP[ ,splt[[s]]]
           vF <- unlist(clusterApply(cl, listP, OF, ...))
       },
       {  ## vec mc

       }
         )

    ## initial penalty
    if (!is.null(algoD$pen)) {
        if (algoD$loopPen) {
            for (s in snP) vP[s] <- Pe1(mP[, s])
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

        ## create children ...
        mC <- array(NA, dim = dim(mP))

        ## ... through crossover
        o <- sample.int(nP)
        oo <- o[snP1]
        if (crossOver1) {
            for (s in snP)
                mC[ ,s] <- crossOver(mP[ ,o[s]],mP[ ,oo[s]])
        } else if (crossOver2) {
            mC <- crossOver(mP[ ,o],mP[ ,oo])
        }

        ## ... through mutation
        mutate <- runif(nB*nP) < algoD$prob
        mC[mutate] <- !mC[mutate]

        ## REPAIR
        if (!is.null(algoD$repair)) {
            if (algoD$loopRepair) {
                for (s in snP) mC[ ,s] <- Re1(mC[ ,s])
            } else {
                mC <- Re1(mC)
            }
        }

        ## EVALUATE OF
        switch(methodOF, ## (loop|vec).(seq|snow|mc)
               {
                   for (s in snP)
                       vFc[s] <- OF1(mC[, s])
               }, {
                   for (s in seq_len(length(cl)))
                       listP[[s]] <- mC[ ,splt[[s]]]
                   vFc <- unlist(clusterApply(cl, listP, loopfun, OF, ...))
                   ## for (s in snP)
                   ##     lP[[s]] <- mC[ ,s]
                   ## vFc <- unlist(clusterApply(cl, lP, OF, ...))
               },
               {
                   for (s in snP)
                       lP[[s]] <- mC[ ,s]
                   vFc <- unlist(mclapply(lP, OF, ...,
                                          mc.preschedule = mc.settings$mc.preschedule,
                                          mc.set.seed = mc.settings$mc.set.seed,
                                          mc.silent = mc.settings$mc.silent,
                                          mc.cores = mc.settings$mc.cores,
                                          mc.cleanup = mc.settings$mc.cleanup))
               }, {
                   vFc <- OF1(mC)
               }, {
               }, {
               }
               )

               ## PENALISE
               if (!is.null(algoD$pen)) {
                   if (algoD$loopPen) {
                       for (s in snP) vP[s] <- Pe1(mC[ ,s])
                   } else {
                       vP <- Pe1(mC)
                   }
                   vFc <- vFc + vP
               }
               ## pairwise comparison
               logik <- vFc < vF
               mP[, logik] <- mC[, logik]
               vF[logik] <- vFc[logik]
               if (algoD$storeF)
               Fmat[g, ] <- vF
               if (algoD$storeSolutions)
               xlist[[c(1L, g)]] <- mP

               ## print info
               if (printDetail > 1) {
                   if (g %% printDetail == 0L) {
                       cat("Best solution (iteration ", g, "/", nG, "): ",
                           prettyNum(min(vF)[1L]),"\n", sep = "")
                       flush.console()
                   }
               }
           }
    if (printBar)
        close(whatGen)
    sGbest <- min(vF)
    sgbest <- which.min(vF)[1L]

    if (printDetail)
        cat("Best solution has objective function value ",
            prettyNum(sGbest), " ;",
            "\nstandard deviation of OF in final population is ",
            prettyNum(sd(vF)), " .\n\n", sep = "")

    list(xbest = mP[, sgbest], OFvalue = sGbest, popF = vF,
         Fmat = Fmat, xlist = xlist)
}
