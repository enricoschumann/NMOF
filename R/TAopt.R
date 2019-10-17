TAopt <- function(OF, algo = list(), ...) {
    algoD <- list(nD = 2000L, ## random steps for computing thresholds
                  nT = 10L,   ## number of thresholds
                  nS = 1000L, ## steps per threshold
                  nI = NULL,  ## total number of iterations
                  q = 0.5,    ## starting quantile for thresholds
                  x0 = NULL,  ## initial solution
                  vT = NULL,  ## threshold sequence
                  neighbour = NULL,
                  printDetail = TRUE,
                  printBar = TRUE,
                  stepUp = 0L,
                  scale = 1,
                  storeF = TRUE,
                  storeSolutions = FALSE,
                  classify = FALSE,
                  OF.target = NULL,
                  thresholds.only = FALSE)

    checkList(algo, algoD)
    algoD[names(algo)] <- algo
    state <- if (!exists(".Random.seed", envir = .GlobalEnv,
                         inherits = FALSE))
                 NA
             else
                 .Random.seed

    if (!is.null(algoD$nI))
        algoD$nS <- ceiling(algoD$nI/algoD$nT)

    ## user *must* specify the following
    if (is.null(algoD$neighbour))
        stop("specify a neighbourhood function ", sQuote("algo$neighbour"))
    if (!is.function(algoD$neighbour))
        stop(sQuote("algo$neighbour"), " must be a function")
    if (is.null(algoD$x0))
        stop("specify start solution ", sQuote("algo$x0"))

    if (is.function(algoD$x0))
        x0 <- algoD$x0() else x0 <- eval(algoD$x0)

    OF1 <- function(x)
        OF(x, ...)
    N1 <- function(x)
        algoD$neighbour(x, ...)

    target.reached <- FALSE

    printDetail <- algoD$printDetail
    printBar <- algoD$printBar
    if (printBar && printDetail > 1)
        printBar <- FALSE
    if (printDetail)
        cat("\nThreshold Accepting\n")


    nT <- makeInteger(algoD$nT, "algo$nT")
    nS <- makeInteger(algoD$nS, "algo$nS")
    nD <- makeInteger(algoD$nD, "algo$nD")
    stepUp <- makeInteger(algoD$stepUp, "algo$stepUp", 0L)
    niter <- nS * nT * (stepUp+1L)

    ## compute thresholds
    if (is.null(algoD$vT)) {
        if (algoD$q < .Machine$double.eps^0.5) {
            vT <- numeric(nT)
        } else {
            if (printDetail) {
                cat("\n  Computing thresholds ... ")
                if (printBar)
                    cat("\n")
                flush.console()
                gc(FALSE)
                startTime <- proc.time()
            }
            if (printBar)
                whatGen <- txtProgressBar(min = 1, max = nD, style = 3,
                                          width = getOption("width")*0.75)
            xc  <- x0
            xcF <- OF1(xc)
            diffF <- numeric(nD)
            diffF[] <- NA
            for (i in seq_len(nD)){
                if (printBar)
                    setTxtProgressBar(whatGen, value = i)
                xn  <- N1(xc)
                xnF <- OF1(xn)
                diffF[i] <- abs(xcF - xnF)
                xc  <- xn
                xcF <- xnF
            }
            vT <- algoD$q * ((nT - 1L):0)/nT
            if (any(is.na(diffF)))
                stop("objective function evaluated to NA")
            vT <- quantile(diffF, vT, na.rm = FALSE)
            vT[nT] <- 0  ## set last threshold to zero
            if (printBar)
                close(whatGen)
            if (printDetail) {
                cat("  OK", sep = "")
                endTime <- proc.time()
                cat("\n  Estimated remaining running time:",
                    as.numeric(endTime[3L] - startTime[3L]) /
                    nD * niter * (stepUp + 1L),
                    "secs\n")
                flush.console()
            }
        }
    } else {
        vT <- algoD$vT
    }
    if (stepUp > 0L)
        vT <- rep.int(vT, stepUp + 1L)
    if (algoD$scale < 0) {
        scale <- 0
        warning(sQuote("scale"), " set to 0 (",
                sQuote("scale"), " must be nonnegative)")
    } else {
        scale <- algoD$scale
    }
    vT <- vT * scale
    nT <- length(vT)
    niter <- nS * nT

    if (algoD$thresholds.only) {
        ans <- list(xbest = NA,
                    OFvalue = NA,
                    Fmat = NA,
                    xlist = NA,
                    vT = vT,
                    initial.state = state,
                    x0 = x0)
        if (algoD$classify)
            class(ans) <- "TAopt"
        return(ans)
    }

    ## evaluate initial solution
    xc <- x0
    xcF <- OF1(xc)
    xbest <- xc
    xbestF <- xcF

    if (algoD$storeF) {
        Fmat <- array(NA, dim = c(niter, 2L))
        colnames(Fmat) <- c("xnF", "xcF")
    } else Fmat <- NA
    if (algoD$storeSolutions)
        xlist <- list(xn = vector("list", length = niter),
                      xc = vector("list", length = niter)) else xlist <- NA
    if (printDetail) {
        cat("\n  Running Threshold Accepting ...\n")
        cat("  Initial solution:", prettyNum(xbestF),"\n")
        flush.console()
    }
    if (printBar)
        whatGen <- txtProgressBar(min = 1, max = niter, style = 3,
                                  width = getOption("width")*0.75)

    counter <- 0L
    for (t in seq_len(nT)) {
        for (s in seq_len(nS)) {
            ## counter = total number of iterations
            counter <- counter + 1L

            xn <- N1(xc)
            xnF <- OF1(xn)
            if (xnF <= xcF + vT[t]) {
                xc <- xn
                xcF <- xnF
                if (xnF <= xbestF) {
                    xbest <- xn
                    xbestF <- xnF
                }
            }

            if (printBar)
                setTxtProgressBar(whatGen, value = counter)

            ## store OF values
            if (algoD$storeF) {
                Fmat[counter, 1L] <- xnF ## proposed sol.
                Fmat[counter, 2L] <- xcF ## accepted sol. (cummin=xbestF)
            }

            ## store solutions
            if (algoD$storeSolutions) {
                xlist[[c(1L, counter)]] <- xn
                xlist[[c(2L, counter)]] <- xc
            }

            if (printDetail > 1) {
                if (counter %% printDetail == 0L) {
                    cat("  Best solution (iteration ", counter,
                        "/", niter, "): ",
                        prettyNum(xbestF),"\n", sep = "")
                    flush.console()
                }
            }

            ## check stopif value
            if (!is.null(algoD$OF.target) && xbestF <= algoD$OF.target) {
                if (printDetail) {
                    cat("Target value (", prettyNum(algoD$OF.target), ") ",
                        "for objective function reached: ",
                        prettyNum(xbestF), "\n", sep = "")
                    flush.console()
                    target.reached  <- TRUE
                }
                break
            }
        }

        ## the second break is required since
        ## break only exits the innermost loop
        if (target.reached)
            break
    }
    if (printBar)
        close(whatGen)

    if (printDetail)
        cat("  Finished.\n  Best solution overall: ",
            prettyNum(xbestF), "\n", sep = "")

    ans <- list(xbest = xbest, OFvalue = xbestF,
                Fmat = Fmat, xlist = xlist, vT = vT,
                initial.state = state, x0 = x0)
    if (algoD$classify)
        class(ans) <- "TAopt"
    ans
}

TA.info <- function(n = 0L) {
    e <- parent.frame(3L + n)
    step <- NA
    threshold <- NA
    iteration <- NA
    OF.sampling <- NA
    xbest <- NA
    xbestF <- NA
    if (exists("i", envir = e, inherits = FALSE))
        step <- get("i", envir = e, inherits = FALSE)
    if (exists("s", envir = e, inherits = FALSE))
        step <- get("s", envir = e, inherits = FALSE)
    if (exists("t", envir = e, inherits = FALSE))
        threshold <- get("t", envir = e, inherits = FALSE)
    if (exists("counter", envir = e, inherits = FALSE))
        iteration <- get("counter", envir = e, inherits = FALSE)
    if (exists("xbest", envir = e, inherits = FALSE))
        xbest <- get("xbest", envir = e, inherits = FALSE)
    if (exists("xbestF", envir = e, inherits = FALSE))
        xbestF <- get("xbestF", envir = e, inherits = FALSE)
    list(OF.sampling = is.na(iteration),
         iteration = iteration,
         step = step,
         threshold = threshold,
         xbest = if (is.list(xbest)) list(xbest) else xbest,
         OF.xbest = xbestF)
}


        # METHODS

print.TAopt <- function(x, ...) {
    nt <- length(x$vT)
    ns <- dim(x$Fmat)[[1L]]
    cat("Threshold Accepting\n-------------------\n")
    cat(" - ", nt, " thresholds with ", ns, " steps: ",
        format(nt*ns, big.mark = ","), " iterations \n", sep = "")
    ## cat(" - initial objective-function value: ", prettyNum(x$x0),      "\n", sep = "")
    cat(" -   final objective-function value: ", prettyNum(x$OFvalue), "\n", sep = "")
    invisible(x)
}

plot.TAopt <- function(x, y, plot.type = "interactive", ...) {
    if (plot.type == "interactive") {
        dev.new(title = "TAopt: Threshold sequence")
        defaults <- list(x    = x$vT,
                         xlab = "Threshold",
                         ylab = "Values",
                         main = "TAopt: Threshold Sequence",
                         type = "b")
        do.call("plot", defaults)
        if (!is.null(x$Fmat)) {
            dev.new(title = "TAopt: Objective function values")
            defaults <- list(x    = x$Fmat[,1L],
                             xlab = "Iteration",
                             ylab = "Objective function value",
                             main = "TAopt: Objective function values",
                             type = "l",
                             col  = grey(0.6))
            do.call("plot", defaults, ...)

            defaults$x <- x$Fmat[,2L]
            defaults$col <- grey(0.2)
            do.call("lines", defaults, ...)

            defaults$x <- cummin(x$Fmat[,2L])
            defaults$col <- "blue"
            do.call("lines", defaults, ...)
            legend("topright",
                   legend = c("new solution (xn)",
                              "accepted solution (xc)", "best solution"),
                   col = c(grey(0.6), grey(0.2), "blue"),
                   lwd = 5, box.lwd = 0)
        }
    } else {
        .NotYetUsed("plot.type")
    }
    invisible()
}
