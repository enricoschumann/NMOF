LSopt <- function(OF, algo = list(), ...) {
    algoD <- list(nS = 1000L, nI = NULL,
                  neighbour = NULL, x0 = NULL,
                  printDetail = TRUE, printBar = interactive(),
                  storeF = TRUE, storeSolutions = FALSE,
                  OF.target = NULL,
                  classify = FALSE)

    checkList(algo, algoD)
    algoD[names(algo)] <- algo

    if (!exists(".Random.seed", envir = .GlobalEnv,
                inherits = FALSE))
        state <- NA else state <- .Random.seed

    if (is.null(algoD$neighbour))
        stop("specify a neighbourhood function")
    if (!is.function(algoD$neighbour))
        stop("'algo$neighbour' must be a function")
    if (is.null(algoD$x0))
        stop("specify start solution ", sQuote("algo$x0"))

    if (!is.null(algoD$nI))
        algoD$nS <- algoD$nI
    nS <- makeInteger(algoD$nS, "algo$nS")

    OF1 <- function(x) OF(x, ...)
    N1 <- function(x) algoD$neighbour(x, ...)

    if (is.function(algoD$x0))
        xc <- algoD$x0() else xc <- eval(algoD$x0)

    printDetail  <- algoD$printDetail
    printBar  <- algoD$printBar
    if (printBar && printDetail > 1)
        printBar <- FALSE

    if (algoD$storeF) {
        Fmat <- array(NA, dim = c(nS, 2L))
        colnames(Fmat) <- c("xnF", "xcF")
    } else Fmat <- NA
    if (algoD$storeSolutions)
        xlist <- list(xn = vector("list", length = nS),
                      xc = vector("list", length = nS)) else xlist <- NA

    xcF <- OF1(xc)
    if (printBar) {
        if (nS < 2L) {
            warning("cannot print bar with just one step")
            printBar <- FALSE
        } else {
            whatGen <- txtProgressBar(min = 1, max = nS, style = 3)
        }
    }
    if (printDetail) {
        cat("\nLocal Search.\n")
        cat("Initial solution: ", prettyNum(xcF),"\n")
        flush.console()
    }

    for (s in seq_len(nS)){
        if(printBar)
            setTxtProgressBar(whatGen, value = s)

        ## compute and evaluate neighbour
        xn <- N1(xc)
        xnF <- OF1(xn)

        ## replace solution (if better)
        if (xnF <= xcF){
            xc  <- xn
            xcF <- xnF
        }

        ## print info
        if (printDetail > 1) {
            if (s %% printDetail == 0L) {
            cat("Best solution (step ", s, "/", nS, "): ",
                prettyNum(xcF),"\n", sep = "")
            flush.console()
            }
        }
        ## store information
        if (algoD$storeF) {
            Fmat[s, 1L] <- xnF
            Fmat[s, 2L] <- xcF
        }
        if (algoD$storeSolutions) {
            xlist[[c(1L, s)]] <- xn
            xlist[[c(2L, s)]] <- xc
        }

        ## check stopif value
        if (!is.null(algoD$OF.target) && xcF <= algoD$OF.target) {
            if (printDetail) {
                cat("Target value (", prettyNum(algoD$OF.target), ") ",
                    "for objective function reached: ",
                    prettyNum(xcF), "\n", sep = "")
                flush.console()
            }
            break
        }
    }
    if (printBar)
        close(whatGen)
    if (printDetail)
        cat("Finished.\nBest solution overall: ", prettyNum(xcF), "\n",
            sep = "")

    ans <- list(xbest = xc, OFvalue = xcF, Fmat = Fmat, xlist = xlist,
                initial.state = state)
    if (algoD$classify)
        class(ans) <- "LSopt"
    ans
}

LS.info <- function(n = 0L) {
    e <- parent.frame(3L + n)
    step <- NA
    iteration <- NA
    ## if (exists("i", envir = e, inherits = FALSE))
    ##     step <- get("i", envir = e, inherits = FALSE)
    if (exists("s", envir = e, inherits = FALSE))
        iteration <- step <- get("s", envir = e, inherits = FALSE)
    ## if (exists("t", envir = e, inherits = FALSE))
    ##     threshold <- get("t", envir = e, inherits = FALSE)
    ## if (exists("counter", envir = e, inherits = FALSE))
    ##     iteration <- get("counter", envir = e, inherits = FALSE)
    list(iteration = iteration,
         step = step)
}

print.LSopt <- function(x, ...) {
    cat("Local Search\n\n")
    cat("Number of iterations: ", nrow(x$Fmat), ".    ", sep = "")
    cat("Best solution overall: ", prettyNum(x$OFvalue), "\n", sep = "")
    invisible(x)
}
