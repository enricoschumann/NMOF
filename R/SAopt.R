SAopt <- function(OF, algo = list(), ...) {
    algoD <- list(nD = 2000L, ## random steps for computing acc. prob.
                  nT = 10L,   ## number of temperatures
                  nS = 1000L, ## steps per temperatures
                  nI = NULL,
                  initT = NULL,    ## starting temperature
                  finalT = 0,      ## final temperature
                  initProb = 0.4,  ## initial acceptance probability
                  alpha = 0.9,     ## temperature-reduction rate
                  mStep = 1,       ## step multiplier
                  x0 = NULL,       ## initial solution
                  neighbour = NULL,
                  printDetail = TRUE,
                  printBar = interactive(),
                  storeF = TRUE,
                  storeSolutions = FALSE,
                  classify = FALSE,
                  OF.target = NULL)

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

    x0 <- if (is.function(algoD$x0))
              algoD$x0()
          else
              eval(algoD$x0)

    OF1 <- function(x)
        OF(x, ...)
    N1 <- function(x)
        algoD$neighbour(x, ...)

    target.reached <- FALSE
    T <- algoD$initT
    alpha <- algoD$alpha

    printDetail <- algoD$printDetail
    printBar <- algoD$printBar
    if (printBar && printDetail > 1)
        printBar <- FALSE
    if (printDetail)
        cat("\nSimulated Annealing.\n")


    nT <- makeInteger(algoD$nT, "algo$nT")
    nS <- makeInteger(algoD$nS, "algo$nS")
    nD <- makeInteger(algoD$nD, "algo$nD")
    niter <- nS * nT

    if (!is.null(algoD$initT)) {
        T <- algoD$initT
    } else {
        if (printDetail) {
            cat("\nCalibrating acceptance criterion ... ")
            flush.console()
            gc(FALSE)
            startTime <- proc.time()
        }
        if (printBar)
            whatGen <- txtProgressBar(min = 1,
                                      max = nD,
                                      style = 3,
                                      getOption("width")*0.8)
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
        if (any(is.na(diffF)))
            stop("objective function evaluated to NA")

        T <- try(uniroot(function(T)
                         algoD$initProb - sum(exp(-diffF/T))/nD,
                         interval = c(0.00001, 2))$root, silent = TRUE)
        if (inherits(T, "try-error"))
            T <- -mean(diffF)/log(algoD$initProb)
        if (printBar)
            close(whatGen)

        if (printDetail) {
            cat("OK")
            endTime <- proc.time()
            cat("\nEstimated remaining running time:",
                as.numeric(endTime[[3L]] - startTime[[3L]]) /
                nD * niter ,
                "secs.\n")
            flush.console()
        }
    }

    ## evaluate initial solution
    xc <- x0
    xcF <- OF1(xc)
    xbest <- xc
    xbestF <- xcF

    if (algoD$storeF) {
        Fmat <- array(NA, dim = c(niter, 2L))
        colnames(Fmat) <- c("xnF", "xcF")
    } else
        Fmat <- NA

    xlist <- if (algoD$storeSolutions)
                 list(xn = vector("list", length = niter),
                      xc = vector("list", length = niter))
             else
                 NA

    if (printDetail) {
        cat("\nRunning Simulated Annealing ...\n")
        cat("Initial solution: ",
            prettyNum(xbestF, drop0trailing = TRUE),
            "\n")
        flush.console()
    }

    if (printBar)
        whatGen <- txtProgressBar(min = 1,
                                  max = niter,
                                  style = 3,
                                  getOption("width")*0.8)

    ## counter = total number of iterations
    counter <- 0L
    for (t in seq_len(nT)) {
        for (s in seq_len(nS)) {
            counter <- counter + 1L

            xn <- N1(xc)
            xnF <- OF1(xn)
            if (xnF <= xcF || exp((xcF - xnF)/T) > runif(1L)) {
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
                    cat("Best solution (iteration ", counter,
                        "/", niter, "): ",
                        prettyNum(xbestF),"\n", sep = "")
                    flush.console()
                }
            }

            ## check target value
            if (!is.null(algoD$OF.target) && xbestF <= algoD$OF.target) {
                if (printDetail) {
                    cat("Target value (",
                        prettyNum(algoD$OF.target), ") ",
                        "for objective function reached: ",
                        prettyNum(xbestF, drop0trailing = TRUE),
                        "\n", sep = "")
                    flush.console()
                    target.reached  <- TRUE
                }
                break
            }
        }

        ## the second break for target.reached is
        ## required since break only exits the
        ## innermost loop
        if (target.reached || T <= algoD$finalT)
            break

        nS <- round(algoD$mStep*nS)
        T <- T*alpha
    }
    if (printDetail)
        cat("Finished.\nBest solution overall: ",
            prettyNum(xbestF, drop0trailing = TRUE),
            "\n",
            sep = "")
    if (printBar)
        close(whatGen)

    ans <- list(xbest = xbest, OFvalue = xbestF,
                Fmat = Fmat, xlist = xlist,
                initial.state = state)
    if (algoD$classify)
        class(ans) <- "SAopt"
    ans
}

SA.info <- function(n = 0L) {
    e <- parent.frame(3L + n)
    temp <- NA
    step <- NA
    iteration <- NA
    calibrating <- NA
    xbest <- NA
    if (exists("i", envir = e, inherits = FALSE))
        step <- get("i", envir = e, inherits = FALSE)
    if (exists("s", envir = e, inherits = FALSE))
        step <- get("s", envir = e, inherits = FALSE)
    if (exists("t", envir = e, inherits = FALSE))
        temperature <- get("t", envir = e, inherits = FALSE)
    if (exists("counter", envir = e, inherits = FALSE))
        iteration <- get("counter", envir = e, inherits = FALSE)
    if (exists("xbest", envir = e, inherits = FALSE))
        xbest <- get("xbest", envir = e, inherits = FALSE)
    list(calibration = calibrating,
         iteration = iteration,
         step = step,
         temperature = temperature,
         xbest = if (is.list(xbest)) list(xbest) else xbest)
}


                                        # METHODS

print.SAopt <- function(x, ...) {
    cat("Simulated Annealing\n")
    cat("_ final objective-function value: ",
        prettyNum(x$OFvalue, drop0trailing = TRUE),
        "\n")
    invisible(x)
}

plot.SAopt <- function(x, y, plot.type = "interactive", ...) {
    if (plot.type == "interactive") {
        if (!is.null(x$Fmat)) {
            dev.new(title = "SAopt: Objective function values")
            defaults <- list(x    = x$Fmat[,1L],
                             xlab = "Iteration",
                             ylab = "Objective function value",
                             main = "SAopt: Objective function values",
                             type = "l",
                             col  = grey(0.6))
            do.call("plot", defaults, ...)

            defaults$x <- x$Fmat[,2L]
            defaults$col <- grey(0.2)
            do.call("lines", defaults, ...)

            defaults$x <- cummin(x$Fmat[,2L])
            defaults$col <- "blue"
            do.call("lines", defaults, ...)
        }
    } else {
        .NotYetUsed("plot.type")
    }
    invisible()
}
