LSopt <- function(OF, algo = list(), ...) {
    if (is.null(algo$neighbour))
        stop("specify a neighbourhood function")
    if (!is.function(algo$neighbour))
        stop("'algo$neighbour' must be a function")
    if (is.null(algo$x0))
        stop("specify start solution 'algo$x0'")
    OF1 <- function(x) OF(x, ...)
    N1 <- function(x) algo$neighbour(x, ...)
    algoD = list(nS = 1000L, neighbour = NULL, x0 = NULL,
        printDetail = TRUE, printBar = TRUE)
    algoD[names(algo)] <- algo	
    algo <- algoD
    if(is.function(algo$x0)) xc <- algo$x0() else xc <- algo$x0
    printDetail  <- algo$printDetail
    printBar  <- algo$printBar
    Fmat <- array(NA, dim = c(algo$nS, 2L))
    xcF <- OF1(xc)
    if (printBar)
        whatGen <- txtProgressBar(min = 1, max = algo$nS, style = 3)
    for (s in 1L:algo$nS){	
        if(printBar) 
            setTxtProgressBar(whatGen, value = s)
        xn <- N1(xc)
        xnF <- OF1(xn)
        if (xnF <= xcF){
            xc  <- xn
            xcF <- xnF
        }
        Fmat[s, 1L] <- xnF
        Fmat[s, 2L] <- xcF
    }
    if (printBar) 
        close(whatGen)
    list(xbest = xc, OFvalue = xcF, Fmat = Fmat)
}
