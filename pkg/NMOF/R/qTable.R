qTable  <- function(
    X, xmin = NULL, xmax = NULL, labels = NULL,
    at = NULL, unitlength = "1cm", linethickness = NULL,
    cnames = colnames(X), circlesize = 0.01,
    xoffset = 0, yoffset = 0, dec = 2, filename = NULL) {
    
    X <- as.matrix(X)
    if (nrow(X) < 10L) 
        warning("'X' has less than 10 rows")
    if (!is.null(at) && is.null(labels))
        stop("'labels' must be provided")
    if (!is.null(labels) && is.null(at))
        stop("'at' must be provided")
    if (!is.null(at) && is.null(xmax))
        stop("'xmax' must be provided")
    if (!is.null(at) && is.null(xmin))
        stop("'xmin' must be provided")
    
    # compute quantiles
    A <- apply(X, 2, quantile, c(.25, .5, .75))
    iqr <- abs(A[3L, ] - A[1L, ])
    
    # compute whiskers
    B          <- array(0, dim = c(5L, dim(A)[2L]))
    B[1L, ]    <- pmax(A[1L,] - 1.5 * iqr, apply(X, 2, min))
    B[2L:4L, ] <- A
    B[5L, ]    <- pmin(A[3L,] + 1.5 * iqr, apply(X, 2, max))
    
    
    # ranges of plot
    if (is.null(xmin)) xmin <- min(B)
    if (is.null(xmax)) xmax <- max(B)
    joli <- pretty(c(xmin,xmax), n = 3)
    a <- min(joli)
    b <- max(joli)
    if (is.null(labels)) labels <- joli
    if (is.null(at)) at <- joli
    
    # ranges of picture (map to [0,1])
    B <- (B - a) / (b - a)
    at <- (at - a) / (b - a)
    
    # functions 
    fff <- function(x) formatC(x, digits = dec, format = "f")
    ff <- function(x)  formatC(x, digits = 5, format = "f")
    `%p1%` <- function(a, b) paste(a, b, sep = "")
    `%p2%` <- function(a, b) paste(a, " & ", b, sep = "")
    # create table
    STR <- NULL
    for (cc in 1L:dim(X)[2L]){
        str0   <- cnames[cc] %p2% fff(median(X[,cc])) %p2% fff(min(X[,cc])) %p2% fff(max(X[,cc]))
        str1   <- "& \\begin{picture}(1,0)(" %p1% xoffset %p1% "," %p1% yoffset %p1% ")"
        str2   <- "\\put(" %p1% ff(B[1,cc]) %p1% ",0){\\line(1,0){" %p1% ff(B[2,cc] - B[1,cc]) %p1% "}}"
        str3   <- "\\put(" %p1%  ff(B[3,cc]) %p1% ",0){\\circle*{" %p1% circlesize %p1% "}}"
        str4   <- "\\put(" %p1% ff(B[4,cc]) %p1% ",0){\\line(1,0){" %p1% ff(B[5,cc] - B[4,cc]) %p1% "}}"
        str5   <- "\\end{picture}\\\\"
        STRaux <- paste(str0, str1, str2, str3, str4, str5, sep = "")
        STR    <- rbind(STR,"\n",STRaux)
    }
    # put tickmarks
    nTks <- length(labels)
    tkD <- 1 / (nTks - 1)
    strScale <- "&&&&\\begin{picture}(1,0)\\put(0,0){\\line(1,0){1}}"
    strScale <- paste(strScale,"\\multiput(0,0)(",ff(tkD),",0){",nTks,"}{\\line(0,-1){0.01}}",sep="")
    strScale <- paste(strScale,"\\put(-0.01,-0.1){",labels[1L],"}{\\line(0,-1){0.01}}",sep="")
    strScale <- paste(strScale,"\\put(0.99,-0.1){",labels[nTks],"}{\\line(0,-1){0.01}}",sep="")
    strScale <- paste(strScale,"\\end{picture}\\\\",sep="")
    STR <- rbind("\\begin{tabular}{rrrrrr}",STR,"\n",strScale,"\n\\end{tabular}")
    if(!is.null(linethickness)) STR <- rbind("\\linethickness{" %p1% linethickness  %p1% "}\n", STR)
    STR <- rbind("\\setlength{\\unitlength}{",unitlength,"}\n", STR)
    if(is.null(filename)) {
        cat(STR, "\n", sep = "")
    }else {
        cat(STR,"\n",sep = "", file = filename)
    }
    STR
}
