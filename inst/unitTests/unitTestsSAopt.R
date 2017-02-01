test.SAopt <- function() {

    ## SA should come close to the minimum
    size <- 5L
    x0 <- runif(size)
    xTRUE <- runif(size)
    data <- list(xTRUE = xTRUE,
                 step = 0.02)
    OF <- function(x, data)
        max(abs(x - data$xTRUE))
    neighbour <- function(x, data)
        x + runif(length(data$xTRUE))*data$step - data$step/2

    algo <- list(nS = 1000L, nT = 10L,
                 neighbour = neighbour, x0 = x0,
                 printBar = FALSE,
                 printDetail = FALSE,
                 storeSolutions = TRUE,
                 storeF = TRUE)
    res <- SAopt(OF, algo = algo, data = data)
    checkTrue(res$OFvalue < 0.005)

    ## check 'xlist'
    checkTrue(length(res$xlist[[1L]])==length(res$xlist[[2L]]))
    checkEquals(dim(do.call(rbind, res$xlist[[1L]])),
                dim(do.call(rbind, res$xlist[[2L]])))
    checkEquals(dim(res$Fmat), c(algo$nS * algo$nT, 2L))

    ## check 'Fmat': xn and xc must not all be the same
    checkTrue(!isTRUE(all.equal(res$Fmat[ ,1L],res$Fmat[ ,2L])))

    tmp <- res$Fmat[ ,1L]==res$Fmat[ ,2L]
    checkEquals(res$Fmat[tmp ,1L],
                apply(do.call(rbind, res$xlist[[1L]])[tmp, ], 1,OF, data))

    ## check printDetail
    algo <- list(nS = 10L, nT = 5L,
                 neighbour = neighbour,
                 x0 = x0,
                 printBar = TRUE,
                 printDetail = 5)
    res <- capture.output(ignore <- SAopt(OF, algo = algo, data = data))
    checkEquals(sum(grepl("Best solution", res)), 11L)
}
