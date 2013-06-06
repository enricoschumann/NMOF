require("NMOF")
require("rbenchmark")
require("snow")


timing <- function(nCol, nRow, nG, nC, delay) {

    on.exit(stopCluster(cl))

    ## create data
    mat <- rnorm(nRow * nCol)
    dim(mat) <- c(nRow, nCol)

    ## some objective function
    h <- 5
    fun <- function (x, h, delay) {
        Sys.sleep(delay)
        sort(x, partial = h)[h]
    }
    loopfun <- function(x, f, ...) {
        ns <- ncol(x)
        fv <- numeric(ns)
        for (i in seq_len(ns))
            fv[i] <- f(x[ ,i], ...)
        fv
    }

    ## as implemented in GAopt in version 0.22-3
    cl <- makeCluster(c(rep("localhost", nC)), type = "SOCK")
    z <- expression({
        listP <- vector(mode = "list", length = ncol(mat))
        for (s in seq_len(ncol(mat)))
            listP[[s]] <- mat[ ,s]
        result1 <- unlist(clusterApply(cl, listP, fun, h, delay))
    })
    timing1 <- benchmark(z, columns = c("elapsed"), replications = 100)
    stopCluster(cl)

    ## as implemented in GAopt in version 0.22-4
    cl <- makeCluster(c(rep("localhost", nC)), type = "SOCK")
    z <- expression({
        splt <- splitIndices(nCol, nC)
        listP <- vector(mode = "list", length = nC)
        for (s in seq_len(nC))
            listP[[s]] <- mat[ ,splt[[s]]]
        result2 <- unlist(clusterApply(cl, listP, loopfun, fun, h, delay))
    })
    timing2 <- benchmark(z, columns = c("elapsed"), replications = 100)
    stopCluster(cl)

    ## classic loop
    z <- expression({
        result0 <- numeric(ncol(mat))
        for (s in seq_len(ncol(mat)))
            result0[[s]] <- fun(mat[ ,s], h, delay)
    })
    timing0 <- benchmark(z, columns = c("elapsed"), replications = 100)

    on.exit()
    stopifnot(all.equal(result1, result2))
    stopifnot(all.equal(result0, result2))
    list(timing0 = timing0, timing1 = timing1, timing2 = timing2)
}

df <- expand.grid(nCol = c(20, 200),
                  nRow = c(10, 100),
                  nG   = c(20),
                  nC   = c(2, 4),
                  delay = c(0))
results <- array(0, dim = c(nrow(df), 3L))
for (n in seq_len(nrow(df))) {
    nCol <- df[["nCol"]][n]
    nRow <- df[["nRow"]][n]
    nG <- df[["nG"]][n]
    nC <- df[["nC"]][n]
    delay <- df[["delay"]][n]
    results[n, ] <- unlist(timing(nCol, nRow, nG, nC, delay))
}

## speedup new distribution vs loop
speedup0 <- results[ ,1L]/results[ ,3L]

## speedup new distribution vs old distributions
speedup  <- results[ ,2L]/results[ ,3L]
allres <- cbind(df, data.frame(results),
                data.frame(speedup0 = speedup0, speedup  = speedup))

require(lattice)
dotplot(speedup~factor(nCol) | factor(nC), data = allres,
        layout = c(3,1))
dotplot(speedup~factor(nRow) | factor(nC), data = allres,
        layout = c(3,1))
llines(x=c(0,200),y=1)
