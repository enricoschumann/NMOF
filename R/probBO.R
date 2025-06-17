PBO <- function(M,
                s = 12,
                fun = colMeans,
                threshold = 0, ...,
                loop.fun = TRUE) {

    s <- round(s)
    if (s %% 2) {
        s <- s + 1
        warning(sQuote("s"), " must be even and is set to ", s)
    }

    T <- nrow(M)
    N <- ncol(M)
    N1 <- N + 1

    s1 <- seq_len(s)
    C <- combn(s, s/2, simplify = FALSE)
    starts <- seq(1, T, by = round(T/s))[s1]
    ends <- c(starts[-1] - 1, T)
    ind <- apply(cbind(starts, ends), 1,
                 function(x) seq.int(x[1L], x[2L]), simplify = FALSE)


    oos <- is <- Ls <- numeric(length(C))

    for (j in seq_along(C)) {

        x  <- C[[j]]
        x_ <- s1[-x]

        J  <- unlist(ind[x ])
        J_ <- unlist(ind[x_])

        Mi <- M[J,  ]
        Mo <- M[J_, ]

        if (loop.fun) {
            fo <- fi <- numeric(N)
            for (jj in seq_len(N)) {
                fi[jj] <- fun(Mi[, jj, drop = FALSE], ...)
                fo[jj] <- fun(Mo[, jj, drop = FALSE], ...)
            }
        } else {
            fi <- fun(Mi, ...)
            fo <- fun(Mo, ...)
        }


        n <- which.max(fi)[1]
        om <- rank(fo)[n] / N1
        Ls [j] <- om
        is [j] <- fi[n]
        oos[j] <- fo[n]

    }
    Ls <- log( Ls /(1 - Ls) )
    list(pbo = sum(Ls <= 0)/length(C),
         lambda = Ls,
         in.sample = is,
         out.of.sample = oos
         )
}
