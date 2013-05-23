require("compiler")
n <- 1e6
f1 <- function(n) {
    for (i in seq_len(n))
        NULL
}
f2 <- function(n) {
    for (i in seq_len(n))
        if (4 > 3)
            NULL else NULL
}
f1c <- cmpfun(f1)
f2c <- cmpfun(f2)


system.time(f1(n))
system.time(f2(n))
system.time(f1c(n))
system.time(f2c(n))
