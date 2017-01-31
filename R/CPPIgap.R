## -*- truncate-lines: t; -*-

CPPI <- function(S, multiplier, floor, r, tau = 1, gap = 1) {

    T <- length(S) - 1  ## number of periods: 1, ..., T
   dt <- tau/T          ## timestep
    m <- multiplier
    B <-                ## bond investment
    C <-                ## cushion
    V <-                ## value
    E <-                ## exposure
    N <- numeric(T + 1) ## units of risky asset

    F <- floor*exp(r*-tau*(1 - 0:T/T))
    zero <- 1
    V[zero] <- 1
    C[zero] <- V[zero] - F[zero]
    E[zero] <- m * C[zero]
    N[zero] <- E[zero]/S[zero]
    B[zero] <- V[zero] - E[zero]
    for (t in 1:T + 1) {

        B[t] <- B[t - 1] * exp(r*dt)
        V[t] <- N[t - 1] * S[t] + B[t]
        C[t] <- max(0, V[t] - F[t])

        if (t %% gap == 0L) {
            E[t] <- min(m * C[t], V[t])
            N[t] <- E[t]/S[t]
            B[t] <- V[t] - E[t]
        } else {
            E[t] <- V[t] - B[t]
            N[t] <- N[t - 1]
        }
    }
    list(V = V, C = C, B = B, F = F, E = E, N = N, S = S)
}
