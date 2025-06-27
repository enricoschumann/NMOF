MRC <-
function(w, var, R, risk = sd, Rw = NULL, ...,
             scale = TRUE,
         method = "var",
         h = 1e-8) {
    if (method == "var") {

        ans <- var %*% w
        ans <- w*ans

        if (scale)
            ans <- ans/sqrt(sum(w %*% t(w) * var))

    } else if (method == "fd") {
        if (is.null(Rw))
            Rw <- R %*% w
        dR <- R*h
        rRw <- risk(Rw)

        ans <- numeric(length(w))
        names(ans) <- names(w)
        for (i in seq_along(w)) {
            ans[i] <- w[i] * (risk(Rw + dR[, i]) - rRw) / h
        }

        if (scale)
            ans <- ans/rRw

    }
    ans
}
