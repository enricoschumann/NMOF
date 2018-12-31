## -*- truncate-lines: t; -*-

drawdown <- function(v, relative = TRUE, summary = TRUE) {
    cv <- cummax(v)
    d <- cv - v
    if (relative)
        d <- d/cv
    if (summary) {
        troughTime <- which.max(d)
        peakTime <- which.max(v[seq_len(troughTime)])
        list(maximum       = max(d),
             high          = v[peakTime],
             high.position = peakTime,
             low           = v[troughTime],
             low.position  = troughTime)
    } else
        d
}
