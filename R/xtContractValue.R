## -*- truncate-lines: t; fill-column: 65; comment-column: 50; -*-

xtContractValue <- function(quoted.price, coupon, do.round = TRUE) {

    do.round <- if (do.round)
                    round
                else
                    function(x, ignore)
                        x

    yield <- 100 - quoted.price
    i <- yield/200
    v <- do.round(1/(1+i), 8)
    n <- 20
    c <- coupon/2

    1000*(do.round(c*(1-v^20)/i,8) + 100*do.round(v^20,8))
}

xtTickValue <- function(quoted.price, coupon, do.round = TRUE) {
    (xtContractValue(quoted.price + 0.01, coupon, do.round = do.round) -
     xtContractValue(quoted.price - 0.01, coupon, do.round = do.round))*100/2
}
