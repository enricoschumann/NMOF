bundFuture <- function(clean, coupon, trade.date,
                       expiry.date, last.coupon.date, r, cf) {

    td <- as.numeric(as.Date(trade.date))
    xd <- as.numeric(as.Date(expiry.date))
    lcd <- as.numeric(as.Date(last.coupon.date))
    actual <- 365

    days_to_expiry <- xd - td
    ans  <- clean +
        (clean + coupon*(td - lcd)/actual) * r * days_to_expiry/360 -
        coupon*days_to_expiry/actual
    ans/cf
}

bundFutureImpliedRate <- function(future, clean, coupon, trade.date,
                                expiry.date, last.coupon.date, cf) {

    td <- as.numeric(as.Date(trade.date))
    xd <- as.numeric(as.Date(expiry.date))
    lcd <- as.numeric(as.Date(last.coupon.date))
    actual <- 365

    days_to_expiry <- xd - td
    (future*cf - clean  + coupon*days_to_expiry/actual) /
        (clean + coupon*(td - lcd)/actual) / (days_to_expiry/360 )
}
