NS <- function(param, tm) {
    # param == c(beta1, beta2, beta3, lambda)
    aux <- tm / param[4L]
    y <- param[1L] + param[2L] * ( (1 - exp(-aux)) / aux ) + 
         param[3L] * ( (1 - exp(-aux)) / aux - exp(-aux) )
    y
}

