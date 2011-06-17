NSS <- function(param, tm) {
    # param = c(beta1, beta2, beta3, beta4, lambda1, lambda2)
    gam1 <- tm / param[5]
    gam2 <- tm / param[6]
    aux1 <- 1 - exp(-gam1)
    aux2 <- 1 - exp(-gam2)
    y <- param[1] + param[2] * (aux1 / gam1) + 
         param[3] * (aux1 / gam1 + aux1 - 1) + 
         param[4] * (aux2 / gam2 + aux2 - 1) 
    y
}

