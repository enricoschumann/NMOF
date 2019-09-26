Y <- rnorm(20)
Z <- rnorm(20)
cor(Y, Z, method = "spearman")

ranksY <- rank(Y)
ranksZ <- rank(Z)
cor(ranksY, ranksZ, method = "pearson")
