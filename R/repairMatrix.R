repairMatrix <- function(C, eps = 0) {
    ## compute eigenvectors/-values
    E <- eigen(C, symmetric = TRUE)   
    V <- E$vectors
    D <- E$values
    
    ## replace *negative* eigenvalues by eps
    D <- pmax(D, eps)
    
    ## reconstruct correlation matrix
    BB <- V %*% diag(D) %*% t(V)
    
    ## rescale correlation matrix
    T <- 1/sqrt(diag(BB))
    TT <- outer(T,T)
    BB * TT
}

