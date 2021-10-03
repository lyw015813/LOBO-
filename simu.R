
simu <- function(n,p,rho){
                      
    sigma = matrix(0, p, p)
    corvec = function(i, p, rho) rho^(abs(i - 1L:p))
    for (i in 1:p) sigma[i, ] = corvec(i, p, rho)
    X = mvtnorm::rmvnorm(n, rep(0, p), sigma)
    beta0 = runif(p)
    beta = matrix(sqrt(5)*beta0,nrow = p)
    eps = matrix(rnorm(n, 0, 2))
    y = as.matrix((X %*% beta) + eps)
    
    return(list("X" = X, "y"=y))
    
}