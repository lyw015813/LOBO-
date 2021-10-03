#' A function to perform model training

train.fun <- function(x, y, ncomp ){
  #' library(pls)
  x = as.matrix(x)
  y = as.numeric(y)
  n <- dim(x)[1]
  p <- dim(x)[2]
  
  fit.fun <- simpls.fit(x, y, ncomp, stripped = FALSE) 
  beta.mat = matrix(0,nrow = p,ncol = ncomp) 
  for(i in 1:ncomp){
     beta.mat[,i] = abs(fit.fun$coefficients[,,i]) 
  }
  
  return(list(beta.mat = beta.mat))
  
}

