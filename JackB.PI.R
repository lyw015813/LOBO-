
#'  J+aB prediction intervals.
#' Compute prediction intervals using J+aB method.
#' source('JackB.PI.R',encoding='utf-8')
#' JackBPI = JackB.PI(x,y,newx,ncomp = 10,train.fun = train.fun,predict.fun = predict.fun,alpha = 0.1) 
#' 
JackB.PI <- function(x, y, newx,
                     ncomp, 
                     train.fun = train.fun, 
                     predict.fun = predict.fun, 
                     alpha ,
                     reptimes = 500L,
                     ratio = 1
                     ){
  
    if (missing(x) | missing(y) | missing(newx)) stop("Please specify both x and y")
  
  # Set up data
  x = as.matrix(x)
  y = as.numeric(y)
  n = nrow(x)
  p = ncol(x)
  x0 = matrix(newx,ncol=p)
  n0 = nrow(x0)
  
  fit.mat = matrix(NA,n,ncomp)
  pre.mat = matrix(NA,n,ncomp)
  res.mat = matrix(NA,n,ncomp) 
  samp.idx = vector("list", reptimes)
  m = n * ratio
  
    for (i in 1L:reptimes) {
      samp.idx[[i]] = sample(1L:n, m, replace = TRUE)
    }
  
    modellist = vector("list", reptimes)
    submodellist <- vector("list",n)
    h.vector = NULL
    
    for (i in 1L:reptimes) {
      xx <- x[samp.idx[[i]], ] 
      yy <- y[samp.idx[[i]]]
      modellist[[i]] <- train.fun(xx,yy,ncomp)$beta.mat
    }
    
    for(j in 1L:n){
      h = 0
      sub.matrix <- matrix(0, nrow = p, ncol = ncomp)
      for(i in 1L:reptimes){
       if(all(samp.idx[[i]] != j)){
         sub.matrix <- sub.matrix + modellist[[i]]
         h = h+1
       }
      }
      h.vector[[j]] <- h
      submodellist[[j]] <- 1/h * sub.matrix
      fit.mat[j,] = matrix(predict.fun(submodellist[[j]],x[j,,drop=F]),nrow=1)
      res.mat[j,] = abs(y[j] - fit.mat[j,,drop = F])
      pre.mat[j,] = matrix(predict.fun(submodellist[[j]],x0),nrow=1)
    }
    
    mat.l = pre.mat - res.mat 
    mat.U = pre.mat + res.mat
    lo = up = matrix(0,n0,ncomp)
    N = dim(mat.l)[1]
    
    for(l in 1:ncomp){
     lo[,l] = sort(mat.l[,l])[floor(alpha*(N+1))]
     up[,l] = sort(mat.U[,l])[floor((1-alpha)*(N+1))]
    }
     
    return(list(jackpB.lo = lo,
                jackpB.up  = up
                ))
    
}
    
  