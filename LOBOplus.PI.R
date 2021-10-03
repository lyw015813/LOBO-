#'  LOBO+ prediction intervals.
#'  Compute prediction intervals via LOBO+ method.
#'  source('LOBOplus.PI.R',encoding='utf-8') 
#'  LOBOplus.PI = LOBOplus.PI(x,y,newx,ncomp = 15,alpha = 0.1, train.fun = train.fun,predict.fun = predict.fun,ratio=0.9)
LOBOplus.PI <- function(x, y, newx,
                     ncomp, 
                     train.fun = train.fun, 
                     predict.fun = predict.fun, 
                     alpha,
                     reptimes = 500L,
                     ratio,
                     replace = FALSE){
  
  if (missing(x) | missing(y) | missing(newx)) stop("Please specify both x , y and newx")
  
  # Set up data
  x = as.matrix(x)
  y = as.numeric(y)
  n = nrow(x)
  p = ncol(x)
  x0 = matrix(newx,ncol=p)
  n0 = nrow(x0)
  
  fit.mat = matrix(NA,n,ncomp)
  pre.mat = matrix(NA,n,ncomp)
  samp.idx = vector("list", reptimes)
  m = floor(n * ratio)
  m1 = n - m

  for (i in 1L:reptimes) {
    samp.idx[[i]] = sample(1L:n, m, replace = replace)
  }
  
  modellist = vector("list", reptimes)
  reslist = vector("list",reptimes) 
  N = reptimes * m1   
  res.mat  = matrix(NA,N,ncomp) 
  model.mean = matrix(0,p,ncomp)

  for (i in 1L:reptimes) {
    xx <- x[samp.idx[[i]], ] 
    yy <- y[samp.idx[[i]]]
    modellist[[i]] <- train.fun(xx,yy,ncomp)$beta.mat
    xx.test <- x[-samp.idx[[i]], ]
    yy.test <- y[-samp.idx[[i]]]
    reslist[[i]] <- abs(yy.test - predict.fun(modellist[[i]],xx.test))
    res.mat[((i-1)*m1+1) : (i*m1),] <- reslist[[i]]
    model.mean <- model.mean + train.fun(xx,yy,ncomp)$beta.mat
 }
   
  model.mean <- model.mean / reptimes
  pre.value <- predict.fun(model.mean,x0)
  res.up =  matrix(0,n0,ncomp)
  
   for(l in 1L:ncomp){

     res.up[,l] =  sort(res.mat[,l])[floor((1-alpha)*(N+1))]

  }
  
  lo = up = matrix(0,n0,ncomp)
  
  lo <- matrix(pre.value - res.up, n0, ncomp) 
  up <- matrix(pre.value + res.up, n0, ncomp) 


  return(list( LOBOplus.lo = lo,
               LOBOplus.up = up
             ))
  
}

