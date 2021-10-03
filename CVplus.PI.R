#'  CV+ prediction intervals.
#'  Compute prediction intervals via CV+ method.
#'  source('CVplus.PI.R',encoding='utf-8' )
#'  CVPI = CVplus.PI(x,y,newx,ncomp = 10,train.fun = train.fun,predict.fun = predict.fun,alpha = 0.1) 
CVplus.PI <- function(x, y, newx, 
                      ncomp, 
                      train.fun, 
                      predict.fun, 
                      alpha,
                      nfolds = 5L){
  
  if (missing(x) | missing(y) | missing(newx)) stop("Please specify both x and y and newx")
  #' library(pls)
  # Set up data
  
  x = as.matrix(x)
  y = as.numeric(y)
  n = nrow(x)
  p = ncol(x)
  x0 = matrix(newx,ncol=p)
  n0 = nrow(x0)
  
  index = rep_len(1L:nfolds, n)
  fit.mat = matrix(NA,n,ncomp)
  cvplus.pre = matrix(NA,n,ncomp)
  res.mat =  matrix(NA,n,ncomp) 
  
  for (i in 1L:nfolds) {
    xtrain = x[index != i, ]
    ytrain = y[index != i]
    xtest = x[index == i, ]
    ytest = y[index == i]
    out.i = train.fun(xtrain, ytrain, ncomp=ncomp)
    n.test = nrow(xtest)
    fit.mat[index == i,] = matrix(predict.fun(out.i$beta.mat,xtest),nrow = n.test)
     for(j in 1:ncomp){
       res.mat[index == i,j] = abs(as.matrix(y[index == i],ncol=1) - fit.mat[index == i,j,drop = F])
       cvplus.pre[index == i,j] = predict.fun(out.i$beta.mat[,j],x0)
     }
  }
  
  cvplus.l = cvplus.pre - res.mat 
  cvplus.U = cvplus.pre + res.mat 
  cvplus.lo = cvplus.up = matrix(0,n0,ncomp)

  N = dim(cvplus.l)[1]
  for(l in 1:ncomp){

    cvplus.lo[,l] = sort(cvplus.l[,l])[floor(alpha*(N+1))]
    cvplus.up[,l] = sort(cvplus.U[,l])[floor((1-alpha)*(N+1))]
  }
  
  res <- list(cvplus.lo = cvplus.lo,
              cvplus.up = cvplus.up
  )

}



