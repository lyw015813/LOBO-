
#'  Naive prediction intervals.
#'  Compute prediction intervals using naive method.
#'  source('naive.PI.R',encoding='utf-8' )
#'  NPI = naive.PI(x,y,newx,ncomp = 10,train.fun = train.fun,predict.fun = predict.fun) 
  

naive.PI <- function(x, y, newx, ncomp, train.fun, predict.fun, alpha){

  if (missing(x) | missing(y) | missing(newx)) stop("Please specify both x and y and newx")  
    #' library(pls)
    # Set up data
    x = as.matrix(x)
    y = as.numeric(y)
    n = nrow(x)
    p = ncol(x)
    x0 = matrix(newx,ncol=p)
    n0 = nrow(x0)
    ncomp = ncomp
    
    #' Train, fit, and predict on full data set
    fit.mat = matrix(NA,n,ncomp)
    pred.mat = matrix(NA,n0,ncomp)
    res.mat =  matrix(NA,n,ncomp) 
    out = train.fun(x,y,ncomp)
    lo = up = matrix(0,n0,ncomp)

    for(i in 1:ncomp){
    fit.mat[,i] = matrix(predict.fun(out$beta.mat[,i,drop=F],x),nrow=n)
    #' Get residuals and quantiles
    res.mat[,i] = abs(y- fit.mat[,i,drop = F])
    pred.mat[,i] = matrix(predict.fun(out$beta.mat[,i,drop=F],x0),nrow=n0)
    q = quantile(res.mat[,i],1-alpha)
    lo[,i] = pred.mat[,i] - q 
    up[,i] = pred.mat[,i] + q 
    }
   
    
    return(list(pred=pred.mat,lo=lo,up=up,fit=fit.mat))
    
}
   