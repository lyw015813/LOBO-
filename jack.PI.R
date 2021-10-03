#'  Jackknife and jacknife+ prediction intervals.
#'  Compute prediction intervals via jackknife or jackknife+ method.
#'  source('jack.PI.R',encoding='utf-8' )
#'  JackPI = jack.PI(x,y,newx,ncomp = 10,train.fun = train.fun,predict.fun = predict.fun,alpha = 0.1) 
jack.PI <- function(x, y, newx, ncomp, train.fun, predict.fun, alpha){
  
  if (missing(x) | missing(y) | missing(newx)) stop("Please specify both x and y and newx")
  #'  library(pls)
  # Set up data
  x = as.matrix(x)
  y = as.numeric(y)
  n = nrow(x)
  p = ncol(x)
  x0 = matrix(newx,ncol=p)
  n0 = nrow(x0)
  
  
  fit.mat = matrix(NA,n,ncomp)
  jack.plus.pre = matrix(NA,n,ncomp)
  res.mat =  matrix(NA,n,ncomp) 

  for (i in 1:n){
      xx = x[-i,]; yy = y[-i]
      out.i = train.fun(xx,yy,ncomp)
      
      fit.mat[i,] = matrix(predict.fun(out.i$beta.mat,x[i,,drop=F]),nrow=1)
      res.mat[i,] = abs(y[i] - fit.mat[i,,drop = F])# R_i^(Loo)

      jack.plus.pre[i,] = matrix(predict.fun(out.i$beta.mat,x0),nrow=n0)   
  }

     jack.plus.mat.l = jack.plus.pre - res.mat 
     jack.plus.mat.U = jack.plus.pre + res.mat 
     jack.plus.lo = jack.plus.up = matrix(0,n0,ncomp)

     N = dim(jack.plus.mat.l)[1]
     out = train.fun(x,y,ncomp)
     jack.pre = predict.fun(out.i$beta.mat,x0)

     jack.min = matrix(NA,n,ncomp)
     jack.plus = matrix(NA,n,ncomp)
     jack.lo = jack.up = matrix(0,n0,ncomp)

     for(j in 1:n){
       jack.min[j,] = jack.pre - res.mat[j,]
       jack.plus[j,] = jack.pre + res.mat[j,]
     }
     
     for(l in 1:ncomp){
       
         jack.plus.lo[,l] = sort(jack.plus.mat.l[,l])[floor(alpha*(N+1))]
         jack.plus.up[,l] = sort(jack.plus.mat.U[,l])[floor((1-alpha)*(N+1))]
         
         jack.lo[,l] = sort(jack.min[,l])[floor(alpha*(N+1))]
         jack.up[,l] = sort(jack.plus[,l])[floor((1-alpha)*(N+1))]
         
     }
  

return(list( jack.plus.lo = jack.plus.lo,
             jack.plus.up = jack.plus.up,
             jack.lo =  jack.lo,
             jack.up =  jack.up))
}



