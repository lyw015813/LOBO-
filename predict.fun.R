#' A function to perform prediction for the responses at new feature values.
#' beta.mat is produced by train.fun
#' newx: Matrix of features, each row being a point at which we want to form a prediction interval, 
#' 


predict.fun <- function(beta.mat,newx){
    p = dim(newx)[2]
    newx = matrix(newx,ncol=p)
    predict.val = newx %*% beta.mat
  
  return(predict.val = predict.val)
  
}