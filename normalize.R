normalize <- function(x,y,normalize = TRUE, intercept = TRUE,  eps = .Machine$double.eps){
call <- match.call()
nm <- dim(x)
n <- nm[1]
m <- nm[2]
im <- seq(m)
one <- rep(1, n)
vn <- dimnames(x)[[2]]
meanx <- drop(one %*% x)/n
if (intercept == FALSE) {
  meanx <- rep(0, m)
}
x <- scale(x, meanx, FALSE)
normx <- sqrt(drop(one %*% (x^2)))
if (normalize == FALSE) {
  normx <- rep(1, m)
}
if (any(normx < eps * sqrt(n))) 
  stop("Some of the columns of x have zero variance")
names(normx) <- NULL
x <- scale(x, FALSE, normx)
mu <- mean(y)
if (intercept == FALSE) {
  mu <- 0
}
y <- drop(y - mu)

obj =list(X=x,y=y)
return(obj) 
}