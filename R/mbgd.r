MiniBatchGradientDescent <- function(X, Y, alpha=0.1, max.iter=10, precision=0.0001, batchRate=0.5, seed=1){
  #shuffle data
  set.seed(seed)
  X <- X[sample(nrow(X)), ]
  set.seed(NULL)
  #initialize theta
  B <- rep(0, ncol(X))
  #constant variables
  rowLength <- nrow(X)
  batchSize <- ceiling(rowLength * batchRate)
  #loop the gradient descent
  for(iteration in 1:max.iter){
    B.prev <- B
    for(i in seq(1, rowLength, batchSize)){
      indexes <- i:min((i+batchSize), rowLength)
      Xtemp <- X[indexes,,drop=FALSE]
      Ytemp <- Y[indexes,drop=FALSE]
      
      yhat <- Xtemp %*% B
      B <- B + 2 * (alpha / length(indexes)) * t(Xtemp) %*% (Ytemp - yhat)
    }
    
    if(any(is.na(B)) ||
       !any(abs(B.prev - B) > precision * B)){
      break
    }
    
    X <- X[sample(nrow(X)), ]
  }
  
  fv <- X %*% B
  rs <- Y - fv
  coef <- as.vector(B)
  names(coef) <- colnames(X)
  
  z <- structure(list(
    x=X,
    y=Y,
    coefficients = coef,
    fitted.values = fv,
    residuals = rs
  ),
  class = "rlm")
  
  z
}