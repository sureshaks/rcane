MiniBatchGradientDescent <- function(X, Y, alpha=1, max.iter=1000, precision=0.0001, batchRate=0.5, seed=1){
  if (is.null(n <- nrow(X))) stop("'X' must be a matrix")
  
  if(n == 0L) stop("0 (non-NA) cases")
  
  p <- ncol(X)
  
  if(p == 0L) {
    return(list(
      x = X,
      y = Y,
      coefficients = numeric(),
      residuals = Y,
      fitted.values = 0 * Y
    ))
  }
  
  if(NROW(Y) != n) {
    stop("incompatible dimensions")
  }

  # Initial value of coefficients
  B <- rep(0, ncol(X))
  # batch size
  batchSize <- ceiling(n * batchRate)
  # Recorded for loss vs iteration
  loss_iter <- data.frame(
    loss = numeric(),
    iter = integer()
  )
  for(iter in 1:max.iter){
    B.prev <- B

    for(i in seq(1, n, batchSize)){
      indexes <- i:min((i+batchSize), n)
      Xtemp <- X[indexes,,drop=FALSE]
      Ytemp <- Y[indexes,drop=FALSE]
      
      yhat <- Xtemp %*% B
      B <- B + (alpha / length(indexes)) * t(Xtemp) %*% (Ytemp - yhat)
    }
    
    loss <- Y - X %*% B
    loss_iter <- rbind(loss_iter, c(sqrt(mean(loss^2)), iter))
    
    if(any(is.na(B)) ||
       !any(abs(B.prev - B) > precision * B)){
      break
    }
  }
  
  names(B) <- colnames(X)
  fv <- X %*% B
  rs <- Y - fv
  coef <- as.vector(B)
  names(coef) <- rownames(B)
  colnames(loss_iter) <- c('loss', 'iter')
  
  z <- structure(list(
    x=X,
    y=Y,
    coefficients = coef,
    fitted.values = fv,
    residuals = rs,
    loss_iter = loss_iter
  ),
  class = "rlm")
  
  z
}