StochasticGradientDescent <- function(X, Y, alpha = 1, max.iter = 1000, precision = 0.0001, seed=1){
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

  # Shuffle data
  set.seed(seed)
  X <- X[sample(nrow(X)), ]
  set.seed(NULL)
  # Initial value of coefficients
  B <- rep(0, ncol(X))
  # Recorded for AdaGrad
  G <- matrix(rep(0,ncol(X)), ncol=1)
  # Recorded for loss vs iteration
  loss_iter <- data.frame(
    loss = numeric(),
    iter = integer()
  )
  for(iter in 1:max.iter){
    B.prev <- B
    
    for(i in 1:nrow(X)){
      x <- X[i,, drop=FALSE]
      y <- Y[i]
      yhat <- x %*% B

      # AdaGrad
      g <- (t(x) %*% (y-yhat)) ^ 2
      G <- G + g

      # Use AdaGrad to update coefficients
      B <- B + 1/(sqrt(G + 1e-8)) * alpha * (t(x) %*% (y - yhat))
    }
    
    # Record loss vs iteration
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
