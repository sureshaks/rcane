BatchGradientDescent <- function(X, Y, alpha=0.1, max.iter=1000, precision=0.0001) {
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
  err <- NA
  # Record loss vs iteration
  loss_iter <- data.frame(
    loss = numeric(),
    iter = integer()
  )
  for(iter in 1:max.iter){
    B.prev <- B
    err.prev <- err
    yhat <- X %*% B
    loss <- Y - yhat
    loss_iter <- rbind(loss_iter, c(sqrt(mean(loss^2)), iter))
    B <- B + 2 * alpha * t(X) %*% (loss)
    
    if(any(is.na(B)) ||
       !any(abs(B.prev - B) > precision * B)){
      break
    }
    
    err <- error(Y,yhat)
    if(!is.na(err.prev)) {
      if(err <= err.prev) {
        alpha <- alpha + alpha * 0.1
      } else {
        B <- B.prev
        alpha <- alpha - alpha * 0.5
      }
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

error <- function(actual, predicted) {
  sqrt(sum((actual-predicted)^2))
}

