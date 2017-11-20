BatchGradientDescent <- function(X, Y, alpha=0.0004, max.iter=1000, precision=0.0001) {
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
  names(B) <- colnames(X)
  
  for(iter in 1:max.iter){
    B.prev <- B
    
    yhat <- X %*% B
    B <- B + 2 * alpha * t(X) %*% (Y - yhat)

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
