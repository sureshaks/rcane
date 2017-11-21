StochasticGradientDescent <- function(X, Y, alpha = 1, max.iter = 1000, precision = 0.0001){
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
  B     <- rep(0, ncol(X))
  names(B) <- colnames(X)
  G <- matrix(rep(0,ncol(X)), ncol=1)
  for(iter in 1:max.iter){
    B.prev <- B
    
    for(i in 1:nrow(X)){
      x <- X[i,, drop=FALSE]
      y <- Y[i]
      g <- (t(x) %*% (y - y.hat)) ^ 2
      G <- G + g
      y.hat <- x %*% B
      B <- B + 1/(sqrt(G + 1e-8)) * alpha * (t(x) %*% (y - y.hat))
    }
    
    if(any(is.na(B)) ||
       !any(abs(B.prev - B) > precision * B)){
      break
    }
  }
  
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
