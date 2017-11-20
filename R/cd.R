CoordinateDescent <- function(X, Y, max.iter = 1000, precision = 0.0001) {
  if (is.null(n <- nrow(X))) stop("'X' must be a matrix")
  
  if(n == 0L) stop("0 (non-NA) cases")
  
  p <- ncol(X)
  
  if(p == 0L) {
    return(list(
      x  <-  X,
      y  <-  Y,
      coefficients  <-  numeric(),
      residuals  <-  Y,
      fitted.values  <-  0 * Y
    ))
  }
  
  if(NROW(Y) !=  n) {
    stop("incompatible dimensions")
  }
  
  B <- rep(0, ncol(X))
  names(B) <- colnames(X)
  
  for(i in 1:max.iter) {
    B.prev <- B
    for(j in (1:length(B))) {
      hx <- (X[,-j,drop=FALSE] %*% as.matrix(B[-j]))
      derrivative <- (Y-hx)
      derrivative <- ifelse(derrivative<0.001,0,derrivative)
    
      B[j] <- (1/norm(as.matrix(X[,j]),"F")^2)*(t(derrivative)%*%X[,j])
      
      j <- j+1
    }

  if(any(is.na(B)) ||
     !any(abs(B.prev - B) > precision * B)){
    break
  }

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
