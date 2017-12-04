CoordinateDescent <- function(X, Y, max.iter = 10000, precision = 0.0001) {
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
  
  # Initial value of coefficients
  B <- rnorm(ncol(x), 0, 1)
  # Recorded for loss vs iteration
  loss_iter <- data.frame(
    loss = numeric(),
    iter = integer()
  )
  
  for(iter in 1:max.iter) {
    B.prev <- B

    for(j in 1:length(B)) {
      hx <- (X[, -j, drop=FALSE] %*% as.matrix(B[-j]))
      derrivative <- (Y-hx)
    
      B[j] <- (1/norm(as.matrix(X[,j]), "F")^2) * (t(derrivative) %*% X[,j])
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
  names(coef) <- names(B)
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
