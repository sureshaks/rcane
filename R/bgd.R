#' Batch Gradient Descent
#'
#' This function performs parameter estimation for Linear Regression using Batch Gradient Descent
#' @param X the input matrix of predictors. Each column represents a predictor
#' @param Y the response vector.
#' @param alpha the learning rate - typically this would be set to the optimum value
#' @param max.iter the maximum number of iterations - in case of delayed convergence, the function would terminate after max.iter iterations
#' @param precision the precision of the result
#' @export
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
  
  for(i in 1:nrow(X)){
    B.prev <- B
    
    yhat <- X %*% B
    B <- B + 2 * alpha * t(X) %*% (Y - yhat)

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
