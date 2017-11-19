#' Stochastic Gradient Descent
#'
#' This function performs parameter estimation for Linear Regression using Stochastic Gradient Descent
#' @param X the input matrix of predictors. Each column represents a predictor
#' @param Y the response vector.
#' @param alpha the learning rate - typically this would be set to the optimum value
#' @param lambda the regularization parameter - if you want to constrain your parameter estimation to avoid overfitting
#' @param max.iter the maximum number of iterations - in case of delayed convergence, the function would terminate after max.iter iterations
#' @param precision the minimum difference of Betas between each iterations. If no difference is more than precision, stop the iteration.
#' 
#' @examples
#' StochasticGradientDescent(as.matrix(c(1,2,3,4,5), as.matrix(c(1,2,3,4,5), alpha=0.01)
#' 
#' @export

StochasticGradientDescent <- function(X, Y, alpha = 1, lambda = 0, max.iter = 1000, precision = 0.0001){
  if (is.null(n <- nrow(X))){
    stop("'X' must be a matrix")
  }
  if(n == 0L){
    stop("0 (non-NA) cases")
  }
  
  p <- ncol(X)
  if(p == 0L){
    return(list(
      x = X,
      y = Y,
      coefficients = numeric(),
      residuals = y,
      fitted.values = 0 * y
    ))
  }
  
  if(NROW(Y) != n){
    stop("incompatible dimensions")
  }
  # Initial value of coefficients
  B     <- rep(0, ncol(X))
  names(B) <- colnames(X)
  
  # For each iteration
  for(iter in 1:max.iter){
    # Record previous Beta for calculating difference between each iterations
    B.prev <- B
    
    # For each observations
    for(i in 1:nrow(X)){
      x <- X[i,, drop=FALSE]
      y <- Y[i]
      
      y.hat <- x %*% B
      B <- B + alpha * (t(x) %*% (y - y.hat) - lambda * B)
    }
    
    # Check for conditions to stop iteration
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
    class = "rgasm")
  
  z
}