#' Stochastic Gradient Descent
#'
#' This function performs parameter estimation for Linear Regression using Stochastic Gradient Descent
#' @param X the input matrix of predictors. Each column represents a predictor
#' @param Y the response vector.
#' @param alpha the learning rate - typically this would be set to the optimum value
#' @param lambda the regularization parameter - if you want to constrain your parameter estimation to avoid overfitting
#' @param max.iter the maximum number of iterations - in case of delayed convergence, the function would terminate after max.iter iterations
#' @param precision the minimum difference of Betas between each iterations. If no difference is more than precision, stop the iteration.
#' @export
#' @examples
#' StochasticGradientDescent(as.matrix(c(1,2,3,4,5), as.matrix(c(1,2,3,4,5), alpha=0.01)

StochasticGradientDescent <- function(X, Y, alpha = 1, lambda = 0, max.iter = 1000, precision = 0.0001){
  # Adding intercept as 1
  X     <- cbind(1, X)
  # Initial value of coefficients
  B     <- rep(0, ncol(X))
  
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
  
  return(B)
}
