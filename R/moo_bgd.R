#' Stochastic Gradient Descent
#'
#' This function performs parameter estimation for Linear Regression using Stochastic Gradient Descent
#' @param X the input matrix of predictors. Each column represents a predictor
#' @param Y the response vector.
#' @param alpha the learning rate - typically this would be set to the optimum value
#' @param lambda the regularization parameter - if you want to constrain your parameter estimation to avoid overfitting
#' @param max.round the maximum number of iterations - in case of delayed convergence, the function would terminate after max.round iterations
#' @export
#' @examples
#' StochasticGradientDescent(as.matrix(c(1,2,3,4,5), as.matrix(c(1,2,3,4,5), alpha=0.01)

StochasticGradientDescent <- function(X, Y, alpha = 1, lambda = 0, max.round = 1000, precision = 0.0001){
  alpha <- alpha / length(Y)
  X     <- cbind(1, X)
  B     <- rep(0, ncol(X))
  
  for(round in 1:max.round){
    B.prev <- B
    
    for(i in 1:nrow(X)){
      x <- X[i,, drop=FALSE]
      y <- Y[i]
      
      y.hat <- x %*% B
      B <- B + alpha * (t(x) %*% (y - y.hat) - lambda * B)
    }
    
    if(any(is.na(B)) ||
       !any(abs(B.prev - B) > precision * B)){
      break
    }
  }
  
  return(B)
}
