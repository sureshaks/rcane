#' Batch Gradient Descent
#'
#' This function performs parameter estimation for Linear Regression using Batch Gradient Descent
#' @param x the input matrix of predictors. Each column represents a predictor
#' @param y the response vector.
#' @param alpha the learning rate - typically this would be set to the optimum value
#' @param lambda the regularization parameter - if you want to constrain your parameter estimation to avoid overfitting
#' @param max.round the maximum number of iterations - in case of delayed convergence, the function would terminate after max.round iterations
#' @export
#' @examples
#' batch(as.matrix(c(1,2,3,4,5),as.matrix(c(1,2,3,4,5),alpha=0.01)
batch <- function(x, y, alpha=0.004, lambda=0, max.round=1000, precision=0.0001) {
  x <- cbind(1, x)
  b <- rep(0, ncol(x))
  round <- 1
  first.round = TRUE
  b.prev <- rep(NA,length(b))
  
  while(round <= max.round &&
        !any(is.na(b)) &&
        (first.round || 
         any(abs(b.prev - b) > precision*b)
        )) {
    b.prev <- b
    first.round <- FALSE
    yhat <- x %*% b
    b <- b + 2 * alpha * t(x) %*% (y - yhat)
    round <- round + 1
  }
  b
}