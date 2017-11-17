#' Batch Gradient Descent
#'
#' This function performs heuristic parameter estimation for Linear Regression using different
#' gradient descent algorithms
#' @param formula an object of class "formula" (or one that can be coerced to that class):
#'  a symbolic description of the model to be fitted.
#' @param data the data frame containing variables in the model
#' @param method the type of gradient descent algorithm to be used
#' @param alpha the learning rate at which the parameters converge to optimal values.
#' @param max.round the maximum number of iterations - in case of delayed convergence,
#'  the function would terminate after max.round iterations
#' @export
#' @examples
#' rlm(y ~ x, data=something, method="batch")
rlm <- function (formula, data, method = "batch", ...) {
  if ( as.character(formula[[1]]) != "~" )
	  stop("invalid formula")
  mf <- model.frame(formula,data)
  mt <- attr(mf, "terms")
  y <- model.response(mf, "numeric")
  x <- model.matrix(mt, mf)
  if(method == "batch") {
    BatchGradientDescent(x, y, ...)
  }
}
