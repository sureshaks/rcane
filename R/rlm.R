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
#'  the function would terminate after max.iter iterations
#' @export
rlm <- function (formula, data, method = "batch", ...) {
  if ( as.character(formula[[1]]) != "~" )
  	stop("invalid formula")
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")
  y <- model.response(mf, "numeric")
  x <- model.matrix(mt, mf)
  if(method == "batch") {
    BatchGradientDescent(x, y, ...)
  }
}