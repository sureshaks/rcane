#' rgasm
#'
#' This function is a wrapper for the optimization functions written in the rcane project.
#' @param formula an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted. The details of model specification are given under ‘Details’.
#' @param data an optional data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model. If not found in data, the variables are taken from environment(formula), typically the environment from which lm is called.
#' @param method the method to be used. Possible values include "bgd", "sgd", "cgd" and "mini-bgd".
#' @param ... additional arguments to be passed to the low level regression fitting functions.
#' @export
#' @examples
#' data <- data.frame(matrix(10:18, 3))
#' formula = X1~X2+X3
#' rgasm(formula, data, alpha=0.001)

rgasm <- function (formula, data, method = "sgd", ...){
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")
  
  y <- model.response(mf, "numeric")
  x <- model.matrix(mt, mf)
  
  z = NA
  if(method=="sgd"){
    z <- (StochasticGradientDescent(x, y, ...))
  }
  z
}