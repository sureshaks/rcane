#' rgasm
#'
#' \code{rgasm} is an interface for the optimization functions written in the rcane project.
#' 
#' @param formula an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted. The details of model specification are given under ‘Details’.
#' @param data an optional data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model. If not found in data, the variables are taken from environment(formula), typically the environment from which lm is called.
#' @param method the method to be used. Possible values include "bgd", "sgd", "cgd" and "mini-bgd".
#' @param ... additional arguments to be passed to the low level regression fitting functions.
#' 
#' @examples
#' library(datasets)
#' rgasm(mpg ~ disp, data = mtcars, alpha = 0.00001)
#' 
#' @export

rgasm <- function (formula, data, method = "sgd", ...){
  cl <- match.call()
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
  
  z$call <- cl
  z
}


#' @describeIn rgasm Print method for rgasm
#' 
#' @param object The object to be printed.
#'  
#' @method print rgasm
#' @export

print.rgasm <- function(object, ...){
  cat("Call:\n")
  print(object$call)
  cat("\nCoefficients\n")
  print(object$coefficients)
}