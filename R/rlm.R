#' RCANE
#' 
#' @docType package
#' @name rlm
#' 
#' @import stats
NULL

#' rlm
#'
#' \code{rlm} is an interface for the optimization functions written in the rcane project.
#' 
#' @param formula an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.
#' @param data an optional data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model. If not found in data, the variables are taken from environment(formula), typically the environment from which lm is called.
#' @param method the method to be used. Possible values include "bgd", "sgd", "cd" and "mini-bgd".
#' @param alpha the learning rate - typically this would be set to the optimum value
#' @param max.iter the maximum number of iterations - in case of delayed convergence, the function would terminate after max.iter iterations
#' @param precision the precision of the result
#' @param ... additional arguments to be passed to the low level regression fitting functions.
#' 
#' @examples
#' library(datasets)
#' rlm(mpg ~ disp, data = mtcars, alpha = 0.00001)
#' 
#' @export

rlm <- function (formula, data, method = "sgd", alpha=1, max.iter=1000, precision=0.0001, ...){
  cl <- match.call()
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
  
  z = NA
  if(method == "sgd"){
    z <- (StochasticGradientDescent(x, y, alpha, max.iter, precision, ...))
  } else if (method == "bgd"){
    z <- (BatchGradientDescent(x, y, alpha, max.iter, precision, ...))
  } else if (method == 'mini-bgd'){
    z <- (MiniBatchGradientDescent(x, y, alpha, max.iter, precision, ...))
  } else if (method == 'cd'){
    z <- (CoordinateDescent(x, y, alpha, max.iter, precision, ...))
  } else {
    stop(gettextf("%d not implemented", method))
  }
  
  class(z) <- "rlm"
  z$call <- cl
  z$method <- method
  z
}


#' @describeIn rlm Print method for rlm
#' 
#' @param x The object to be printed.
#'  
#' @method print rlm
#' @export

print.rlm <- function(x, ...){
  cat("\nCall:\n")
  print(x$call)
  cat("\nCoefficients\n")
  if(length(x$coefficients)){
    print(x$coefficients)
  } else {
    print("No coefficients\n")
  }
}