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
#' @param boldDriver set \code{TRUE} to use bold driver for method='bgd'
#' @param AdaGrad set \code{TRUE} to use AdaGrad for method='sgd'
#' @param ... additional arguments to be passed to the low level regression fitting functions.
#' @description
#' Gradient descent is a first-order iterative optimization algorithm for finding the minimum of a function.
#' bgd (Batch Gradient Descent) - Batch Gradient Descent updates the parameters by computing loss function of the entire dataset.
#' sgd (Stochastic Gradient Descent) - Stochastic Gradient Descent updates the parametes by computing loss function for each record in
#' the dataset.
#' cd (Coordinate Descent) - Coordinate Descent updates the parameter by minimizing the loss function along each coordinate axis.
#' mini-bgd (Mini Batch Gradient Descent) - Mini Batch Gradient Descent divides the data into batches and updates the parameters by computing the loss 
#' function for each batch.
#' @examples
#' library(datasets)
#' rlm(mpg ~ disp, data = mtcars, alpha = 0.00001)
#' 
#' @export
rlm <- function (formula, data, method = "sgd", alpha=0.1, max.iter=1000, precision=0.0001, boldDriver=FALSE, AdaGrad=FALSE, ...){
  if(!method %in% c("bgd", "sgd", "cd", "mini-bgd")) {
    stop("'method' should be one of 'bgd', 'sgd', 'cd', 'mini-bgd'")
  }
  
  if(boldDriver & method != "bgd") {
    warning("Unused argument 'boldDriver'")
  } 
  
  if(AdaGrad && method != "sgd") {
    warning("Unused argument 'AdaGrad'")
  }
  
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
    z <- (StochasticGradientDescent(x, y, alpha, max.iter, precision, AdaGrad, ...))
  } else if (method == "bgd"){
    z <- (BatchGradientDescent(x, y, alpha, max.iter, precision, boldDriver, ...))
  } else if (method == 'mini-bgd'){
    z <- (MiniBatchGradientDescent(x, y, alpha, max.iter, precision, ...))
  } else if (method == 'cd'){
    z <- (CoordinateDescent(x, y, max.iter, precision, ...))
  } else {
    stop(gettextf("%d not implemented", method))
  }
  
  class(z) <- c("rlm","rlmmodel")
  z$call <- cl
  z$method <- method
  z$formula <- formula
  z$terms <- mt
  z
}
