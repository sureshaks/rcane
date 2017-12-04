#' Accessing rlm Model Fits
#' @name rlm.summaries
#' @description All these functions are methods for class \code{"rlm"} objects.
#' @param object,x an object of class rlm
#' @param newdata An optional data frame in which to look for variables with which to predict. If omitted, the fitted values are used
#' @param ... futher arguments passed to or from other methods.
NULL

#' @rdname rlm.summaries
#' @method coef rlm
#' @export
coef.rlm <- function(object, ...) {
  if(length(object$coefficients)){
    return(object$coefficients)
  } else {
    print("No coefficients\n")
  }
}