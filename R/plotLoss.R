#' @title plotLoss: Plot loss vs iteration graph
#' @description Plot the result of loss function and number of iterations.
#' @export
#' @importFrom graphics plot
plotLoss <- function(object) UseMethod("plotLoss")

#' @describeIn plotLoss Plot loss vs iteration of rlm object
#' @method plotLoss rlm
#' @param object an object of class rlm
#' @param ... other arguments
#' @export
plotLoss.rlm <- function(object, ...) {
  if(length(object$loss_iter)){
    plot(object$loss_iter$iter, object$loss_iter$loss, type='b', xlab='iter', ylab='loss function')
  } else {
    print("No residuals\n")
  }
}

#' @describeIn plotLoss Plot loss vs iteration
#' @export
plotLoss.default <- function(object, ...) {
  if(length(object$loss_iter)){
    plot(object$loss_iter$iter, object$loss_iter$loss, type='b', xlab='iter', ylab='loss function')
  } else {
    print("No residuals\n")
  }
}
