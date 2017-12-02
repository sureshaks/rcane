plot_lossIter <- function(object) UseMethod("plot_lossIter")

plot_lossIter.rlm <- function(object, ...) {
  if(length(object$loss_iter)){
    plot(object$loss_iter$iter, object$loss_iter$loss, type='b', xlab='iter', ylab='loss function')
  } else {
    print("No residuals\n")
  }
}
