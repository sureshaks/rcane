#' @rdname rlm.summaries
#' 
#' @method predict rlm
#' @export
predict.rlm <- function(object, newdata, ...) {
  if(missing(newdata) || is.null(newdata)) {
    return(fitted(object))
  } else{
    form <- formula(object$terms) # has . expanded
    environment(form) <- environment(object$formula)
    x <- model.matrix(form, newdata)
    return(as.matrix(x) %*% coef(object))
  }
}