#' @rdname rlm.summaries
#' 
#' @method predict rlm
#' @export
predict.rlm <- function(object, newdata, ...) {
  if(missing(newdata) || is.null(newdata)) {
    return(fitted(object))
  } else{
    tt <- terms(object)
    Terms <- delete.response(tt)
    x <- model.matrix(Terms, newdata)
    return(as.matrix(x) %*% coef(object))
  }
}