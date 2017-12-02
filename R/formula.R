#' @rdname rlm.summaries
#' 
#' @method formula rlm
#' @export
formula.rlm <- function(x, ...) {
  if(!is.null(x$formula)) {
    form <- formula(x$terms) # has . expanded
    environment(form) <- environment(x$formula)
    form
  } else {
    formula(x$terms)
  }
}