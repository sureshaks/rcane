fitted.values.rlm <- function(object, ...) {
  if(length(object$fitted.values)){
    print(object$fitted.values)
  } else {
    print("No fitted values\n")
  }
}