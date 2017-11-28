coef.rlm <- function(object, ...) {
  if(length(object$coefficients)){
    print(object$coefficients)
  } else {
    print("No coefficients\n")
  }
}