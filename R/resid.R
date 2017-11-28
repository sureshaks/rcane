resid.rlm <- function(object, ...) {
  if(length(object$residuals)){
    print(object$residuals)
  } else {
    print("No residuals\n")
  }
}
