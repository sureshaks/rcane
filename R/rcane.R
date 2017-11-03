rcane <- function (formula, data, alpha=1, method = "sgd"){
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
  if(method=="sgd"){
    z <- (StochasticGradientDescent(x, y, alpha))
  }
  z
}