moo_bgd <- function(X, Y, alpha=1, lambda=0, max_round=1000, precision=0.0001){
  alpha <- alpha / length(Y)
  X = cbind(X, 1)
  B = rep(0, nrow(X))
  
  round <- 1
  first_round = TRUE
  B_prev <- rep(NA, 13)
  
  while(round <= max_round &&
        !any(is.na(B)) &&
        (first_round || 
         any(abs(B_prev - B) > precision*B)
        )){
    
    B_prev <- B
    first_round <- FALSE
    
    for(i in 1:ncol(X)){
      pi <- 1/(1+exp(-(t(X[,i,drop=FALSE]) %*% B)))
      
      B <- B + alpha * ( X[,i, drop=FALSE] %*% (Y[i,drop=FALSE] - pi) - lambda * B)
      
    }
    
    round <- round+1
  }
  
  B
}