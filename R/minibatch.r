getTheta <- function(columnLength, minTheta=0, maxTheta=1, seed=1){
  #create static random
  set.seed(seed)
  #random a value
  thetaList <- runif(columnLength, min=minTheta, max=maxTheta)
  #clear static random
  set.seed(seed)
  #transform into matrix
  result <- matrix(unlist(thetaList), ncol=columnLength, nrow=1, byrow=FALSE)
  return(result)
}


mini_batch_gd <- function(X, Y, alpha=0.1, maxIter=10, batchRate=0.5, seed=1){
  #shuffle data
  set.seed(seed)
  X <- X[sample(nrow(X)), ]
  set.seed(NULL)
  #initialize theta
  theta <- t(getTheta(ncol(X), seed=seed))
  #bind 1 column to dataTrain
  X <- cbind(1, X)
  updateRule <- matrix(0, ncol=length(theta), nrow=1)
  #constant variables
  rowLength <- nrow(data)
  batchSize <- rowLength * batchRate
  #loop the gradient descent
  for(iteration in 1:maxIter){
    temporaryTheta <- theta
    for(i in seq(1, rowLength, batchSize)){
      indexes <- i:(i+batchSize)
      
      inputTemp <- inputData[indexes,]
      inputTemp <- inputTemp[!is.na(inputTemp[,1])]
      outputTemp <- outputData[indexes,]
      outputTemp <- outputTemp[!is.na(outputTemp[,1])]
      
      yhat <- inputTemp %*% theta
      theta <- theta + 2 * alpha * t(X) %*% (outputTemp - yhat)
    }
    
    if(any(is.na(theta)) ||
       !any(abs()))
  }
  result <- theta
  return(result)
}
