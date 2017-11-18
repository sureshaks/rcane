
set.seed(1)

x1=runif(50,0,3)
x2=runif(50,0,3)
y=3*(x1)+2*(x2)


X=as.matrix(cbind(1,(x1),(x2)))
Y=as.matrix(y)
theta=as.matrix(rbind(0,0,0))



j=1
theta=c(0,0,0)
options(digits=2)
coordinate=function(X,Y,theta){
  m=nrow(X)
  

  magnitude=NULL
  tolerance=NULL
 
  while(n<1000){
    for(j in (1:3)){
    
    tolerance=0.005
    hx=(X[,-j]%*%as.matrix(theta[-j]))
    derrivative=(Y-hx)*ifelse(j<2,(1/length(y)),1)
    #derrivative=ifelse(derrivative<0.001,0,derrivative)
  
    theta[j]=(1/norm(as.matrix(X[,j]),"F")^2)*(t(derrivative)%*%X[,j])
    
    j=j+1
  }
  n=n+1
  
  }
  return(theta)
}


coordinate(X,Y,theta,alpha)

