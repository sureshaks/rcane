set.seed(1)

x1=runif(50,0,3)
x2=runif(50,0,3)
y=3*(x1)+2*(x2)

#x1=scale(x1,center=T,scale=F)
#x2=scale(x2,center=T,scale=F)
#y=y-mean(y)
X=as.matrix(cbind(1,(x1),(x2)))
Y=as.matrix(y)
theta=as.matrix(rbind(0,0,0))
alpha=(seq(10e-02,20e-02,0.11e-01))


j=1
theta=c(0,0,0)
options(digits=2)
coordinate=function(X,Y,theta){
cost=NULL
n=1
tolerance=2
  p=0
 
  while(n<1000 &&  tolerance>0.005){
    for(j in (1:3)){
    
   
    hx=(X[,-j]%*%as.matrix(theta[-j]))
    derrivative=(Y-hx)*ifelse(j<2,(1/length(y)),1)
    derrivative=ifelse(derrivative<0.001,0,derrivative)
  
    theta[j]=(1/norm(as.matrix(X[,j]),"F")^2)*(t(derrivative)%*%X[,j])
    
    j=j+1
  }
cost[n]=((1/length(y))*sum((Y-X%*%theta)^2))
tolerance=sum(theta^2)-sum(p^2)
 p=theta
 n=n+1
  
  }
plot(x=seq(1,n-1,1),y=cost,type="l",xlab="No. Of Iterations")

  return(as.matrix(theta))
}


coordinate(X,Y,theta)
