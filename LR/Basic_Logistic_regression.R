#basic logistic regression 
#sigmoid
my_sigmoid<-function(z){
        1/(1+exp(-z))
}
#cost function
my_cost<-function(theta,X,y,lambda){
        cost_gradient=list()
        #cost_gradient compute cost and gradient for logistic regression using theta
        h_theta=my_sigmoid(X %*% theta)
        cost =1/nrow(X)*sum(-y*log(h_theta)-(1-y)*log(1-h_theta))+lambda/(2*nrow(X))*sum(theta^2)
        return(cost)
}

#gradient
my_gradient<-function(theta,X,ny,lambda){
        h_theta=my_sigmoid(X %*% theta)
        ##option 1 --looping
        gradient=rep(0,ncol(X))
        for(j in 2:ncol(X)){
                for(i in 1:nrow(X)){
                        gradient[1]=gradient[1]+1/nrow(X)*(my_sigmoid(X[i,]%*%theta)-ny[i])*X[i,1]
                        gradient[j]=gradient[j]+1/nrow(X)*(my_sigmoid(X[i,]%*%theta)-ny[i])*X[i,j]+lambda/(nrow(X))*theta[j]
                }
        }
                
}
# initial_theta
initial_theta=matrix(rep(0,ncol(X)))
#training model
optimized=optim(par=initial_theta,X=X,y=y,ny=, labmda=1,fn=my_cost,gr=my_gradient,method="BFGS")

#test