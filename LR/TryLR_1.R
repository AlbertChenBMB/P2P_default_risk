#Logistic Regression

#step1. function to update theta via gradient descent
update_theta<-function(theta_arg,n,x,y,d,alpha){
  #計算分子(numerator)
  numerator<-t(replicate(d,y))*x
  
  #perform element-wise multiplication between theta and x
  #t(x) 有問題
  theta_x_prod<-t(replicate(n,theta_arg))*t(x)
  #prior to getting their dot product
  dotprod<-rowSums(theta_x_prod)
  denominator<-1+exp(-y*dotprod)
  #cast the denominator as a matrix
  denominator<-t(replicate(d,denominator))
  #final step
  theta_arg<-theta_arg-alpha*rowSums(numerator/denominator)
  return(theta_arg)
}
#step2.
# wrapper around update_theta to iteratively update theta until the maximum number of iterations is reached
get_final_theta <- function(theta,x,y,threshold,alpha=0.9,max_iter = 100)
{
  n <- nrow(x)
  d <- length(theta)
  
  first_iteration <- TRUE
  total_iterations <- 0
  
  # while the number of iterations is below the input max allowable, 
  # update theta via gradient descent
  new_theta <- theta
  while(total_iterations <= max_iter)
  {
    
    first_iteration <- FALSE
    # create copy of theta for comparison between new and old
    old_theta <- new_theta
    new_theta <- update_theta(new_theta,n,x,y,d,alpha)
    
    diff_theta <- sqrt(sum((new_theta - old_theta)**2))
    
    if(diff_theta < threshold) {break}
    
    # index the iteration number
    total_iterations <- total_iterations + 1
    
  }
  
  # return the train theta parameter
  return(new_theta)
  
}
train_theta_result<-get_final_theta(theta = theta,x = x,y = y,threshold = 1,alpha = 0.9,max_iter = 100)
#final LR
tlr <- function(x,theta_arg)
{
  result <- sapply(1:nrow(x) , function(index) {1 / (1 + exp(sum(theta_arg * x[,index])))})
  return(result)
  
  
}
#test
tlr(x = x,theta_arg = train_theta_result)

  