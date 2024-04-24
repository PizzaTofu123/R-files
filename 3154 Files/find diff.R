func.2.6 =  function(y,X,beta0,beta,r){
  ret_list = list()
  n = length(y)
  ret_list$negative.log.likelihood = -sum(lgamma(y+r)) + sum(lgamma(y+1)) + n*lgamma(r)- n*r*log(r) - sum(y)*beta0 - y%*%X%*%beta + 
    sum((y+r)*log(r+exp(beta0+rowSums(X%*%beta))))
  
  ret_list$beta0 = -sum(y) + sum((y+r)*(exp(beta0+rowSums(X%*%beta))/(r+exp(beta0+rowSums(X%*%beta)))))
  
  ret_list$betaj = c()
  for (j in 1:length(beta)){
    ret_list$betaj[length(ret_list$betaj)+1] = - sum(y*X[,j]) + sum((y+r)*(exp(beta0+rowSums(X%*%beta))/(r+exp(beta0+rowSums(X%*%beta)))))*X[,j]
  }
  
  return(ret_list)
}
#2.6
func.2.6 =  function(y,X,beta0,beta,r){
  ret_list = list()
  n = length(y)
  ret_list$negative.log.likelihood = -sum(lgamma(y+r)) + sum(lgamma(y+1)) + n*lgamma(r)- n*r*log(r) - sum(y)*beta0 - sum(y*rowSums(X%*%beta)) + 
    sum((y+r)*log(r+exp(beta0+rowSums(X%*%beta))))
  
  ret_list$beta0 = -sum(y) + sum((y+r)*(exp(beta0+rowSums(X%*%beta))/(r+exp(beta0+rowSums(X%*%beta)))))
  
  ret_list$betaj = c()
  for (j in 1:length(beta)){
    ret_list$betaj= append(ret_list$betaj, (-sum(y*X[,j]) + sum(((y+r)*X[,j]*(exp(beta0+rowSums(X%*%beta))))/(r+exp(beta0+rowSums(X%*%beta))))))
  }
  
  return(ret_list)
}