# Gradient descent for negative binomial regression models
nbreg.gd <- function(X, y, r = 1, kappa = 1, max.grad = 1e-3)
{
  #########################
  # Initialisation
  p = ncol(X)
  n = nrow(X)

  # Standardise data
  stdX     = my.standardise(X)
  X        = stdX$X  
  
  # Initial starting values for coefficients
  beta0 = mean(log(y+1/2))
  beta = solve(t(X) %*% X, t(X) %*% (log(y+1/2)-beta0))
  rownames(beta) = colnames(X)

  L.path = c()

  func_res = func.2.6(y,X,beta0,beta,r)
  L.cur = func_res$negative.log.likelihood
  max_grad_j = max(abs(func_res$betaj))
  while(max_grad_j > max.grad){
    func_res = func.2.6(y,X,beta0,beta,r)
    grad_j = func_res$betaj
    grad_0 = func_res$beta0
    
    new_beta = beta - (grad_j)*k
    new_beta0 = beta0 - (grad_0)*k
    
    prior_neg_log=  func_res$negative.log.likelihood
    
    func_res = func.2.6(y,X,new_beta0,new_beta,r)
    post_neg_log=  func_res$negative.log.likelihood
    L.path = append(L.path, post_neg_log)
    L.cur = post_neg_log
    
    max_grad_j = max(abs(grad_j), abs(grad_0))
    cat(post_neg_log, "with k value of", k, "max_grad", max_grad_j,"\n")
    if (post_neg_log >= prior_neg_log){
      k = k*0.9
    }
    else {
      beta = new_beta
      beta0 = new_beta0
    }
  }
  
  
  beta <- beta / t(stdX$stdX)
  beta0 <- beta0 - stdX$meanX %*% beta
  
  # Return estimated beta's, neg-log-likelihood of model and gradient
  #   beta.hat     = your estimates for beta
  #   beta0.hat    = your estimate for beta0 (intercept)
  #   L            = likelihood at your final solution
  #   g            = gradient at your final solution
  #   L.path       = neg-log-likelihoods recorded at every iteration 
  #                  of algorithm 
  g = append(beta, beta0)
  
  return(list(beta.hat=as.vector(beta),beta0.hat=as.numeric(beta0),L=L.cur,grad=g,L.path=L.path))
}


# Take a formula and a data frame and return an X and y matrix
df2matrix <- function(formula,data)
{
  # Figure out the target variable
  mf = model.frame(formula,data=data)
  target.var = names(mf)[[1]]
  
  # Extract data
  y = as.matrix(mf[,1])
  mf = stats::model.matrix(formula, data=data)
  X = as.matrix(mf)
  X = X[,-1,drop=FALSE]
  
  # Return
  return(list("y"=y,"X"=X))
}


# function to standardise columns of X to have mean zero and unit length
my.standardise <- function(X)
{
  n = nrow(X)
  p = ncol(X)
  
  # 
  r       = list()
  r$X     = X
  if (p > 1)
  {
    r$meanX = colMeans(X)
  } else
  {
    r$meanX = mean(X)
  }
  r$stdX  = t(apply(X,2,stats::sd))
  
  # Perform the standardisation
  if (p == 1)
  {
    r$X <- as.matrix(apply(X,1,function(x)(x - r$meanX)))
    r$X <- as.matrix(apply(r$X,1,function(x)(x / r$stdX)))
  } else
  {
    r$X <- t(as.matrix(apply(X,1,function(x)(x - r$meanX))))
    r$X <- t(as.matrix(apply(r$X,1,function(x)(x / r$stdX))))
  }
  
  return(r)
}


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

