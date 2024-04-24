rm(list = ls())
x = matrix(c(1,2,3,4))
y = matrix(c(5,6,7,8))
t(x) %*% y
x %*% y
nrow(x)
ncol(x)
dim(x)

X = matrix(1,2,3)
X
Y = matrix(c(1,2,3,4,5,6), 3, 2)
Y
d = diag(c(1,2,3,4))
d
I = diag(1,3)
I

X + t(Y)

setwd("D:/Desktop/R files/3154 Files")
bpdata = read.csv("bpdata.csv")
X = as.matrix(bpdata[,c("Age","Weight","Pulse","Stress")])
X
n = nrow(X)
n
p = ncol(X)
p
cor(X)
beta = as.matrix(c(0.7, 1.1, -0.13, 0.2))
beta0 = -17
yp = X %*% beta + beta0
yp

y = as.matrix(bpdata["BP"])
e = y - yp
#residual
e
#RSS
t(e) %*% e

X = cbind(matrix(1,n,1),X)
X
#gets the inverse of the matrix
XtXi = solve(t(X) %*% X)
XtXi
XtXi %*% (t(X) %*% X)
#lest squares
b = XtXi %*% t(X) %*% y
b
b = solve(t(X)%*%X, t(X)%*%y)
b

rv = lm(BP ~ Age+Weight+Pulse+Stress,data=bpdata)
rv$coefficients

# Implementation of least-squares method for fitting linear regression models
# for FIT3154, Daniel Schmidt
#
# Takes a design matrix (without intercept) and a target vector, and returns
#  (i) the least-squares estimates for the intercept & coefficient
# (ii) the unbiased estimator of the error variance
#(iii) an estimate of the covariance matrix of the estimates
#
my.ls <- function(X, y)
{
  # Error checks
  if (!is.matrix(X) || (!is.matrix(y) && !is.vector(y)))
  {
    stop("Must pass a matrix X and vector y")
  }
  
  # Dimensions of data
  n = nrow(X)
  p = ncol(X)
  if (nrow(y) != n)
  {
    stop("Target vector and design matrix must have same number of rows")
  }
  
  # Return values
  rv = list()
  
  # Augment with intercept
  X = cbind(matrix(1,n,1), X)
  colnames(X)[[1]] = "(Intercept)"
  
  # Compute least squares solutions
  XtX = t(X) %*% X
  rv$b = solve(XtX, t(X) %*% y)
  
  # Compute unbiased estimate of variance
  rv$sigma2 = sum( (y-X %*% rv$b)^2 )/(n-p-1)
  
  # Estimate covariance matrix
  rv$covB = rv$sigma2 * solve(XtX)
  
  # Done
  return(rv)
}

X = as.matrix(bpdata[,c("Age","Weight","Pulse","Stress")])
fit.ls = my.ls(X, y)
n = nrow(X)
v = 0.01*X[,1] + rnorm(n,0,1)
cor(X[,1],v)
plot(X[,1],v)
diag(fit.ls$covB)


s = 0.01
Z = cbind(X, s*X[,1]+rnorm(n,0,1))
colnames(Z)[[5]] = "NewVar"
cor(Z)
fit.ls = my.ls(Z, y)
n = nrow(Z)
v = 0.01*Z[,1] + rnorm(n,0,1)
cor(Z[,1],v)
plot(Z[,1],v)
diag(fit.ls$covB)


s = 10
Z = cbind(X, s*X[,1]+rnorm(n,0,1))
colnames(Z)[[5]] = "NewVar"
cor(Z)
fit.ls = my.ls(Z, y)
n = nrow(Z)
v = 0.01*Z[,1] + rnorm(n,0,1)
cor(Z[,1],v)
plot(Z[,1],v)
diag(fit.ls$covB)


s = 100
Z = cbind(X, s*X[,1]+rnorm(n,0,1))
colnames(Z)[[5]] = "NewVar"
cor(Z)
fit.ls = my.ls(Z, y)
n = nrow(Z)
v = 0.01*Z[,1] + rnorm(n,0,1)
cor(Z[,1],v)
plot(Z[,1],v)
diag(fit.ls$covB)





