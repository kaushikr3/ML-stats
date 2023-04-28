# Assignment 1
# Author: Kaushik Raman
# NetID: kr2701

####################
# Loading packages #
####################
library(R.utils, verbose = F)

############
# File I/O #
############
args = commandArgs(trailingOnly = T)
input_y = read.table(args[1], header = F)
input_x = read.table(args[2], header = T)
alpha = args[3]
epsilon = args[4]
output_file = args[5]

#####################
# File Manipulation #
#####################
X0 = rep(1, nrow(input_x))
input_x = as.matrix(cbind(X0,input_x))
X = as.matrix(input_x)
Y = as.matrix(input_y)

theta = matrix(0, nrow = ncol(X), ncol = 1)

#############
# Functions #
#############
computeCost = function(X,Y,theta){
  n = length(Y)
  yhat = X%*%theta
  cost = (1/(2*n))*sum((Y - yhat)^2)
  return(cost)
}

computeGrad = function(X,Y,theta){
  n = length(Y)
  yhat = X%*%theta
  grad = matrix(0, nrow = nrow(theta), ncol = 1)
    for (i in 1:nrow(theta)){
      grad[i,] = (1/n)*(sum(t(X[,i])%*%(yhat - Y)))
    }
  return(grad)
}

gradientDescent = function(X,Y,theta){
  theta = matrix(0, nrow = ncol(X), ncol = 1)
  deltaJ = 100
  alpha = alpha
  eps = epsilon
  cost = computeCost(X,Y,theta)
  while(deltaJ > eps){
    grad = computeGrad(X,Y,theta)
    theta = theta - alpha*grad
    prevCost = cost
    cost = computeCost(X,Y,theta)
    deltaJ = prevCost - cost
  }
  return(theta)
}

###################
# Minimizing cost #
###################
theta_new = gradientDescent(X,Y,theta)

new_cost = computeCost(X,Y,theta_new)

yhat = X%*%theta_new

print("The residual sum of squares is ",new_cost)
write.table(yhat, file = output_file, header = F)

#######
# END #
#######




