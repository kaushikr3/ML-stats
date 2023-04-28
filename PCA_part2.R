# Assignment 4 part 2
# Author: Kaushik Raman
# NetID: kr2701

####################
# Loading packages #
####################
library(R.utils)

####################
# Loading the data #
####################
args = commandArgs(trailingOnly = T)
input = read.delim(args[1], header = T) #input df
vector = read.delim(args[2], header = F) #initializing vector
output1 = read.delim(args[3], header = F)
output2 = read.delim(args[4], header = F)

######################
# Objective function #
######################
objective_fn = function(vector,data){
  projected = ((data %*% vector) / sum(vector**2)) #projects the data onto the vector
  variance = -1*(var(projected) * sum(vector**2)) #finds variance
  return(variance)
}

###############
# Finding PC1 #
###############
#Find PC1 values
PC1 <- optim(
  par = vector[,1],
  fn = objective_fn,
  data = input)

variance = PC1$value #variance of PC1
PC1_values = (PC1$par/sqrt(sum(PC1$par^2))) #since vector is not a unit vector

##########
# Output #
##########
write.table(variance, output1)
write.table(PC1_values, output2)

#END#


