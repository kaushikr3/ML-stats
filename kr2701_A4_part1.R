# Assignment 4 part 1
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
output = read.delim(args[3], header = F)

#############
# Functions #
#############
project_vector = function(data, vector){
  projected = ((data %*% vector) / sum(vector**2))
  return(projected)
}

compute_variance = function(vector,init_vec){
  variance = var(vector) * sum(init_vec**2)
  return(variance)
}
########################
# Calling the function #
########################
projected_value = project_vector(input,vector[,1]) #projects data onto vector

variance = compute_variance(projected_value,vector) #compute the variance

##########
# Output #
##########
write.table(variance, output)

#END#