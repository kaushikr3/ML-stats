# Assignment 4 part 3
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

variance_pc1 = PC1$value #variance of PC1
PC1_values = (PC1$par/sqrt(sum(PC1$par^2))) #since vector is not a unit vector

input2 = apply(input, 1, function(x) x - PC1_values)
###############
# Finding PC2 #
###############
#Find PC2 values
PC2 <- optim(
  par = PC1_values,
  fn = objective_fn,
  data = t(input2))

variance_pc2 = PC2$value #variance of PC1
PC2_values = (PC2$par/sqrt(sum(PC2$par^2))) #since vector is not a unit vector

input3 = apply(input2, 1, function(x) x - PC2_values)
###############
# Finding PC3 #
###############
#Find PC2 values
PC3 <- optim(
  par = PC2_values,
  fn = objective_fn,
  data = t(input3))

variance_pc3 = PC3$value #variance of PC1
PC3_values = (PC2$par/sqrt(sum(PC2$par^2))) #since vector is not a unit vector

##########
# Output #
##########
var_table = data.frame(variance = c(variance_pc1,variance_pc2,variance_pc3), 
                       row.names = c("varPC1","varPC2","varPC3"))
PC_table = data.frame(PC1 = PC1_values, PC2 = PC2_values, PC3 = PC3_values)

write.table(var_table, output1)
write.table(PC_table, output2)

#END#


