# Assignment 3 part 1
# Author: Kaushik Raman
# NetID: kr2701

####################
# Loading packages #
####################
library(R.utils, quietly = T)
library(tidyverse, quietly = T)

############
# File I/O #
############
args = commandArgs(trailingOnly = T)
df = read.table(args[1], header = T, sep = " ")
seeding = read.table(args[2], header = F)
output = args[3]

#############
# Functions #
#############
my_kmeans.cost = function(df,centroids,clusters){
  n = nrow(df)
  k = nrow(centroids)
  
  #compute distances between every data point and respective centroids
  distances = matrix(data = NA, nrow = n, ncol = 1)
  for(i in 1:k){
    select.i = which(clusters == i)
    samples = df[select.i,]
    centroid = centroids[i,]
    #distance to centroids
    distances[select.i,] = sqrt(sum((samples - centroid)^2))*(1/ncol(df))
  }
  #return the average distance
  return(mean(distances))
}

my_kmeans = function(df, init.clusters, k){
  df = t(df)
  #define number of samples and features
  n = nrow(df)
  p = ncol(df)
  
  #start with initial clusters
  clusters = init.clusters
  #initialize centroids
  centroids = matrix(NA, nrow = k, ncol = p)
  
  #cbinding the initial cluster to the df
  df1 = as.data.frame(df) %>% cbind(clusters)
  
  #kmeans clustering
  max.step = 1e6
  step.num = 0
  converged = F
  
  #while algorithm not converged
  while(!converged){
    #iteration count
    step.num = step.num + 1
    
    #save current cluster labels
    prev = clusters
    #print(clusters)
    
    #compute cluster specific centroid
    for(i in 1:k){
      for(j in 1:ncol(df)){
        df2 = df1[df1$V1==i,] %>% select(c(1:10))
        centroids[i,j] = mean(df2[,j]) 
      }
    }
    
    dist = matrix(NA, nrow = n, ncol = k)
    #find the euclidean distance of the ith observation to each of the clusters
    for(i in 1:nrow(df)){
      for(j in 1:nrow(centroids)){
        dist[i,j] = sqrt(sum(df[i,] - centroids[j,])^2)
      }
    }
    
    #assign the clusters
    for(i in 1:nrow(dist)){
      prev[i,] = which.min(dist[i,])
    }
    #print(dim(clusters)) 
    #print(dim(prev))
    
    
    #check for convergence
    if(all(clusters==prev) || step.num > max.step){
      converged = T
    }
  }   
  #save the cluster assignments from this iteration to another object
  #so we can check to see if cluster assignments changed
  clusters = prev
  centroids = centroids
  
  #compute final value of objective function
  final = my_kmeans.cost(df, centroids, clusters)
  
  #return results
  results = rbind(clusters, final)
  return(results)
}

################
#Call function #
################
results = my_kmeans(df, seeding, k = 7)

##########
# Output #
##########
write.table(results, output)

#######
# END #
#######

