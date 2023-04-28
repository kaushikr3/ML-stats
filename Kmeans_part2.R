# Assignment 3 part 2
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
df = read.table(args[1], header = T)
output = args[2]

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
    print(dim(samples))
    centroid = centroids[i,]
    print(dim(centroids))
    #distance to centroids
    distances[select.i,] = sqrt(sum((samples - centroid)^2))*(1/ncol(df))
  }
  #return the average distance
  return(mean(distances))
}

k_means_nstart <- function(df, k){
  
  data = t(df)
  #randomly select the indices of k rows to use as starting
  #centers of the k clusters
  rand <- sample(1:nrow(data), k)
  
  #data frame with k observations that were randomly selected
  centroids <- data[rand,]
  
  #empty vectors that will contain the cluster assignments for each observation
  cluster_vec <- c()
  last_vec <- c(0)
  
  #iteration counter
  iter <- 1
  
  #algorithm will stop once stop is equal to 1
  stop <- 0
  
  while (stop == 0) {
    
    iter <- iter + 1
    
    #loop through each observation
    for (i in 1:nrow(data)) {
      
      #find the euclidean distance of the ith observation to each of the clusters
      dist <- data[i,] %>%
        rbind(centroids) %>%
        dist()
      
      #find which cluster the ith observation has the smallest distance with
      i_cluster <- dist[1:k] %>%
        which.min()
      
      #add the cluster assignment for the ith observation to a vector
      #containing the cluster assignments of all observations
      cluster_vec[i] <- i_cluster
    }
    
    #check to see if the cluster assignments have changed at all since
    #the last iteration
    if (all(cluster_vec == last_vec)) {
      stop <-  1
    }
    
    #save the cluster assignments from this iteration to another object
    #so we can check to see if cluster assignments changed
    last_vec <- cluster_vec
    
    #save the cluster assignments from this iteration to another object
    #so we can check to see if cluster assignments changed
    clusters = as.data.frame(last_vec)
    centroids = as.data.frame(centroids)
    
    #compute final value of objective function
    final = my_kmeans.cost(df, centroids, clusters)
    
    #return results
    results = rbind(clusters, final)
    return(results)
  }
}    

################
#Call function #
################
kmeans = list()
for(i in 1:25){
  kmeans[i] = k_means_nstart(as.data.frame(df), 7)
  final_cluster = data.frame(clusters = kmeans[[which.min(kmeans[[i]][51])]])
}


##########
# Output #
##########
write.table(final_cluster, output)

#######
# END #
#######
