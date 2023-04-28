# Assignment 2
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
input = read.table(args[1], header = T)
output = args[2]

#####################
# File Manipulation #
#####################
#transposing the matrix to have predictors as columns
df = as.data.frame(t(input))
#changing the factors/categories to 0s and 1s
labels = gsub("_.*","",rownames(df))
df$labels = labels
for(level in levels(df$labels)){
  levels(df$labels) = c(0,1)
}

#############
# Functions #
#############
computeMSE = function(label, probs){
  #formula to compute the mean squared error
  mse = sum((as.numeric(label) - probs)^2)  
  return(mse)
}

forwardstep = function(df,prev_feature){
  #find the remaining features
  remaining = setdiff(colnames(df),c(prev_feature,"labels")) 
  minMSE = Inf
  for(feat in remaining){
    #formula for the glm model for forward selection
    fx = as.formula(paste("labels ~ ",paste(c(prev_feature,feat),collapse = "+"), sep = "")) #formula for glm()
    #running the model
    model = glm(formula = fx, family = "binomial", data = df) 
    #predicting the probabilities
    probs = predict.glm(model, type = "response")  
    #finding the error for each feature
    MSE = computeMSE(df$labels,probs) 
    if(MSE < minMSE){
      #selecting the feature with least error
      select_feat = feat 
      minMSE = MSE
    }
  }
  return(select_feat)
}

compute_cvMSE = function(df,formula,nfolds){
  #shuffle samples
  shuffle = df[sample(nrow(df), nrow(df)),]
  #perform cross-validation
  folds <- cut(seq(1,nrow(shuffle)),breaks=nfolds,labels=FALSE)
  MSEs = numeric()
  for(fold in 1:nfolds){
    #separate test/training sets
    traindata = shuffle[folds != fold,]
    testdata= shuffle[folds == fold,]
    #fit model
    model.cv = glm(formula, family = "binomial", data = traindata)
    #predict probabilities
    probs.test = predict.glm(model.cv, type = "response", newdata = testdata)
    #compute MSE on test
    MSEs[fold] = computeMSE(testdata$labels, probs.test)
  }
  return(mean(MSEs))
}

loglikelihood = function(df,labels){
  llh = numeric()
  for(i in 1:ncol(df)){
    #formula to compute the log-likelihood
    llh[i] = sum( (labels[i]*log(prob)) + ((1-labels[i])*log(1-prob)) )
  }
  return(llh)
}

##################################################
# Finding the best 50 genes by forward selection #
##################################################
prev_feat = c()
for(k in 1:50){
  #doing the forward selection for 50 best genes
  select.feature = forwardstep(df,prev_feat) 
  #updating the vector to include selected genes
  prev_feat = c(prev_feat,select.feature) 
}

####################
# Computing CV MSE #
####################
#formula for CV
fx = as.formula(paste0("labels ~", paste(prev_feat, collapse="+")))
#finding the CV MSE
cvMSE = compute_cvMSE(df = df,formula = fx, nfolds = 5) 


##############################
# Finding the log likelihood #
##############################
fx = as.formula(paste0("labels ~", paste(prev_feat, collapse="+"))) #formula for the model
model = glm(formula = fx, family = "binomial", data = df) #running the model with the best 50 genes
prob = predict.glm(model, type = "response") #predicting their probabilities

#finding the log-likelihood for each predictor
log_likelihood = loglikelihood(df[,prev_feat],as.numeric(levels(df$labels))) 

################
# Output table #
################
#making the output table
results.df = data.frame(iteration = c(1:50), predictors = prev_feat, 
                        CV = cvMSE, Log.Likelihood = log_likelihood[,1])

# writing the output to a file
write.table(results.df,output, sep = "\t", quote = F, row.names = F)
 
#######
# END #
#######








