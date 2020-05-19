# =======================================================================
# Names: Ander Eguiluz and Mikel Moreno
# Group Number: A
# Assignment: Assignment 5 - COVID Symptoms
# Date: 18/5/20
# Time spent in the assignment: 5-6h
# Proportion of effort done by members of the group: 50% Mikel-50% Ander
# Doubts and difficulties that arose during the realization: To manage getting the proper attributes to use the 
# special functions was the biggest issue 
# =======================================================================
# 1. Be sure to include, with this template, any necessary files
#    for execution, including datasets (problem.R, methodXXX.R, ...)
#    (submission of the entire template folder is recommended)
# 2. If you use a function of a certain package, do not forget to include the
#    corresponding call to the "library ()" function
# 3. Do not forget to comment on the code, especially those non-trivial commands
#    (remember that part of the rating depends on the cleaning of the code)
# 4. It is strongly recommended to test any implemented function in order to 
#    check for its proper operation
# =======================================================================
# (This is a general code, you must adapt it)
# =======================================================================
# Configuring the Environment
{rm(list=ls())
  cat("\014")
  graphics.off()
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  getwd()
  dir()

  # LIBRARIES (add any needed library here)
  
  library(mlbench)
  library(dplyr)
  library(rpart)
  
  library(caret)
  library(rpart.plot)
  
  library(ggplot2)
  
  # FUNCTIONS
  # =======================================================================
  # Define the entropy as a function able to receive any set of partitions
  entropy = function (s) {
    p  = (s/sum(s))
    logp  = log2(p)
    logp[logp==-Inf]  = 0
    
    return(sum(-p * logp))
  }
  
  # Defining information gain as a function able to receive any set of partitions
  gain = function (attr, target) {
    values  = table(attr, target)
    ps  = rowSums(values) / sum(values)
    es  = apply(values, 1, entropy)
    gain  = entropy(colSums(values)) - sum(ps * es)
    
    return(gain)
  }
  
}

# VARIABLES we are using
# =======================================================================
i = 1                   #iterator
vIssues = vector()      #Here we order the Symptoms
vGains  = vector()      #Here we order the gains of the different items
vResults = vector()     #Here we are storing the different accuracies of every iteration

# READING DATA 
# =======================================================================
dataTable = read.table("dataset/2020 covid-19-recommendations.tab", 
header=TRUE)

print(dataTable) #We assure that we have successfully read the dataset

#Lets see now the different gains for all relations and see which one is the biggest
# ==================================================================================

while(i<15){ # we print every symptom with its gain
  cat(sprintf("\"%s\" : \"%f\"\n", colnames(dataTable[i]), gain(dataTable[[i]], dataTable$TARGET)))
  vGains = append(vGains, gain(dataTable[[i]], dataTable$TARGET))
  i=i+1
}

# Now let's plot them in a proper order
issueMatrix = cbind(colnames(dataTable), vGains)
issueMatrix[order(-vGains),]


#FEVER turns out to be the main one

# Do this data partitioning 10 times 
# 1- Randomly divide data 
# 2- Train decision tree
# 3- Test the model
# ===================================
i=1
seed = 007
while(i<11){
  print(paste("ITERATION: ",i))
  set.seed(seed) # We set the random seed generator, different for every iteration
  
  index = createDataPartition(as.factor(dataTable$FEVER), p = 0.8, list = F) # this function is making the data partitions
                                                                             # 80% training and 20% testing
  #Obtaining training and testing datasets:                                                                           
  training = dataTable[ index,] # Training data
  testing = dataTable[-index,]  # Testing data
  rm(index)
  
  trainedModel = rpart(TARGET~., training) # Creating the decision tree
  
  rpart.plot(trainedModel) #we plot the decision tree with rpart.plot() -- way simpler than prp()
  
  predicciones = predict(trainedModel, testing, type= "class") # Making predictions with the testing model ; type = "class" for a coefficient covariance matrix
  
  predicVStest = table(predicciones, testing$TARGET) # We join data from made predictions and testing data
  
  #Let's calculate accuracy now:
  #-Accuracy is: GoodPredictions/TotalPredictions
  #-From a matrix of PREDICTIONS VS TESTING dataset we can infere that good predictions are the ones
  #-in [1,1] and [2,2] as are the ones that nailed the real outcome
  accuracyResults = (predicVStest[1,1]+predicVStest[2,2])  / sum(predicVStest)
  print("ACCURACY:")
  print(accuracyResults)
  
  #We store the actual value 
  vResults = append(vResults, accuracyResults)
  
  #Next iteration
  seed = seed + 7 #change seed for the next iteration
  i = i+1
}

# FINALLY:
# ========
# Build the tree with all the previous data
generalModel = rpart(TARGET~., dataTable) # Creating the decision tree

# Plot the general tree
rpart.plot(generalModel) 

# Print the top-5 most important symptoms 
print(generalModel) # Last print indicates the top5 symptoms of COVID taking into account the dataset

