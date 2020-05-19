# =======================================================================
# Names: Ander Eguiluz and Mikel Moreno
# Group Number: A
# Assignment: Assignment 4 - Linear Regression
# Date: 18/5/20
# Time spent in the assignment: 5-6h
# Proportion of effort done by members of the group: 50% Mikel-50% Ander
# Doubts and difficulties that arose during the realization: Understand how to use ggplot and 
# alpha meaning in the linearization expression 
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
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  getwd()
  dir()
  
  # LIBRARIES (add any needed library here)
  
  library(ggplot2)
  library(caret)
  
}


# Read data from csv
#====================== 
csvData = read.csv(file.path("../data/2020 - 2019 world happiness.csv"))

# structure of the data relation we are going to use in the first problem (relation between GPD per capita and the obtained score)
ggplot(csvData, aes(x = GDP.per.capita, y = Score)) + geom_point()


# ========================================================================================================
# ========================================================================================================

# Given one factor we are going to get the linear regression to obtain the "Score"


# Let's try to linearize with lm() function
# ==========================================

# Data frame of worldHappinessLin
worldHappinessData = data.frame(
  x1 = csvData$GDP.per.capita,
  x0 = 1,
  y  = csvData$Score,
  h  = 0,
  e  = 0
)

# Use lm function
worldHappinessLin = lm(y ~ x1, data = worldHappinessData)
worldHappinessData$h = predict(worldHappinessLin, worldHappinessData)

# Visualize & summarize the model 
print(worldHappinessLin) 
summary(worldHappinessLin)

# Plot "Score" & "hypothesis"
ggplot(worldHappinessData) + 
  geom_point(aes(x = x1, y = y, col = "Data")) + 
geom_point(aes(x = worldHappinessData$x1, y = worldHappinessData$h, col = "worldHappinessLin hypothesis")) + 
  geom_line(aes(x = worldHappinessData$x1, y = worldHappinessData$h, col = "worldHappinessLin hypothesis")) + 
  labs(caption=paste0("worldHappinessLin_w1=", round(coef(worldHappinessLin)[[2]], digits=3),
                      "  worldHappinessLin_w0=", round(coef(worldHappinessLin)[[1]], digits=3)))

# ====================================================================================================
# ====================================================================================================

# Repeat 10 times:
# -Randomly divide data in train (80%) and test (20%)
# -Train a linear regression worldHappinessLin to predict Score given the six factors
# -Test the worldHappinessLin and print Mean Absolute Error (MAE)

# Variables
number = 112  
ObtainedMAEs = vector() # store MAE values 

# Let's try to linearize with lm() function
# ==========================================

for (i in 1:10) {
  
  # Seeds 
  set.seed(number)
  number = number + 7
  
  #Select 80% of the data and create partitions
  dataPartition = createDataPartition(
    csvData$Overall.rank,
    times = 1,
    p = 0.8,
    list = FALSE
  )
  trainDF = csvData[dataPartition,] # get the 80% of the data for training
  testDF = csvData[-dataPartition,] # the rest (20%) for testing in descending order
  
  # Meaningful names to variables [TRAINING]
  cleanTrain = data.frame(
    x6 = trainDF$Social.support,
    x5 = trainDF$Healthy.life.expectancy,
    x4 = trainDF$Freedom.to.make.life.choices,
    x3 = trainDF$Generosity,
    x2 = trainDF$Perceptions.of.corruption,
    x1 = trainDF$GDP.per.capita,
    x0 = 1,
    y  = trainDF$Score,
    h  = 0,
    e  = 0
  )
  
  # Meaningful names to variables [TESTING]
  cleanTest = data.frame(
    x6 = testDF$Social.support,
    x5 = testDF$Healthy.life.expectancy,
    x4 = testDF$Freedom.to.make.life.choices,
    x3 = testDF$Generosity,
    x2 = testDF$Perceptions.of.corruption,
    x1 = testDF$GDP.per.capita,
    x0 = 1,
    y  = testDF$Score,
    h  = 0,
    e  = 0
  )
  
  # Use lm() function
  worldHappinessLin = lm(y ~ x1+x2+x3+x4+x5+x6, data = cleanTrain)
  
  # Add hypothesis and error values to the training dataframe
  cleanTrain$h = predict(worldHappinessLin, cleanTrain)
  cleanTrain$e = cleanTrain$y - cleanTrain$h
  
  # Plot the trained worldHappinessLin
  print(ggplot(cleanTrain) + 
          geom_point(aes(x = x1, y = y, col = "Data")) + 
          geom_point(aes(x = x1, y = h, col = "worldHappinessLin hypothesis")) + 
          geom_line(aes(x = x1, y = h, col = "worldHappinessLin hypothesis")) + 
          labs(caption=paste0(
            "  w6=", round(coef(worldHappinessLin)[[7]], digits=5),
            "  w5=", round(coef(worldHappinessLin)[[6]], digits=5),
            "  w4=", round(coef(worldHappinessLin)[[5]], digits=5),
            "  w3=", round(coef(worldHappinessLin)[[4]], digits=5),
            "  w2=", round(coef(worldHappinessLin)[[3]], digits=5),
            "  w1=", round(coef(worldHappinessLin)[[2]], digits=5),
            "  w0=", round(coef(worldHappinessLin)[[1]], digits=5)
          ))
  )
  
  
  # Print information about the trained worldHappinessLin
  
  print(paste0(" Calculated weights in training data and error"))
  print(paste0(
    " w6=", round(coef(worldHappinessLin)[[7]], digits=5),
    " w5=", round(coef(worldHappinessLin)[[6]], digits=5),
    " w4=", round(coef(worldHappinessLin)[[5]], digits=5),
    " w3=", round(coef(worldHappinessLin)[[4]], digits=5),
    " w2=", round(coef(worldHappinessLin)[[3]], digits=5),
    " w1=", round(coef(worldHappinessLin)[[2]], digits=5),
    " w0=", round(coef(worldHappinessLin)[[1]], digits=5),
    " error=", round(mean(abs(cleanTrain$e)), digits=5))
  )
  
  # Test the created worldHappinessLin comparing it WITH test worldHappinessLin (hipothesys=h vs reality=y)
  cleanTest$h = 
    coef(worldHappinessLin)[[7]]*cleanTest$x6 + 
    coef(worldHappinessLin)[[6]]*cleanTest$x5 + 
    coef(worldHappinessLin)[[5]]*cleanTest$x4 + 
    coef(worldHappinessLin)[[4]]*cleanTest$x3 + 
    coef(worldHappinessLin)[[3]]*cleanTest$x2 + 
    coef(worldHappinessLin)[[2]]*cleanTest$x1 + 
    coef(worldHappinessLin)[[1]]*cleanTest$x0
  
  cleanTest$e = cleanTest$y - cleanTest$h
  
  # We add MAE value to the vector
  ObtainedMAEs = append(ObtainedMAEs, mean(abs(cleanTest$e))) 
  
  # Visualize calculated R-squared values
  print(paste0(" Multiple R^2 = ", summary(worldHappinessLin)$r.squared))
  print(paste0(" Adjusted R^2 = ", summary(worldHappinessLin)$adj.r.squared))
  
  # Visualize calculated MAE
  print(paste0(" Calculated weights VS Real test values"))
  print(paste0(" MAE = ", mean(abs(cleanTest$e))))
  
  
  invisible(readline(prompt = "Press <-' "))
  
}

# ========================================================================================================
# ========================================================================================================

#FINALLY: 
#========

# Calculate average and variability in MAE
var(ObtainedMAEs)

mean(ObtainedMAEs)

# Summary to conclude
summary(worldHappinessLin)

