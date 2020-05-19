# =======================================================================
# Names: Ander Eguiluz and Mikel Moreno
# Group Number: A
# Assignment: Assignment 4 - Linear Regression
# Date: 18/5/20
# Time spent in the assignment: 5-6h
# Proportion of effort done by members of the group: 50% Mikel-50% Ander
# Doubts and difficulties that arose during the realization: ???
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
csv_dataframe = read.csv(file.path("../data/2020 - 2019 world happiness.csv"))

# structure of the data relation we are going to use in the first problem (relation between GPD per capita and the obtained score)
ggplot(csv_dataframe, aes(x = GDP.per.capita, y = Score)) + geom_point()


# ========================================================================================================
# ========================================================================================================

# Given one factor we are going to get the linear regression to obtain the "Score"

#-------------------------------------1.not using lm()-----------------------------------------------------

# initialize dataframe
first_set = data.frame(
  x1 = csv_dataframe$GDP.per.capita,
  x0 = 1,
  y  = csv_dataframe$Score,
  h  = 0,
  e  = 0
)
# initialize weights
w0 = 1
w1 = 1

# set hypothesis and error formulas
first_set$h = w1*first_set$x1 + w0*first_set$x0
first_set$e = first_set$y - first_set$h # error is the distance between what the function has told me vs what really the y value is.

iterations = 300 # number of iterations on the loop
alpha = 0.01 


# each iteration i adjust the weights, "feed the function".
for (i in 1:iterations) {
  w1 = w1 + alpha * sum(first_set$e * first_set$x1) / nrow(first_set)
  w0 = w0 + alpha * sum(first_set$e * first_set$x0) / nrow(first_set)
  first_set$h = w1*first_set$x1 + w0*first_set$x0
  first_set$e = first_set$y - first_set$h
  print(paste0(
    i,
    " w1=", round(w1, digits=5),
    " w0=", round(w0, digits=5),
    " error=", round(mean(abs(first_set$e)),digits=5)
    )
  )
}

# plot both the "Score" and the "hypothesis"
ggplot(first_set) + 
  geom_point(aes(x = x1, y = y, col = "Data")) + # as a point
  geom_point(aes(x = x1, y = h, col = "Hypothesis")) + # as a point
  geom_line(aes(x = x1, y = h, col = "Hypothesis")) + # as a line
  labs(caption=paste0("w1=", round(w1, digits=3),"  w0=", round(w0, digits=3),"  Nº iterations= ", i))




#---------------------------------------------2. Using lm()-----------------------------------------------

# initialize dataframe
dataset_of_model = data.frame(
  x1 = csv_dataframe$GDP.per.capita,
  x0 = 1,
  y  = csv_dataframe$Score,
  h  = 0,
  e  = 0
)

# use lm function
model = lm(y ~ x1, data = dataset_of_model)
dataset_of_model$h = predict(model, dataset_of_model)
print(model)

summary(model)

# plot  both the "Score" and the "hypothesis"
ggplot(first_set) + 
  geom_point(aes(x = x1, y = y, col = "Data")) + 
  geom_point(aes(x = dataset_of_model$x1, y = dataset_of_model$h, col = "Model hypothesis")) + 
  geom_line(aes(x = dataset_of_model$x1, y = dataset_of_model$h, col = "Model hypothesis")) + 
  labs(caption=paste0("w1=", round(coef(model)[[2]], digits=3),
                      "  w0=", round(coef(model)[[1]], digits=3),
                      "  Nº iterations= ", i
                      
  ))

# plot both non lm and lm functions to compare them and see diferences
ggplot(first_set) + 
  geom_point(aes(x = x1, y = y, col = "Data")) + 
  geom_point(aes(x = x1, y = h, col = "Manual hypothesis")) + 
  geom_line(aes(x = x1, y = h, col = "Manual hypothesis")) + 
  geom_point(aes(x = dataset_of_model$x1, y = dataset_of_model$h, col = "Model hypothesis")) + 
  geom_line(aes(x = dataset_of_model$x1, y = dataset_of_model$h, col = "Model hypothesis")) + 
  labs(caption=paste0("LM_W1=", round(coef(model)[[2]], digits=3),
                      "  LM_W0=", round(coef(model)[[1]], digits=3),
                      "  Handmade_w1=", round(w1, digits=3),
                      "  Handmade_w0=", round(w0, digits=3),
                      "  Nº iterations= ", i
                      
  ))


# ====================================================================================================
# ====================================================================================================

# Repeat 10 times division of data, training with 6 factors and test it and measure it #


# ------------------------------------------non-lm() part----------------------------------------------


# value for random data-partitioning
number = 007

# vector (like an array) for MAE comparisons
ObtainedMAEs = vector()

for (i in 1:10) {
  
  # create partitions
  set.seed(number)
  number = number + 7
  
  #select 80% of the data for training
  training_data_in_indexes = createDataPartition(
    csv_dataframe$Overall.rank,
    times = 1,
    p = 0.8,
    list = FALSE
  )
  training_dataframe = csv_dataframe[training_data_in_indexes,] # get the 80% of the data for training
  testing_dataframe = csv_dataframe[-training_data_in_indexes,] # the rest (20%) for testing
  
  # adapt the format of the TRAINING dataframe
  training_dataframe_parsed = data.frame(
    x6 = training_dataframe$Social.support,
    x5 = training_dataframe$Healthy.life.expectancy,
    x4 = training_dataframe$Freedom.to.make.life.choices,
    x3 = training_dataframe$Generosity,
    x2 = training_dataframe$Perceptions.of.corruption,
    x1 = training_dataframe$GDP.per.capita,
    x0 = 1,
    y  = training_dataframe$Score,
    h  = 0,
    e  = 0
  )
  
  # adapt the format of the TESTING dataframe
  testing_dataframe_parsed = data.frame(
    x6 = testing_dataframe$Social.support,
    x5 = testing_dataframe$Healthy.life.expectancy,
    x4 = testing_dataframe$Freedom.to.make.life.choices,
    x3 = testing_dataframe$Generosity,
    x2 = testing_dataframe$Perceptions.of.corruption,
    x1 = testing_dataframe$GDP.per.capita,
    x0 = 1,
    y  = testing_dataframe$Score,
    h  = 0,
    e  = 0
  )
  
  # initialize weights
  w0 = 1
  w1 = 1
  w2 = 1
  w3 = 1
  w4 = 1
  w5 = 1
  w6 = 1
  
  # set hypothesis and error
  training_dataframe_parsed$h = 
    w6*training_dataframe_parsed$x6 + 
    w5*training_dataframe_parsed$x5 + 
    w4*training_dataframe_parsed$x4 + 
    w3*training_dataframe_parsed$x3 + 
    w2*training_dataframe_parsed$x2 + 
    w1*training_dataframe_parsed$x1 + 
    w0*training_dataframe_parsed$x0
  
  training_dataframe_parsed$e = training_dataframe_parsed$y - training_dataframe_parsed$h
  
  #train the model
  iterations = 1100 # number of iterations
  alpha = 0.0001
  
  for (j in 1:iterations) {
    w6 = w6 + alpha * sum(training_dataframe_parsed$e * training_dataframe_parsed$x6)
    w5 = w5 + alpha * sum(training_dataframe_parsed$e * training_dataframe_parsed$x5)
    w4 = w4 + alpha * sum(training_dataframe_parsed$e * training_dataframe_parsed$x4)
    w3 = w3 + alpha * sum(training_dataframe_parsed$e * training_dataframe_parsed$x3)
    w2 = w2 + alpha * sum(training_dataframe_parsed$e * training_dataframe_parsed$x2)
    w1 = w1 + alpha * sum(training_dataframe_parsed$e * training_dataframe_parsed$x1)
    w0 = w0 + alpha * sum(training_dataframe_parsed$e * training_dataframe_parsed$x0)
    
    training_dataframe_parsed$h = 
      w6*training_dataframe_parsed$x6 + 
      w5*training_dataframe_parsed$x5 + 
      w4*training_dataframe_parsed$x4 + 
      w3*training_dataframe_parsed$x3 + 
      w2*training_dataframe_parsed$x2 + 
      w1*training_dataframe_parsed$x1 + 
      w0*training_dataframe_parsed$x0
    
    training_dataframe_parsed$e = training_dataframe_parsed$y - training_dataframe_parsed$h
  }
  
  # plot the trained model
  print(ggplot(training_dataframe_parsed) + 
          geom_point(aes(x = x1, y = y, col = "Data")) + 
          geom_point(aes(x = x1, y = h, col = "Hypothesis")) + 
          geom_line(aes(x = x1, y = h, col = "Hypothesis")) + 
          labs(caption=paste0(
            "  w6=", round(w6, digits=4),
            "  w5=", round(w5, digits=4),
            "  w4=", round(w4, digits=4),
            "  w3=", round(w3, digits=4),
            "  w2=", round(w2, digits=4),
            "  w1=", round(w1, digits=4),
            "  w0=", round(w0, digits=4)
          ))
  )
  
  # print info of the trained model
  print(paste0(" Calculated weights and error"))
  print(paste0(
    " w6=", round(w6, digits=5),
    " w5=", round(w5, digits=5),
    " w4=", round(w4, digits=5),
    " w3=", round(w3, digits=5),
    " w2=", round(w2, digits=5),
    " w1=", round(w1, digits=5),
    " w0=", round(w0, digits=5),
    " error=", round(mean(abs(training_dataframe_parsed$e)), digits=5))
  )
  
  # test the created model comparing it againts test-dataframe (hipothesys=h vs reality=y)
  testing_dataframe_parsed$h = 
    w6*testing_dataframe_parsed$x6 + 
    w5*testing_dataframe_parsed$x5 + 
    w4*testing_dataframe_parsed$x4 + 
    w3*testing_dataframe_parsed$x3 + 
    w2*testing_dataframe_parsed$x2 + 
    w1*testing_dataframe_parsed$x1 + 
    w0*testing_dataframe_parsed$x0
  
  testing_dataframe_parsed$e = testing_dataframe_parsed$y - testing_dataframe_parsed$h
  
  # print MAE
  print(paste0(" Comparing calculated weights against real test value"))
  print(paste0(" MAE = ", mean(abs(testing_dataframe_parsed$e))))
  
  ObtainedMAEs = append(ObtainedMAEs, mean(abs(testing_dataframe_parsed$e)))
  
  invisible(readline(prompt = "Press a key to start next iteration"))
  
}

# variaton and average of obtained MAEs
var(ObtainedMAEs)
mean(ObtainedMAEs)


#--------------------------------------------lm() part ----------------------------------------------------------

# value for random data-partitioning
number = 112

# vector (like an array) for MAE comparisons
ObtainedMAEs_lm = vector()

for (i in 1:10) {
  
  # create partitions
  set.seed(number)
  number = number + 7
  #select 80% of the data
  training_data_in_indexes = createDataPartition(
    csv_dataframe$Overall.rank,
    times = 1,
    p = 0.8,
    list = FALSE
  )
  training_dataframe = csv_dataframe[training_data_in_indexes,] # get the 80% of the data for training
  testing_dataframe = csv_dataframe[-training_data_in_indexes,] # the rest (20%) for testing
  
  # adapt the format of the TRAINING dataframe
  training_dataframe_parsed = data.frame(
    x6 = training_dataframe$Social.support,
    x5 = training_dataframe$Healthy.life.expectancy,
    x4 = training_dataframe$Freedom.to.make.life.choices,
    x3 = training_dataframe$Generosity,
    x2 = training_dataframe$Perceptions.of.corruption,
    x1 = training_dataframe$GDP.per.capita,
    x0 = 1,
    y  = training_dataframe$Score,
    h  = 0,
    e  = 0
  )
  
  # adapt the format of the TEST dataframe
  testing_dataframe_parsed = data.frame(
    x6 = testing_dataframe$Social.support,
    x5 = testing_dataframe$Healthy.life.expectancy,
    x4 = testing_dataframe$Freedom.to.make.life.choices,
    x3 = testing_dataframe$Generosity,
    x2 = testing_dataframe$Perceptions.of.corruption,
    x1 = testing_dataframe$GDP.per.capita,
    x0 = 1,
    y  = testing_dataframe$Score,
    h  = 0,
    e  = 0
  )
  
  # use lm() function
  model = lm(y ~ x1+x2+x3+x4+x5+x6, data = training_dataframe_parsed)
  
  # add hypothesis and error values to the training dataframe
  training_dataframe_parsed$h = predict(model, training_dataframe_parsed)
  training_dataframe_parsed$e = training_dataframe_parsed$y - training_dataframe_parsed$h
  
  # plot the trained model
  print(ggplot(training_dataframe_parsed) + 
          geom_point(aes(x = x1, y = y, col = "Data")) + 
          geom_point(aes(x = x1, y = h, col = "Model hypothesis")) + 
          geom_line(aes(x = x1, y = h, col = "Model hypothesis")) + 
          labs(caption=paste0(
            "  w6=", round(coef(model)[[7]], digits=4),
            "  w5=", round(coef(model)[[6]], digits=4),
            "  w4=", round(coef(model)[[5]], digits=4),
            "  w3=", round(coef(model)[[4]], digits=4),
            "  w2=", round(coef(model)[[3]], digits=4),
            "  w1=", round(coef(model)[[2]], digits=4),
            "  w0=", round(coef(model)[[1]], digits=4)
          ))
  )
  
  # print information about the trained model
  print(paste0(" Calculated weights in training data and error"))
  print(paste0(
    " w6=", round(coef(model)[[7]], digits=5),
    " w5=", round(coef(model)[[6]], digits=5),
    " w4=", round(coef(model)[[5]], digits=5),
    " w3=", round(coef(model)[[4]], digits=5),
    " w2=", round(coef(model)[[3]], digits=5),
    " w1=", round(coef(model)[[2]], digits=5),
    " w0=", round(coef(model)[[1]], digits=5),
    " error=", round(mean(abs(training_dataframe_parsed$e)), digits=5))
  )
  
  # test the created model comparing it againts test-dataframe (hipothesys=h vs reality=y)
  testing_dataframe_parsed$h = 
    coef(model)[[7]]*testing_dataframe_parsed$x6 + 
    coef(model)[[6]]*testing_dataframe_parsed$x5 + 
    coef(model)[[5]]*testing_dataframe_parsed$x4 + 
    coef(model)[[4]]*testing_dataframe_parsed$x3 + 
    coef(model)[[3]]*testing_dataframe_parsed$x2 + 
    coef(model)[[2]]*testing_dataframe_parsed$x1 + 
    coef(model)[[1]]*testing_dataframe_parsed$x0
  
  testing_dataframe_parsed$e = testing_dataframe_parsed$y - testing_dataframe_parsed$h
  
  
  # print MAE
  print(paste0(" Comparing calculated weights against real test value"))
  print(paste0(" MAE = ", mean(abs(testing_dataframe_parsed$e))))
  

  # print R-squared values
  print(paste0(" Multiple R-squared = ", summary(model)$r.squared))
  print(paste0(" Adjusted R-squared = ", summary(model)$adj.r.squared))

  ObtainedMAEs_lm = append(ObtainedMAEs_lm, mean(abs(testing_dataframe_parsed$e)))
  
 
  invisible(readline(prompt = "Press a key to start next iteration"))

}

# average and variability in MAE
var(ObtainedMAEs_lm)
mean(ObtainedMAEs_lm)

#print the summary of the last iteration
summary(model)

