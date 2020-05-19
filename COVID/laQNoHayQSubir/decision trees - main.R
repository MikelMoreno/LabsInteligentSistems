
# Setup the environment
rm(list=ls())
cat("\014")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
dir()

# Imports and functions
library(lattice)
library(ggplot2)
library(caret)
library(iml)
library(rpart)
library(rpart.plot)

library(dplyr) # mmmmh
library(mlbench) # mmmmh
library(rattle) # mmmmh


# Define the entropy as a function able to receive any set of partitions
entropy = function(s) {
  p = (s/sum(s))
  logp = log2(p)
  logp[logp==-Inf] = 0
  return(sum(-p * logp))
}

# Defining information gain as a function able to receive any set of partitions
gain = function(attr, target) {
  values = table(attr, target)
  ps = rowSums(values) / sum(values)
  es = apply(values, 1, entropy)
  gain = entropy(colSums(values)) - sum(ps * es)
  print(values) # let's see confusion matrix, just for kicks...
  return(gain)
}


################################################
# Read tab file and store in data in dataframe #
################################################

  dataframe_from_table = read.table(
    file = file.path("../data/2020 covid-19-recommendations.tab"),
    header = TRUE
  )

  # Check gain of different variables and distribution of values, sorted from 
  # best to worst, see how tree turns out later and there's some correlation
  # between information gain and first attributes evaluated (expected answer: yes).
  #
  # In essence, we expect to see FEVER, COUGH, BREATHING_DIFFICULTY, NASAL_CONGESTION
  # and BLOOD_EXPECTORATION early at the top of the tree.

  gain(dataframe_from_table$FEVER, dataframe_from_table$TARGET)
  gain(dataframe_from_table$COUGH, dataframe_from_table$TARGET)
  gain(dataframe_from_table$BREATHING_DIFFICULTY, dataframe_from_table$TARGET)
  gain(dataframe_from_table$NASAL_CONGESTION, dataframe_from_table$TARGET)
  gain(dataframe_from_table$BLOOD_EXPECTORATION, dataframe_from_table$TARGET)
  gain(dataframe_from_table$CONJUNCTIVITIS, dataframe_from_table$TARGET)
  gain(dataframe_from_table$NAUSEAS, dataframe_from_table$TARGET)
  gain(dataframe_from_table$FATIGUE, dataframe_from_table$TARGET)
  gain(dataframe_from_table$DIARRHOEA, dataframe_from_table$TARGET)
  gain(dataframe_from_table$HEADACHE, dataframe_from_table$TARGET)
  gain(dataframe_from_table$MUSCLE_PAIN, dataframe_from_table$TARGET)
  gain(dataframe_from_table$SPUTUM, dataframe_from_table$TARGET)
  gain(dataframe_from_table$SORE_THROAT, dataframe_from_table$TARGET)
  gain(dataframe_from_table$SHIVERS, dataframe_from_table$TARGET)


#######################################################################################
# Repeat 10 times division of data, training with all factors and test and measure it #
#######################################################################################

  # vector for later metrics
  vector_acc_results = vector()

  # value for random data-partitioning
  number = 4321

  for (i in 1:10) {

    # create partitions
    set.seed(number)
    number = number + 5
    training_data_in_indexes = createDataPartition(
      dataframe_from_table$FEVER,
      times = 1,
      p = 0.8,
      list = FALSE
    )
    training_dataframe = dataframe_from_table[training_data_in_indexes,]
    testing_dataframe = dataframe_from_table[-training_data_in_indexes,]

    # create model out of training data (dot implies "use all columns except TARGET")
    model_training = rpart(TARGET ~ ., training_dataframe, method = "class")
    print(model_training)

    # test model, get confusion matrix for TARGET (go-to-hospital/stay-at-home) and print accuracy
    predictions = predict(model_training, testing_dataframe, type = "class")
    confusionmatrix = table(predictions, testing_dataframe$TARGET)
    print(confusionmatrix)
    print(paste("Measured accuracy of", sum(diag(confusionmatrix)) / sum(confusionmatrix)))

    vector_acc_results = append(vector_acc_results, sum(diag(confusionmatrix)) / sum(confusionmatrix))

    # reminder4meinthefuture:
    #                                  (PREDICTED)
    #                            go-to-hospital    stay-at-home
    #  (ACTUAL)  go-to-hospital           A               B
    #            stay-at-home             C               D
    #
    # Reading from ACTUAL to PREDICTED, of all the go-to-hospital cases in the
    # testing_dataframe (A + B), the model said that #A people should go to the 
    # hospital and #B should stay at home. Same for stay-at-home
    #
    # Thus, to print predictions' accuracy, we go like:
    #
    #    accuracy = (A + D) / (A + D + C + B)
    #
    #       such that, in more scientific terms,
    #
    #         Correct predictions
    #             A -> True Negative/Positive (depending on how you wanna look at it, what your "good" target is...)
    #             D -> True Positive/Negative
    #
    #         Incorrect predictions
    #             B -> False Positive/Negative
    #             C -> False Negative/Positive
    #
    # So, to sum up:
    #                                  (PREDICTED)
    #                               FALSE            TRUE
    #  (ACTUAL)  FALSE       TrueNegative   FalsePositive
    #            TRUE       FalseNegative    TruePositive


    # plot tree (extra = 106 for binary stuff, (FEATURE) yes/no)
    rpart.plot(model_training, extra = 106)

    invisible(readline(prompt = "Press [enter] to see next iteration's results"))
  }

  # some interesting metrics
  var(vector_acc_results) # that's pretty damn good
  mean(vector_acc_results) # also nice

  # Commentary
  #
  # Accuracy in predictions stays high and consistent (low-mid nineties) throughout all 
  # 10 iterations. As seen with tha information gain values for each feature earlier in the script,
  # FEVER, COUGH, BREATHING_DIFFICULTY and BLOOD_EXPECTORATION are seen in all 10 trees.
  # NASAL_CONGESTION makes it into the mix on few iterations, just 1, 4 and 8. Also, tree structure stays
  # fairly similar across iterations, with FEVER, COUGH and BREATHING_DIFFICULTY at the early "left"
  # stages of the decision tree. BLOOD_EXPECTORATION appears to be a deciding factor on 7 iterations 
  # provided that you tested negative for FEVER, advising you to go to the hospital if you are 
  # coughing up blood (which is, yknow, obvious, so thank God... I mean, Machine Learning for that xD).
  # For the leftmost part of the decision tree, we see that DIARRHOEA, CONJUNCTIVITIS, NAUSEAS and 
  # HEADACHE come in and out of the rotation of important symptoms, all of them telling you to 
  # "go-to-hospital" if you have one of them, "stay-at-home" otherwise. A bit more of variability
  # here in the bottom-left part of the tree than one might want to see, maybe the decision tree with
  # all data sheds some light into how to interpret these symptoms. Also, interestingly, 
  # all roots of the trees start out by telling you to "stay-at-home".
  #
  # To sum up all these decision trees, if you dont have a fever but you are coughing up blood,
  # "go-to-hospital". If you aren't coughing blood, "stay-at-home". If you do have a fever and you
  # have (any of all those bottom-left symptoms), "go-to-hospital". If you have a fever but do not have
  # any other symptom, "stay-at-home".


########################################################################################
# Build decision tree with all data, plot obtained tree, top-5 most important symptoms #
########################################################################################

  # build decision tree with all data, test it and plot it
  model_alldata = rpart(TARGET ~ ., dataframe_from_table, method = "class")
  print(model_alldata)
  predictions = predict(model_alldata, dataframe_from_table, type = "class")
  confusionmatrix = table(predictions, dataframe_from_table$TARGET)
  print(confusionmatrix)
  print(paste("Measured accuracy of", sum(diag(confusionmatrix)) / sum(confusionmatrix)))
  rpart.plot(model_alldata, extra = 106)

  # Commentary
  #
  # We observe here a pretty similar tree-structure to previous train-testing cases,
  # with FEVER, COUGH, BLOOD_EXPECTORATION, BREATHING_DIFFICULTY and DIARRHOEA making it
  # to the final, hopefully-definitive decision tree. All these features come up in previous
  # decision trees, DIARRHOEA being the least common fo them all. However, all in all, final
  # result is consistent with what we saw in train-test iterations beforehand, with an
  # accuracy metric higher than the average than the iterations'. To sum up the decision tree,
  # if you dont have a fever but you are coughing up blood, "go-to-hospital". If you aren't 
  # coughing blood, "stay-at-home". If you do have a fever and you have a cough or breathing
  # difficulty or diarrhoea, "go-to-hospital". If you have a fever but do not have any other
  # symptom, "stay-at-home" (summary coincides with previous summary, which is good).


  # top-5 most important symptoms
  print(varImp(model_alldata))

  # for the life of me I couldn't figure out how to sort this dataframe, here's the 
  # manually-sorted output (pitty only randomForests take the varImpPlot function):
  #
  #     FEVER                1402.65025
  #     COUGH                1222.85731
  #     BREATHING_DIFFICULTY  570.77597
  #     NASAL_CONGESTION      449.89276
  #     BLOOD_EXPECTORATION   347.09503
  #     CONJUNCTIVITIS        120.45981
  #     NAUSEAS                56.34338
  #     DIARRHOEA              24.35065
  #     HEADACHE               21.41924
  #     FATIGUE                 0.00000
  #     SPUTUM                  0.00000
  #     MUSCLE_PAIN             0.00000
  #     SORE_THROAT             0.00000
  #     SHIVERS                 0.00000

  # Commentary
  #
  # Curiously, BREATHING_DIFFICULTY appears to be more important than BLOOD_EXPECTORATION.
  # Same goes for CONJUNCTIVITIS and NAUSEAS against DIARRHOEA, but in this case both CONJUNCTIVITIS
  # and NAUSEAS do not make it into the final decision tree even tho they appear to be more
  # significant according to varImp(). NASAL_CONGESTION doesn't make it either, and it is above both
  # BLOOD_EXPECTORATION and DIARRHOEA (which are in the tree) and other apparently-important features
  # like CONJUNCTIVITIS and NAUSEAS. All this seems pretty interesting...

  # one last thing
  summary(model_alldata)
  
  # I do not know how in heaven's name the pretty functions in your "05 Entendiendo el modelo de 
  # Diabetes.R" don't work for me at all. I've installed and uninstalled the necessary packages about a
  # thousand times and no luck, so no variable correlation for this assignment I'm afraid. I was hoping to
  # ask you about it on Friday but we had class at 11.00, so maybe next time hopefully :).
