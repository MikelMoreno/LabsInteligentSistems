
# Setup the environment
rm(list=ls())
cat("\014")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
dir()

# Imports
library(ggplot2)
library(caret)


################################################
# Read csv file and store in data in dataframe #
################################################

  dataframe_from_csv = read.csv(file.path("../data/2020 - 2019 world happiness.csv"))

  # check out the structure of...

  # str(dataframe_from_csv)
  # str(dataframe_from_csv$Overall.rank)
  # str(dataframe_from_csv$Country.or.region)
  # str(dataframe_from_csv$Score)
  # str(dataframe_from_csv$GDP.per.capita)
  # str(dataframe_from_csv$Social.support)
  # str(dataframe_from_csv$Healthy.life.expectancy)
  # str(dataframe_from_csv$Freedom.to.make.life.choices)
  # str(dataframe_from_csv$Generosity)
  # str(dataframe_from_csv$Perceptions.of.corruption)

  # output as expected for these two xD
  ggplot(dataframe_from_csv, aes(x = Overall.rank, y = Score)) + geom_point()
  ggplot(dataframe_from_csv, aes(x = Score, y = Score)) + geom_point()

  # no luck, would've been nice to find some relation...
  ggplot(dataframe_from_csv, aes(x = Country.or.region, y = Score)) + geom_point()

  # these ones look nice
  ggplot(dataframe_from_csv, aes(x = GDP.per.capita, y = Score)) + geom_point()
  ggplot(dataframe_from_csv, aes(x = Social.support, y = Score)) + geom_point()
  ggplot(dataframe_from_csv, aes(x = Healthy.life.expectancy, y = Score)) + geom_point()

  # these ones not so much...
  ggplot(dataframe_from_csv, aes(x = Freedom.to.make.life.choices, y = Score)) + geom_point()
  ggplot(dataframe_from_csv, aes(x = Generosity, y = Score)) + geom_point()
  ggplot(dataframe_from_csv, aes(x = Perceptions.of.corruption, y = Score)) + geom_point()



#########################################################################################
# Perform linear regression to predict "Score" given only one factor to start things up #
#########################################################################################

  #################
  # non-lm() part #
  #################

    # initialize dataframe
    initial_set = data.frame(
      x1 = dataframe_from_csv$GDP.per.capita,
      x0 = 1,
      y  = dataframe_from_csv$Score,
      h  = 0,
      e  = 0
    )

    # initialize weights for single input variable
    w0 = 1
    w1 = 1

    # set hypothesis and error formulas
    initial_set$h = w1*initial_set$x1 + w0*initial_set$x0
    initial_set$e = initial_set$y - initial_set$h # el error de lo que me saca mi funcion vs lo que realmente hay

    iterations = 250 # iterations at 2000 gives pretty similar result to lm() later below...
    alpha = 0.01
    
    #ajusto los pesos, los divido entre nrow para ajustarlo a la medida del w (puede que sea un porcentaje)
    for (i in 1:iterations) {
      w1 = w1 + alpha * sum(initial_set$e * initial_set$x1) / nrow(initial_set)
      w0 = w0 + alpha * sum(initial_set$e * initial_set$x0) / nrow(initial_set)
      initial_set$h = w1*initial_set$x1 + w0*initial_set$x0
      initial_set$e = initial_set$y - initial_set$h
      print(paste0(
        i,
        " w1=", round(w1, digits=5),
        " w0=", round(w0, digits=5),
        " error=", round(mean(abs(initial_set$e)),
        digits=5))
      )
    }

    # plot Score and hypothesis
    ggplot(initial_set) + 
      geom_point(aes(x = x1, y = y, col = "Data")) + 
      geom_point(aes(x = x1, y = h, col = "Hypothesis")) + 
      geom_line(aes(x = x1, y = h, col = "Hypothesis")) +
      labs(caption=paste0("iterations --> ", i, "  w1=", round(w1, digits=3),"  w0=", round(w0, digits=3)))

  ###########
  # lm() part
  ###########

    # initialize dataframe again
    model_set = data.frame(
      x1 = dataframe_from_csv$GDP.per.capita,
      x0 = 1,
      y  = dataframe_from_csv$Score,
      h  = 0,
      e  = 0
    )

    # do whole thing in one operation
    model = lm(y ~ x1, data = model_set)
    model_set$h = predict(model, model_set)
    print(model)
    
    # as expected, R-squared values are not the best here, this will improve later with more features
    summary(model)
    
    # reminder4me in the future:
    #
    #   R-squared is a statistical metric that is used to measure how 
    #   much of the variation in outcome can be explained by the variation 
    #   in the independent variables, basically how much of/well the output 
    #   (in this case Score) can be "explained"/plotted as a function of 
    #   the given input variable/s (in this case, so far, GDP per capita)

    # comparison of two outputs, manual one and lm()
    ggplot(initial_set) + 
      geom_point(aes(x = x1, y = y, col = "Data")) + 
      geom_point(aes(x = x1, y = h, col = "Manual hypothesis")) + 
      geom_line(aes(x = x1, y = h, col = "Manual hypothesis")) + 
      geom_line(aes(x = model_set$x1, y = model_set$h, col = "Model hypothesis")) + 
      labs(caption=paste0("model_w1=", round(coef(model)[[2]], digits=3),
                        "  model_w0=", round(coef(model)[[1]], digits=3),
                  "   ||   iterations --> ", i,
                        "  w1=", round(w1, digits=3),
                        "  w0=", round(w0, digits=3)
      ))
    
    # Even for a relatively low number of iterations, the "manual" option doesn't
    # differ that much from what the lm() function outputs, which is a good sign.



########################################################################################
# Repeat 10 times division of data, training with 6 factors and test it and measure it #
########################################################################################

  #################
  # non-lm() part #
  #################

    # value for random data-partitioning
    number = 4321
    
    # vector for MAE comparisons later on...
    vector_mae_results = vector()

    for (i in 1:10) {

      # create partitions
      set.seed(number)
      number = number + 5
      training_data_in_indexes = createDataPartition(
        dataframe_from_csv$Overall.rank,
        times = 1,
        p = 0.8,
        list = FALSE
      )
      training_dataframe = dataframe_from_csv[training_data_in_indexes,]
      testing_dataframe = dataframe_from_csv[-training_data_in_indexes,]
  
      # parse train dataframe into pretty format
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
  
      # parse test dataframe into pretty format
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
      
      # initialize weights for six input variables
      w0 = 0.5
      w1 = 0.5
      w2 = 0.5
      w3 = 0.5
      w4 = 0.5
      w5 = 0.5
      w6 = 0.5
      
      # set hypothesis and error formulas
      training_dataframe_parsed$h = 
        w6*training_dataframe_parsed$x6 + 
        w5*training_dataframe_parsed$x5 + 
        w4*training_dataframe_parsed$x4 + 
        w3*training_dataframe_parsed$x3 + 
        w2*training_dataframe_parsed$x2 + 
        w1*training_dataframe_parsed$x1 + 
        w0*training_dataframe_parsed$x0
  
      training_dataframe_parsed$e = training_dataframe_parsed$y - training_dataframe_parsed$h
  
      # loop constraints
      iterations = 1000
      alpha = 0.0001
      
      for (j in 1:iterations) {
        w6 = w6 + alpha * sum(training_dataframe_parsed$e * training_dataframe_parsed$x6)# / nrow(training_dataframe_parsed)
        w5 = w5 + alpha * sum(training_dataframe_parsed$e * training_dataframe_parsed$x5)# / nrow(training_dataframe_parsed)
        w4 = w4 + alpha * sum(training_dataframe_parsed$e * training_dataframe_parsed$x4)# / nrow(training_dataframe_parsed)
        w3 = w3 + alpha * sum(training_dataframe_parsed$e * training_dataframe_parsed$x3)# / nrow(training_dataframe_parsed)
        w2 = w2 + alpha * sum(training_dataframe_parsed$e * training_dataframe_parsed$x2)# / nrow(training_dataframe_parsed)
        w1 = w1 + alpha * sum(training_dataframe_parsed$e * training_dataframe_parsed$x1)# / nrow(training_dataframe_parsed)
        w0 = w0 + alpha * sum(training_dataframe_parsed$e * training_dataframe_parsed$x0)# / nrow(training_dataframe_parsed)
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
  
      # plot trained model
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
  
      # print info of trained model
      print(paste0(" Calculated weights in training data and error"))
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
  
      # test model (weights, really) againts test dataframe
      testing_dataframe_parsed$h = 
        w6*testing_dataframe_parsed$x6 + 
        w5*testing_dataframe_parsed$x5 + 
        w4*testing_dataframe_parsed$x4 + 
        w3*testing_dataframe_parsed$x3 + 
        w2*testing_dataframe_parsed$x2 + 
        w1*testing_dataframe_parsed$x1 + 
        w0*testing_dataframe_parsed$x0
  
      testing_dataframe_parsed$e = testing_dataframe_parsed$y - testing_dataframe_parsed$h
  
      # print results (Mean Absolute Error)
      print(paste0(" Test calculated weights against other partition, test data"))
      print(paste0(" MAE -> ", mean(abs(testing_dataframe_parsed$e))))
      
      vector_mae_results = append(vector_mae_results, mean(abs(testing_dataframe_parsed$e)))
      
      invisible(readline(prompt = "Press a key to see next iteration's results"))
      
    }
    
    # some interesting metrics
    var(vector_mae_results)
    mean(vector_mae_results)
    
    # The minimum MAE obtained for the manual method (provided the 
    # "number = 4321" is not changed) is of 0.274680833992387, which indicates 
    # very little variation between the Score values of the test dataset and the 
    # predictions even if that data was not included in the training dataset, 
    # showing that training was somewhat successful (other MAE double that 
    # of 0.2746 though). We can also see by the plotted values that we are going to
    # have some overfitting issues, but testing data shows that error is going to be
    # pretty acceptable, all things considered. Error during training ranges between 
    # 0.4 and 0.45 and the MAE for testing goes as high as 0.503 (significantly 
    # higher than the lm() case afterwards).

  #############
  # lm() part #
  #############
    
    # value for random data-partitioning
    number = 5678
    
    # vector for MAE comparisons later on...
    vector_mae_results_lm = vector()
    
    for (i in 1:10) {
      
      # create partitions
      set.seed(number)
      number = number + 5
      training_data_in_indexes = createDataPartition(
        dataframe_from_csv$Overall.rank,
        times = 1,
        p = 0.8,
        list = FALSE
      )
      training_dataframe = dataframe_from_csv[training_data_in_indexes,]
      testing_dataframe = dataframe_from_csv[-training_data_in_indexes,]
      
      # parse train dataframe into pretty format
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
      
      # parse test dataframe into pretty format
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
      
      # create model out of training dataset
      model = lm(y ~ x1+x2+x3+x4+x5+x6, data = training_dataframe_parsed)
      
      # assign hypothesis and error values according to model's predictions
      training_dataframe_parsed$h = predict(model, training_dataframe_parsed)
      training_dataframe_parsed$e = training_dataframe_parsed$y - training_dataframe_parsed$h
      
      # plot trained model
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
      
      # print info of trained model
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
      
      # test model (weights, really) againts test dataframe
      testing_dataframe_parsed$h = 
        coef(model)[[7]]*testing_dataframe_parsed$x6 + 
        coef(model)[[6]]*testing_dataframe_parsed$x5 + 
        coef(model)[[5]]*testing_dataframe_parsed$x4 + 
        coef(model)[[4]]*testing_dataframe_parsed$x3 + 
        coef(model)[[3]]*testing_dataframe_parsed$x2 + 
        coef(model)[[2]]*testing_dataframe_parsed$x1 + 
        coef(model)[[1]]*testing_dataframe_parsed$x0
      
      testing_dataframe_parsed$e = testing_dataframe_parsed$y - testing_dataframe_parsed$h
      
      # print results (Mean Absolute Error)
      print(paste0(" Test calculated weights against other partition, test data"))
      print(paste0(" MAE -> ", mean(abs(testing_dataframe_parsed$e))))
      
      # print out R-squared values of model
      print(paste0(" R-squared values for model"))
      print(paste0(" Multiple R-squared -> ", summary(model)$r.squared))
      print(paste0(" Adjusted R-squared -> ", summary(model)$adj.r.squared))
      
      vector_mae_results_lm = append(vector_mae_results_lm, mean(abs(testing_dataframe_parsed$e)))
      
      invisible(readline(prompt = "Press a key to see next iteration's results"))
      
    }
    
    # some interesting metrics
    var(vector_mae_results_lm)
    mean(vector_mae_results_lm)

    # For the lm() case, we see similar results. We get MAE results of as low as low as
    # 0.3172399 (even though we do get a higher average MAE for the lm() case) and 
    # pretty reasonable R-squared values all throughout the 10 iterations (they stay 
    # fairly consistent within the high-70's-low-80's values). Error during the 
    # training phase is a bit more concentrated and doesn't fluctuate as much as in the 
    # manual process, seeing error values between 0.39 and 0.44. Variability in MAE is 
    # also a bit higher than in the manual process, but only marginally so (0.0035306 
    # manually vs 0.003943972 in lm()) so we can safely attribute this to the difference 
    # in data partitioning and not to just worse prediction capabilities.

    