#setwd("/Users/vera/Desktop/BIOMI600/hw4/")
source("hemoprep.R", local = TRUE)
library(neuralnet)

# feed training data into model
error_vector <- c()  # create an empty vector to hold errors for 10 runs
for (i in 1:10) {      # run model 10 times
  nn_deltarule <- neuralnet(form, data=training_usepre, hidden=8, rep=30, learningrate=0.01, algorithm = "backprop", err.fct = "ce", act.fct = "logistic", linear.output = FALSE)
  errors <- nn_deltarule$result.matrix[1,] # get errors for each epoch
  plot(c(1:30), errors, xlab = 'epoch', ylab = 'error', type = 'l')  # plot error over epoch
  mean_error_train <- mean(errors)  # overall error is the mean of all errors for 30 epochs
  error_vector <- c(error_vector, mean_error_train)
}

# use model to predict classification for testing data
test_deltarule <- compute(nn_deltarule, testing_usepre)  # use model to predict testing data
test_deltarule_predictions <- test_deltarule$net.result  # extract prediction values
test_deltarule_predictions <- ifelse(test_deltarule_predictions[,1] >=0.5,1,0)  #  round up prediction to be either 0 or 1
error_test <- length(which(testing_tclass!=test_deltarule_predictions)) / nrow(testing) # get testing error rate

