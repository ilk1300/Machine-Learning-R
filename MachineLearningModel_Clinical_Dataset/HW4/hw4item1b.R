#setwd("/Users/vera/Desktop/BIOMI600/hw4/")
source("hemoprep.R", local = TRUE)
library(neuralnet)

# feed training data into model
error_vector_bp <- c()
for (i in 1:10) {
  nn_bp <- neuralnet(form, data=training_usepre, hidden=c(8,4), rep=30, learningrate = 0.01, algorithm = "backprop", err.fct = "ce", act.fct = "logistic", linear.output = FALSE)
  errors_bp <- nn_bp$result.matrix[1,]
  plot(c(1:30), errors_bp, xlab = 'epoch', ylab = 'error', type = 'l')
  mean_error_train_bp <- mean(errors_bp)
  error_vector_bp <- c(error_vector_bp, mean_error_train_bp)
}

# use model to predict classification for testing data
test_bp <- compute(nn_bp, testing_usepre)
test_bp_predictions <- test_bp$net.result
test_bp_predictions <- ifelse(test_bp_predictions[,1] >=0.5,1,0)
error_test_bp <- length(which(testing_tclass!=test_bp_predictions)) / nrow(testing)

