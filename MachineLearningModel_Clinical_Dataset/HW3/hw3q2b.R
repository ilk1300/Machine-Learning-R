#setwd("/Users/vera/Desktop/BIOMI600/hw3/")
leu_train <- read.csv("golubtrain.csv", header = TRUE)
leu_test <- read.csv("golubtest.csv", header = TRUE)
library(class)

#function to calculate accuracy, false negative, false positive
cal <- function(class_model, tclass) {
  #accuracy
  right_count=0
  for (i in 1:length(tclass)) {
    if(class_model[i] == tclass[i]) {right_count = right_count +1}
  } #if classification in model and true class is the same, that is a correct prediction
  accuracy <- right_count / length(tclass) #accuracy is equal to number of correct predictions divided by number of all observations(patients)
  
  #false negative
  tclass_r <- which(tclass==1) #extract patients who are classified as 1 accoring to true classification
  tclass_r_m_nr <- which(prediction[tclass_r]==2) #extract patients who are classified as 2 by model but are actually 1
  false_negative <- length(tclass_r_m_nr) / length(tclass_r) #false negative is the ratio of these two
  
  #false positive
  tclass_nr <- which(tclass==2) #extract patients who are classified as 2 accoring to true classification
  tclass_nr_m_r <- which(prediction[tclass_nr]==1) #extract patients who are classified as 1 by model but are actually 2
  false_positive <- length(tclass_nr_m_r) / length(tclass_nr) #false positive is the ratio of these two
  
  #make a return list of accuracy, false negative, false positive
  out_list <- list("Accuracy"=accuracy, "FalseNegative"=false_negative, "FalsePositive"=false_positive)
  return(out_list)
}

prediction_leu <- knn(train = leu_train, test = leu_test, cl = leu_train$class, k = 7, use.all = TRUE)

#use prediction_leu as input to calculate accuracy, false negative and false positive, and print out results
model_result_leu <- cal(prediction_leu, leu_test$class) #test output of knn function based on test data true classification
print(list("Accuracy"=model_result_leu$Accuracy, "FalseNegative"=model_result_leu$FalseNegative, "FalsePositive"=model_result_leu$FalsePositive))
