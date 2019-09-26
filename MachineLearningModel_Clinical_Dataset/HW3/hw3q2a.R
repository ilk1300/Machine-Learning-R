#setwd("/Users/vera/Desktop/BIOMI600/hw3/")
hemo_data <- read.csv("hemo.csv", header = TRUE)
library(class)

#get true classification according to 15% Fhbf criteria
tclass <- hemo_data$FHbF
for (i in 1:length(tclass)) {
  if (tclass[i] >= 15) {tclass[i] <- 1} #if final Hbf value is greater than or equal to 15, assign 1 as a responder
  else {tclass[i] <- 0}
}

#function to calculate accuracy, false negative, false positive
cal <- function(class_model, tclass) {
  #accuracy
  right_count=0
  for (i in 1:length(tclass)) {
    if(class_model[i] == tclass[i]) {right_count = right_count +1}
  } #if classification in model and true class is the same, that is a correct prediction
  accuracy <- right_count / length(tclass) #accuracy is equal to number of correct predictions divided by number of all observations(patients)
  
  #false negative
  tclass_r <- which(tclass==1) #extract patients who are responders accoring to true classification
  tclass_r_m_nr <- which(prediction[tclass_r]==0) #extract patients who are classified as non-responders by model but are actually responders
  false_negative <- length(tclass_r_m_nr) / length(tclass_r) #false negative is the ratio of these two
  
  #false positive
  tclass_nr <- which(tclass==0) #extract patients who are non-responders accoring to true classification
  tclass_nr_m_r <- which(prediction[tclass_nr]==1) #extract patients who are classified as responders by model but are actually non-responders
  false_positive <- length(tclass_nr_m_r) / length(tclass_nr) #false positive is the ratio of these two
  
  #make a return list of accuracy, false negative, false positive
  out_list <- list("Accuracy"=accuracy, "FalseNegative"=false_negative, "FalsePositive"=false_positive)
  return(out_list)
}

#use knn.cv function to classify
model_result_hemo <- matrix(data = NA, nrow = 23, ncol = 3) #make a 23x3 matrix to hold accuracy, false negative and false positive
rownames(model_result_hemo) <- c(colnames(hemo_data)[1:23])
colnames(model_result_hemo) <- c('Accuracy','FalseNegative','FalsePositive') #set matrix row names and column names
for (i in 1:23) { 
  para <- hemo_data[,i] #since hemo_data has parameters in columns, get each of 23 columns as training data
  prediction <- knn.cv(para, tclass, k=7, use.all = TRUE) #one column input as training data, tclass as true classifications, k=7 stands for 7 nearest neighbour
  out <- cal(prediction, tclass) #take output from knn.cv as input for calculate accuracy, fn, fp
  model_result_hemo[i,1] <- out$Accuracy #add values to result matrix
  model_result_hemo[i,2] <- out$FalseNegative
  model_result_hemo[i,3] <- out$FalsePositive
}

mean_aacu_hemo <- mean(model_result_hemo[,1]) #mean for accuracy
mean_fn_hemo <- mean(model_result_hemo[,2]) #mean for false nagetive
mean_fp_hemo <- mean(model_result_hemo[,3]) #mean for false positive
print(model_result_hemo)