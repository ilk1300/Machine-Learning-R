#setwd("/Users/vera/Desktop/BIOMI600/hw3/")
source("KLExpand.R", local = TRUE)

hemo_data <- data.matrix(read.csv("hemo.csv", header = TRUE)) #read in hemo.csv dataset as matrix
hemo_data <- hemo_data[,(1:23)] #exclude Fhbf column and classification column
hemo_data <- scale(hemo_data, center = TRUE, scale = TRUE) #use scale function to normalize data to have zero mean and standard deviation equals to 1

KLresult_hemo <- KLExpand(hemo_data) #use KLExpand functionon on normalized dataset
print(KLresult_hemo$EigenVectors) #only print out eigen vectors to show the dimensionality of the feature space
