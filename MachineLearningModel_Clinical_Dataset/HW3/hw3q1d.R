#setwd("/Users/vera/Desktop/BIOMI600/hw3/")
source("KLExpand.R", local = TRUE)

leu_train_data <- data.matrix(read.csv("golubtrain.csv", header = TRUE)) #read in golub training data set as matrix
leu_test_data <- data.matrix(read.csv("golubtest.csv", header = TRUE)) #read in golub testing data set as matri
leu_data <- rbind(leu_train_data, leu_test_data) #combine traning data and test data into one data matrix
leu_data <- leu_data[,(1:2000)] #get rid of last classification column
leu_data <- scale(leu_data, center = TRUE, scale = TRUE) #normalize data by using scale function to have zero mean and standard deviation equals to one
KLresult_leu <- KLExpand(leu_data) #apply KL expansion function on normalized data set
print(KLresult_leu$EigenVectors) #only print out eigen vectors to show the dimensionality of the feature space
print("The dimensioni of feature space is:")
print(dim(KLresult_leu$EigenVectors))