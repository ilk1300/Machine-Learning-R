#setwd("/Users/vera/Desktop/BIOMI600/hw3/")
source("KLExpand.R", local = TRUE)

#input data, combine training and testing, nomarlize data
leu_train_data <- data.matrix(read.csv("golubtrain.csv", header = TRUE)) #read in golub training data set as matrix
leu_test_data <- data.matrix(read.csv("golubtest.csv", header = TRUE)) #read in golub testing data set as matri
leu_data <- rbind(leu_train_data, leu_test_data) #combine traning data and test data into one data matrix
leu_data <- leu_data[,(1:2000)] #get rid of last classification column
leu_data <- scale(leu_data, center = TRUE, scale = TRUE) #normalize data by using scale function to have zero mean and standard deviation equals to one

#apply KLExpand functoin
KLresult_leu <- KLExpand(leu_data) #apply KL expansion function on normalized data set
EV_leu <- KLresult_leu$EigenValues #extract eigen values from output of KLExpand function

#get accumulative sum of mu values
accu_mu <- c() #make an empty vector to hold accumulative sum of mu values
sum_mu <- 0 #set sum of mu values equal to zero before calculating
for (i in EV_leu) {
  con_per <- i/sum(EV_leu) #use for loop to calculate mu value=eigen value i/sum of all eigen values
  sum_mu <- sum_mu + con_per #do accumulative sum for mu values
  accu_mu <- c(accu_mu, sum_mu) #append accumulative sum to vector
}

#use correlation function to rank variable
variable_rank_leu <- cor(leu_data, KLresult_leu$FeatureVectors[1,]) #find correlation between feature value and variable. First input is a 72x2000 original normalized golub data set, since my feature vector matrix is 2000x72, so second input is first row of feature vector matrix 
variable_rank_leu <- variable_rank_leu^2 #square the output of correlation function to get rid of negative values
sorted_variable_rank_leu <- sort(variable_rank_leu[,1], decreasing = TRUE) #sort the squared vector in decreasing order with labels

#my threshold is 90%, so select and print out first 41 parameters, the reason of my dimensionality reduction is explained in the report
print(list("variable names and correlation in descending order"=sorted_variable_rank_leu, "variables I select"=sorted_variable_rank_leu[1:41]))