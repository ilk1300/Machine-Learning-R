#setwd("/Users/vera/Desktop/BIOMI600/hw3/")
source("KLExpand.R", local = TRUE)

#input and normalize data
hemo_data <- data.matrix(read.csv("hemo.csv", header = TRUE)) #read in hemo.csv dataset as matrix
hemo_data <- hemo_data[,(1:23)] #exclude Fhbf column and classification column
hemo_data <- scale(hemo_data, center = TRUE, scale = TRUE) #use scale function to normalize data to have zero mean and standard deviation equals to 1

#apply KLExpand function
KL_result <- KLExpand(hemo_data) #use KLExpand functionon on normalized dataset
EV_hemo <- KL_result$EigenValue #extract eigen values from KLExpand function output

#get accumulative sum of mu values
EV_contribution_percent <- c() #make an empty vector to hold accumulative sum of mu values
sum_mu <- 0 #set sum of mu values equal to zero before calculating
for (i in EV_hemo) {
  con_per <- i/sum(EV_hemo) #use for loop to calculate mu value=eigen value i/sum of all eigen values
  sum_mu <- sum_mu + con_per #do accumulative sum for mu values
  EV_contribution_percent <- c(EV_contribution_percent, sum_mu) #append accumulative sum to vector
}

#use correlation function to rank variable
variable_rank <- cor(hemo_data, KL_result$FeatureVectors[1,]) #find correlation between feature value and variable. First input is a 23x72 original normalized hemo data set, since my feature vector matrix is 23x72, so second input is first row of feature vector matrix 
variable_rank <- variable_rank^2 #square the output of correlation function to get rid of negative values
sorted_variable_rank <- sort(variable_rank[,1], decreasing = TRUE) #sort the squared vector in decreasing order with labels

#my threshold is 98%, so select and print out first 16 parameters, the reason of my dimensionality reduction is explained in the report
print(list("variable names and correlation in descending order"=sorted_variable_rank, "variables I select"=sorted_variable_rank[1:16]))
