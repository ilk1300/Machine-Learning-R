## This file aims to do prep work for golbub dataset 
## as input into neuralnet function, 

# setwd("/Users/vera/Desktop/BIOMI600/hw4/")

# read in file
golub1 <- read.csv("reduced_golubtrain.csv", header = TRUE)
golub1 <- golub1[,-1] # get rid of first column, which is just order number
golub2 <- read.csv("reduced_golubtest.csv", header = TRUE)
golub2 <- golub2[,-1]
golub_whole <- rbind(golub1, golub2) # combine training and testing
golub_whole <- as.matrix(golub_whole) # change data into matrix

# normalize data between 0 and 1
golub_whole[,1:80] <- as.matrix(apply(golub_whole[,1:80], 2, function(x)(x-min(x))/(max(x)-min(x)))) # normalize data between 0 and 1, but keep the last classification column
golub_whole[,81] <- as.numeric(golub_whole[,81])-1
