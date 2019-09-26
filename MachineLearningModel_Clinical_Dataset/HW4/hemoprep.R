## This file aims to do prep work for hemo data as input into neuralnet function, 

#setwd("/Users/vera/Desktop/BIOMI600/hw4/")
hemo_data <- read.csv("hemo.csv", header = TRUE)

# get true classification according to 15% FHbF criteria
tclass <- hemo_data$FHbF 
for (i in 1:length(tclass)) {
  if (tclass[i] >= 15) {tclass[i] <- 1} # if final Hbf value is greater than or equal to 15, assign 1 as a responder
  else {tclass[i] <- 0} # if lower than 15, assign 0 as non-responder
}
# replace Class column with true classification
hemo_data$Class <- tclass

# normalize data between range 0 and 1 by each column
hemo_norm_data <- as.data.frame(apply(hemo_data, 2, function(x)(x-min(x))/(max(x)-min(x))))

# split hemo dataset into training and testing by ratio 0.7/0.3
set.seed(123)
split_index <- sample(1:nrow(hemo_norm_data), round(0.7*nrow(hemo_norm_data)))
training <- hemo_norm_data[split_index,]
testing <- hemo_norm_data[-split_index,]
training_tclass <- tclass[split_index]
testing_tclass <- tclass[-split_index]

# get 16 predictors that will be used to classify
notuse_train <- c(3,4,9,14,21,22,23,24) # index numbers of predictors that won't be used in builing the model
notuse_test <- c(3,4,9,14,21,22,23,24,25) # last (25th) column is also excluded because when feed testing data into model, groundtruth cannot be included
preuse <- colnames(hemo_norm_data)[-notuse_test] # get the predictors that will be used
form <- as.formula(paste("Class~", paste(preuse, collapse = "+"))) # build form as input into neuralnet function
training_usepre <- training[,-notuse_train] # prepare training and testing data by excluding predictors that won't be used
testing_usepre <- testing[,-notuse_test]
