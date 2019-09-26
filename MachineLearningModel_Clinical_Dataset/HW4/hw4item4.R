#setwd("/Users/vera/Desktop/BIOMI600/hw4/")
source('golubprep.R', local = TRUE)
library(keras)
install_keras(tensorflow = '1.5.0')

# Build a sequential model
model_4 <- keras_model_sequential() 
model_4 %>%
  layer_dense(units = 16, activation = 'relu', input_shape = c(80)) %>% 
  layer_dense(units = 32, activation = 'relu') %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dense(units = 1, activation = 'sigmoid')

# Compile model
model_4 %>%
  compile(loss = "binary_crossentropy", optimizer = "sgd", metrics = 'accuracy')

# Split data into training and testing
golub_train <- golub_whole[1:45,1:80] # first 45 patients in training set
golub_test <- golub_whole[46:72,1:80] # last 27 patients in testing set
train_tclass <- golub_whole[1:45,81] # get true classification for training and testing
test_tclass <- golub_whole[46:72,81]

# Fit training data into model
model_4 %>%
  fit(golub_train, train_tclass, epochs = 10, validation_split = 0.1)

# Test model on testing data
model_4 %>%
  evaluate(golub_test, test_tclass)
