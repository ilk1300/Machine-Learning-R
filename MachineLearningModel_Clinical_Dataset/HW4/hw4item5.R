#setwd("/Users/vera/Desktop/BIOMI600/hw4/")
source('golubprep.R', local = TRUE)
source('hemoprep.R', local = TRUE)
library(keras)
install_keras(tensorflow = '1.5.0')

## Golub dataset*********************************************************
# Split data into training and testing 0.7/0.3
set.seed(123)
split_index1 <- sample(1:nrow(golub_whole), round(0.7*nrow(golub_whole)))
golub_train1 <- golub_whole[split_index1,1:80]
golub_test1 <- golub_whole[-split_index1,1:80]
tclass_golub <- golub_whole[,81]
train1_tclass_golub <- golub_whole[split_index1,81]
test1_tclass_golub <- golub_whole[-split_index1,81]

# Split data into training and testing 0.6/0.4
set.seed(123)
split_index2 <- sample(1:nrow(golub_whole), round(0.6*nrow(golub_whole)))
golub_train2 <- golub_whole[split_index2,1:80]
golub_test2 <- golub_whole[-split_index2,1:80]
train2_tclass_golub <- golub_whole[split_index2,81]
test2_tclass_golub <- golub_whole[-split_index2,81]

## Build sequential model1 with five hidden layers
model1 <- keras_model_sequential() 
model1 %>%
  layer_dense(units = 67, activation = 'relu', input_shape = c(80)) %>% 
  layer_dense(units = 54, activation = 'relu') %>%
  layer_dense(units = 40, activation = 'relu') %>%
  layer_dense(units = 27, activation = 'relu') %>%
  layer_dense(units = 14, activation = 'relu') %>%
  layer_dense(units = 1, activation = 'sigmoid')

# Compile model
model1 %>%
  compile(loss = "binary_crossentropy", optimizer = "sgd", metrics = 'accuracy')

# Fit training data into model
model1 %>%
  fit(golub_train2, train2_tclass_golub, epochs = 10, validation_split = 0.1)

# Test model on testing data
model1 %>%
  evaluate(golub_test2, test2_tclass_golub)

## Build sequential model2 with four hidden layers
model2 <- keras_model_sequential() 
model2 %>%
  layer_dense(units = 64, activation = 'relu', input_shape = c(80)) %>% 
  layer_dense(units = 46, activation = 'relu') %>%
  layer_dense(units = 30, activation = 'relu') %>%
  layer_dense(units = 14, activation = 'relu') %>%
  layer_dense(units = 1, activation = 'sigmoid')

# Compile model
model2 %>%
  compile(loss = "binary_crossentropy", optimizer = "sgd", metrics = 'accuracy')

# Fit training data into model
model2 %>%
  fit(golub_train2, train2_tclass_golub, epochs = 10, validation_split = 0.1)

# Test model on testing data
model2 %>%
  evaluate(golub_test2, test2_tclass_golub)

## Hemo dataset*********************************************************

# split data 
training1 <- training[,-notuse_test]
testing1 <- testing[,-notuse_test]

## Build sequential model3 with four hidden layers
model3 <- keras_model_sequential() 
model3 %>%
  layer_dense(units = 13, activation = 'relu', input_shape = c(16)) %>% 
  layer_dense(units = 10, activation = 'relu') %>%
  layer_dense(units = 7, activation = 'relu') %>%
  layer_dense(units = 4, activation = 'relu') %>%
  layer_dense(units = 1, activation = 'sigmoid')

# Compile model
model3 %>%
  compile(loss = "binary_crossentropy", optimizer = "sgd", metrics = 'accuracy')

# Fit training data into model
#model3 %>%
#  fit(training1, training_tclass, epochs = 10, validation_split = 0.1)

# Test model on testing data
#model3 %>%
#  evaluate(testing1, testing_tclass)

## Build sequential model4 with three hidden layers
model4 <- keras_model_sequential() 
model4 %>%
  layer_dense(units = 12, activation = 'relu', input_shape = c(16)) %>% 
  layer_dense(units = 8, activation = 'relu') %>%
  layer_dense(units = 4, activation = 'relu') %>%
  layer_dense(units = 1, activation = 'sigmoid')

# Compile model
model4 %>%
  compile(loss = "binary_crossentropy", optimizer = "sgd", metrics = 'accuracy')

# Fit training data into model
#model4 %>%
#  fit(training1, training_tclass, epochs = 10, validation_split = 0.1)

# Test model on testing data
#model4 %>%
#  evaluate(testing1, testing_tclass)
