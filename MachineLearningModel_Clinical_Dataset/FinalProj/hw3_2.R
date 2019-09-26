library(caret)
kin <- read.csv("hw3-kinetics-data.csv", header = FALSE) 
head(kin)


model <- keras_model_sequential() 
##### Add layers to your model ##### 
model %>%
layer_dense(units = 4, activation = 'relu', input_shape = c(17)) %>% # Four hidden neurons, 2,4,8,16,32,64
layer_dense(units = 1, activation = 'sigmoid') 
# Output layer ##### Compile your model ##### 
model %>% compile( 
	loss = 'binary_crossentropy', 
# Good for binary classification task 
	optimizer = 'sgd', # Stochastic Gradient Descent 
	metrics = 'accuracy' ) 
##### Fit your model ##### 
# Fit the model 
model %>% fit( 
	meth.training, # Training data. Replace with whatever you named this. 
	meth.trainingtarget,
 # Training data output. Replace with whatever you named this. 
 	epochs = 200, 
 	batch_size = 5, 
 	validation_split = 0.25, 
 	verbose = 2 # This graphs the training and validation loss and accuracy over epochs while training is happening
 	)
 	
 	