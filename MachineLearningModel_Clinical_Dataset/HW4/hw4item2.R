#setwd("/Users/vera/Desktop/BIOMI600/hw4/")
library('neuralnet')

top3_genes <- c('X98743_at', 'U54778_at', 'Z29505_at')
GolubTrain <- read.csv('golubtrain.csv', header = TRUE)
GolubTest <- read.csv('golubtest.csv', header = TRUE)

gene_indices <- c()
for (i in top3_genes) {
  index <- grep(i, colnames(GolubTrain))
  gene_indices <- c(gene_indices, index)
}

train <- GolubTrain[, gene_indices]
test <- GolubTest[, gene_indices]
train_class <- GolubTrain[,2001]
test_class <- GolubTest[,2001]

train <- as.matrix(apply(train, 2, function(x)(x-min(x))/(max(x)-min(x))))
test <- as.matrix(apply(test, 2, function(x)(x-min(x))/(max(x)-min(x))))

form_golub <- as.formula(paste("Class~", paste(top3_genes, collapse = "+")))
Class <- as.numeric(train_class)-1
train <- cbind(train, Class)

# Use delta rule network
error_vector_dr <- c()  # create an empty vector to hold errors for 10 runs
for (i in 1:10) { 
  nn_deltarule_golub <- neuralnet(form_golub, data=train, hidden=2, stepmax=1e+06, rep=30, learningrate=0.03, algorithm = "backprop", err.fct = "ce", act.fct = "logistic", linear.output = FALSE)
  errors <- nn_deltarule_golub$result.matrix[1,] # get errors for each epoch
  plot(c(1:30), errors, xlab = 'epoch', ylab = 'error', type = 'l')  # plot error over epoch
  mean_error_train <- mean(errors)
  error_vector_dr <- c(error_vector_dr, mean_error_train)
}

# Use two-stage backpropagation network
error_vector_bp_golub <- c()  # create an empty vector to hold errors for 10 runs
for (i in 1:10) { 
  nn_bp_golub <- neuralnet(form_golub, data=train, hidden=c(2,1), stepmax=1e+07, rep=30, learningrate=0.1, algorithm = "backprop", err.fct = "ce", act.fct = "logistic", linear.output = FALSE)
  errors <- nn_bp_golub$result.matrix[1,] # get errors for each epoch
  plot(c(1:30), errors, xlab = 'epoch', ylab = 'error', type = 'l')  # plot error over epoch
  mean_error_train <- mean(errors)
  error_vector_bp_golub <- c(error_vector_bp_golub, mean_error_train)
}