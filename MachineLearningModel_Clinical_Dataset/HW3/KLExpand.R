#setwd("/Users/vera/Desktop/BIOMI600/hw3/")

#KL Expansion function
#input data matrix should have measurements (parameters) as columns and observations (patients) as rows
KLExpand <- function(data_matrix) {
  num_obs <- dim(data_matrix)[1] #get number of observations(patients)=number of rows in data frame
  cov_matrix <- crossprod(data_matrix)/num_obs #calculate covariance matrix by multiplying inputting data matrix with it's transpose matrix and divided by number of observations
  output <- eigen(cov_matrix) #use eigen function to get eigen values and eigen vectors
  eigen_values <- output$values #extract eigen values from output
  eigen_vectors <- output$vectors #extract eigen vectors from output
  feature_vector <- t(eigen_vectors) %*% t(data_matrix) #multiply transpose of eigen vectors with transpose of original data matrix to get feature vectors 
  KL_out <- list("EigenValues"=eigen_values, "EigenVectors"=eigen_vectors, "FeatureVectors"=feature_vector) #make a list of values need to be returned
  return(KL_out) #return that list
}