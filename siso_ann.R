#
# This code performs the ANN model (MLP) with a single input and a single output
#
# data       <- dataframe, input data containg the dependent variable in the last column
# selected   <- vector, selected inputs to perform the regression
# k          <- int, number of folds in the k-fold cross validation
# hidden     <- int, number of nodes in the hidden layer of the MLP
# metric     <- string, the performance metric to evaluate the model
# cutoff     <- cutoff to select the input variables for the model
#
# Returns <- list, containing the performance of the model, the predictions and the ann model
#

siso_ann <- function(data, selected, k, cutoff, hidden = 2, metric = "rsq"){
  
  require(RSNNS)
  
  pred <- vector()
  
  n_obs <- nrow(data)
  perf_test <- matrix(nrow = k, ncol = cutoff)
  folds <- cut(seq(1, n_obs), breaks = k, labels = FALSE)
  
  ind <- ranking[1:cutoff]
  input <- normalizeData(data[,ind], type = "0_1")
  output <- normalizeData(data[,ncol(data)], type = "0_1")
  normParam <- getNormParameters(output)

  for(i in 1:k){
    
    fold <- which(folds == i, arr.ind = TRUE)
    
    for(j in 1:cutoff){
      
      model <- mlp(x = input[-fold,j], 
                   y = output[-fold],
                   size = hidden, maxit = 1000,
                   initFunc = "Randomize_Weights",
                   learnFunc = "Std_Backpropagation",
                   inputsTest = input[fold,j],
                   targetsTest = output[fold])
      
      predictions_test <- denormalizeData(as.numeric(model$fittedTestValues), normParam)
      perf_test[i,j] <- performance(data[fold, ncol(data)], predictions_test, metric)
      
    }
  }
  
  perf_model <- apply(perf_test, 2, mean)
  
  return(list(performance = perf_model, predicted = pred))
}
