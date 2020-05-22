#
# This code performs the ANN model (MLP) with multiple inputs and a single output
#
# data       <- dataframe, input data containg the dependent variable in the last column
# selected   <- vector, selected inputs to perform the regression
# k          <- int, number of folds in the k-fold cross validation
# hidden     <- int, number of nodes in the hidden layer of the MLP
# metric     <- string, the performance metric to evaluate the model
#
# Returns <- list, containing the performance of the model, the predictions and the ann model
#

miso_ann <- function(data, selected, k, hidden = 2, metric = "rsq"){
  
  require(RSNNS)
  
  pred <- vector()
  
  n_obs <- nrow(data)
  perf_test <- matrix(nrow = k, ncol = 1)
  folds <- cut(seq(1, n_obs), breaks = k, labels = FALSE)
  
  input <- normalizeData(data[,selected], type = "0_1")
  output <- normalizeData(data[,ncol(data)], type = "0_1")
  normParam <- getNormParameters(output)
  
  for(i in 1:k){
    
    fold <- which(folds == i, arr.ind = TRUE)
    
    model <- mlp(x = input[-fold,], 
                 y = output[-fold],
                 size = hidden, maxit = 1000,
                 initFunc = "Randomize_Weights",
                 learnFunc = "Std_Backpropagation",
                 inputsTest = input[fold,],
                 targetsTest = output[fold])
    
    predictions_test <- denormalizeData(as.numeric(model$fittedTestValues), normParam)
    perf_test[i,] <- performance(data[fold, ncol(data)], predictions_test, metric)
    
    pred <- c(pred, denormalizeData(as.numeric(model$fittedTestValues), normParam))
    
  }
  
  return(list(performance = mean(perf_test), predicted = pred, ann = model))
}
