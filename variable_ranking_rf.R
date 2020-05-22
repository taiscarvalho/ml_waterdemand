#
# This code performs the variable ranking with Random Forest
#
# data   <- dataframe, input data containg the dependent variable in the last column
# runs   <- int, number of times to run the model
#
# Returns <- vector, ranking of variables according to the IncMSE
#

variable_ranking <- function(data, runs = 100){
  
  require(randomForest)
  require(dplyr)
  
  n_var = ncol(data) - 1
  n_obs = nrow(data)
  vec_obs = 1:n_obs
  
  imp_rf_IncMSE <- list()
  imp_var_IncMSE_mean <- matrix(nrow = n_obs, ncol = n_var)
  
  for(j in 1:n_obs){
    
    imp_rf_IncMSE[[j]] <- matrix(ncol = n_var, nrow = runs)
    
    for(i in 1:runs){
      
      ind_train = vec_obs[-j]
      ind_test = j
      
      forest <- randomForest(x = data[,-ncol(data)], y = data[,ncol(data)],
                             xtest = data[ind_test,-ncol(data)],
                             ytest = data[ind_test, ncol(data)],
                             subset = ind_train,
                             mtry = n_var/3, ntree = 100,
                             importance = T)
      
      obs_test <- data[ind_test, ncol(data)]
      prev_test <- forest$test$predicted
      
      imp_rf_IncMSE[[j]][i,] <- importance(forest)[,1]
      
    }
    imp_var_IncMSE_mean[j,] = apply(imp_rf_IncMSE[[j]], MARGIN = 2, FUN = mean)
  }
  
  ranking <- apply(imp_var_IncMSE_mean, MARGIN = 2, FUN = median) %>%
    order(decreasing = T)
  
  return(ranking)
}
