#
# This code performs the Iterative Input Selection algorithm, 
# proposed by Galelli and Castelletti (2013a),
# using Random Forest as the Input Ranking (IR) algorithm 
# and Neural Network as the Model Building (MB) algorithm
#
#
# max_iter <- int, maximum number of iterations
# epsilon  <- double, tolerance
# k        <- int, number of folds for the k-fold cross validation
# p        <- int, number of SISO models evaluated at each iteration
# delta    <- double, variation of the distance metric
#
# The response variable must be the in last column of the input dataset
#
# This code was written by Taís Maria Nunes Carvalho
#

data <- read.csv("data_cb.csv")
max_iter <- 6
epsilon <- 0
k <- 2
p <- 10

iterative_inp_selection <- function(data, max_iter, epsilon, k, p){

  require(dplyr)

  n_obs <- nrow(data)
  n <- floor(n_obs/k)
  data <- data[sample(1:n_obs, n*k, replace = FALSE),]

  selected <- vector()
  performance_miso <- vector()     
  predicted_miso <- matrix(nrow = nrow(data), ncol = max_iter)
  rankings <- matrix(nrow = ncol(data)-1, ncol = max_iter)

  delta <- 1
  iter <- 1
  
  while(delta > epsilon && iter <= max_iter){
  
    print("Iteration:"); print(iter)
  
    if(iter == 1){
      
      ranking <- variable_ranking(data)
      siso_model <- siso_ann(data, ranking, k, p, 1, "rsq")
      
    }else{
      
      ranking <- new_ranking
      siso_model <- siso_ann(data_residual, ranking, k, p, 1, "rsq")
      
    }
    rankings[,iter] <- ranking
    
    new_var <- ranking[which.max(siso_model$performance)]
    if(new_var %in% selected){
      
      print("A repeated variable was selected")
      break
      
    }else{
      
      selected <- c(selected, new_var)
      
    }
    print("Selected:"); print(selected)
  
    miso_model <- miso_ann(data, selected, k, 2, "rsq")
  
    performance_miso[iter] <- miso_model$performance
    predicted_miso[,iter] <- miso_model$predicted
  
    residual <- data[,ncol(data)] - miso_model$predicted
    data_residual <- data
    data_residual[,ncol(data_residual)] <- residual
  
    new_ranking <- variable_ranking(data_residual)
    print(new_ranking)
  
    if(iter > 1){
      delta <- performance_miso[iter] - performance_miso[iter-1]
        if(delta <= epsilon){
          selected <- selected[-length(selected)]
          print("Delta <= epsilon")
        }
    }
  
    iter <- iter + 1
  
    if(iter == max_iter){
      print("Max number of iterations was reached!")
    }
  
  }
  return(selected)
}
