performance <- function(obs, pred, metric, nvar = NULL){
  
  require(Metrics)
  
  nobs <- length(obs)
  if(metric == 'mae') m <- mae(obs, as.numeric(pred))
  if(metric == 'mse') m <- mse(obs, as.numeric(pred))
  if(metric == 'rmse') m <- rmse(obs, as.numeric(pred))
  if(metric == 'rsq') m <- cor(obs, as.numeric(pred))^2
  if(metric == 'ad_rsq') m <- 1 - ((1 - cor(obs, as.numeric(pred))^2) * ((nobs - 1)/(nobs - nvar - 1)))
  
  return(m)
}