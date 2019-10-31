xgb_test <- function(model, dat_test) {
  
  library(xgboost)
  
  test_mat_dat <- as.matrix(dat_test)
  
  pred <-predict(model, newdata = test_mat_dat, missing=NA, n.trees = 500, reshape=T)
  
  return(pred)
  
}