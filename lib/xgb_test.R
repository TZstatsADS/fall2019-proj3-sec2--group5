xgb_test <- function(model, dat_test, par = NULL) {
  
  library(xgboost)
  
  test_mat_dat <-dat_test[, c(1:(length(dat_test) - 1))] %>% as.matrix()
  test_label <- as.numeric(dat_test$emotion_idx)
  test_matrix <- xgb.DMatrix(data = test_mat_dat, label = test_label)
  
  if (is.null(par)) {
    ntrees = 100
  } else{
    ntrees = par$ntrees
  }
  
  pred <-predict(model, newdata = test_matrix, n.trees = ntrees, reshape=T)
  
  return(pred)
  
}