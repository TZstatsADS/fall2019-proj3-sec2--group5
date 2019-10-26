xgb_tune <- function(dat_train){
  
  
  library("xgboost")  
  source("../lib/xgb_cv.R")
  
  max_depth_values <- seq(7,13,2)
  min_child_weight_values <- seq(5,11,2)
  
  # error matrix
  error_matrix = matrix(NA,nrow = length(max_depth_values), length(min_child_weight_values))
  rownames(error_matrix) <- paste(max_depth_values)
  colnames(error_matrix) <- paste(min_child_weight_values)
  
  ## cv sd matrix
  sd_matrix = matrix(NA,nrow = length(max_depth_values), length(min_child_weight_values))
  rownames(sd_matrix) <- paste(max_depth_values)
  colnames(sd_matrix) <-  paste(min_child_weight_values)
  
  #tuning process
  for (i in 1:length(max_depth_values)){
    for (j in 1:length(min_child_weight_values)){
      par <- list(depth = max_depth_values[i], child_weight = min_child_weight_values[j] )
      error_matrix[i,j] <- xgb_cv(dat_train, par = par)$error
      sd_matrix[i,j] <- xgb_cv(dat_train, par = par)$sd
    }
  }
  
  # best cv.error
  cv_error =  min(error_matrix)
  # best parameter
  best_par = list(depth = max_depth_values[which(error_matrix == min(error_matrix), arr.ind = T)[1]],
                  child_weight = min_child_weight_values[which(error_matrix == min(error_matrix), arr.ind = T)[2]])
  

return(list(error_matrix,best_par))
  

  
}