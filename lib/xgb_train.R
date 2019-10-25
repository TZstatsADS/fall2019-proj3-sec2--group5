xgb_train <- function(dat_train, par=NULL){
  
  
  library("xgboost")
  
  if(is.null(par)){
    depth <- 5
    child_weight <- 3
  } else {
    depth <- par$depth
    child_weight <- par$child_weight
  }
  
  
  numberOfClasses <- length(unique(dat_train$emotion_idx))
  
  train_mat_dat <-dat_train[, c(1:(length(dat_train) - 1))] %>% as.matrix()
  train_label <- as.numeric(dat_train$emotion_idx)
  train_matrix <- xgb.DMatrix(data = train_mat_dat, label = train_label)
  
  
  xgb.fit<- xgb.train(data=train_matrix,
                      eta=0.1,
                      max_depth=depth,
                      min_child_weight = child_weight,
                      objective="multi:softprob",
                      eval_metric="mlogloss",
                      num_class=numberOfClasses + 1,
                      nrounds=300,
                      verbose=0
                      )
  
  return(xgb.fit)
  
}