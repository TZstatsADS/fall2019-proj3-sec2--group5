xgb_train <- function(dat_train, par=NULL){
  
  
  library("xgboost")
  
  if(is.null(par)){
    depth <- 5
    child_weight <- 3
    gamma <- 0
    colsample <- 0.5
    eta <- 0.05
    nrounds <- 500
  } else {
    depth <- par$depth
    child_weight <- par$child_weight
    gamma <- par$gamma
    colsample <- par$colsample
    eta <- par$eta
    nrounds <- par$nrounds
  }
  
  
  numberOfClasses <- length(unique(dat_train$emotion_idx))
  
  train_mat_dat <-dat_train[, c(1:(length(dat_train) - 1))] %>% as.matrix()
  train_label <- as.numeric(dat_train$emotion_idx)
  train_matrix <- xgb.DMatrix(data = train_mat_dat, label = train_label)
  
  
  xgb.fit<- xgb.train(data=train_matrix,
                      eta=eta,
                      gamma=gamma,
                      colsample_bytree=colsample,
                      max_depth=depth,
                      min_child_weight = child_weight,
                      objective="multi:softprob",
                      eval_metric="mlogloss",
                      num_class=numberOfClasses + 1,
                      nrounds=nrounds,
                      verbose=0
                      )
  
  return(xgb.fit)
  
}