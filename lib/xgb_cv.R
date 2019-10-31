xgb_cv <- function(dat_train, K=5, par=NULL){
  
  
  library("xgboost")
  source("../lib/xgb_train.R")
  source("../lib/xgb_test.R")
  
  n <- nrow(dat_train)
  n.fold <- floor(n/K)
  s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
  cv.error <- rep(NA, K)
  
  for (i in 1:K){
    
    train.data <- dat_train[s != i,]
    test.data <- dat_train[s == i,c(1:(length(dat_train) - 1))]
    test.label <- as.numeric(test.data$emotion_idx)
    
    fit.model <- xgb_train(train.data, par = par)
    
    pred <- xgb_test(fit.model, test.data) %>% as.data.frame() %>% 
      mutate(prediction = max.col(., ties.method = "last")-1)
    
    cv.error[i] <- mean(pred$prediction != test.label)
    
  }		
  
  return(list(error = mean(cv.error), sd = sd(cv.error), error_list = cv.error) )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}