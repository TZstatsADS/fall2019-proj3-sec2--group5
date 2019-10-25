xgb_cv <- function(dat_train, K=5, par=NULL){
  
  
  library("xgboost")
  source("../lib/train.R")
  source("../lib/test.R")
  
  n <- nrow(dat_train)
  n.fold <- floor(n/K)
  s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
  cv.error <- rep(NA, K)
  
  for (i in 1:K){
    
    train.data <- dat_train[s != i,]
    test.data <- dat_train[s == i,]
    
    fit.model <- train(train.data, par = par)
    pred <- test(fit.model, par = par)
    
    cv.error[i] <- mean(pred != test.label)
    
  }		
  
  return(list(error = mean(cv.error), sd = sd(cv.error)) )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}