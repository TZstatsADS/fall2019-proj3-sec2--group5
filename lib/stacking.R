stacking <- function(xgb_model, svm_model, coef = rep(1/2,2), testdata){
  
  library(dplyr)
  library(caret)
  source("../lib/xgb_test.R")
  
  xgb.pred <- xgb_test(xgb_model, testdata)[,-1]
  svm.pred <- attr(svm_test(svm_model, testdata, probability=TRUE), "probabilities")
  svm.pred <- svm.pred[, order(as.integer(colnames(svm.pred)))]
  
  pred.stack <- coef[1]*xgb.pred + coef[2]*svm.pred %>% as.data.frame()
  pred.stack$prediction <- max.col(pred.stack)
  pred.stack$label <- testdata$emotion_idx
  
  pred.stack.conf <- confusionMatrix(factor(pred.stack$prediction), factor(pred.stack$label), mode = "everything")
  
  return(pred.stack.conf)
  
}