stacking <- function(xgb_model, svm_model, coef = rep(1/2,2), testdata, nolabel=FALSE){
  
  library(dplyr)
  library(caret)
  source("../lib/xgb_test.R")
  
  if(nolabel==TRUE){
    xgb.pred <- xgb_test(xgb_model, testdata)[,-1]
  }else{
    xgb.pred <- xgb_test(model = xgb_model, dat_test = testdata[, c(1:(length(testdata) - 1))])[,-1]
  }
  
  
  svm.pred <- attr(svm_test(svm_model, testdata, probability=TRUE), "probabilities")
  svm.pred <- svm.pred[, order(as.integer(colnames(svm.pred)))]
  
  pred.stack <- coef[1]*xgb.pred + coef[2]*svm.pred %>% as.data.frame()
  pred.stack$prediction <- max.col(pred.stack)
  
  if(nolabel==FALSE){ 
    pred.stack$label <- testdata$emotion_idx
    pred.stack.conf <- confusionMatrix(factor(pred.stack$prediction), factor(pred.stack$label), mode = "everything")
    return(pred.stack.conf)
  }else{
     return(pred.stack$prediction) 
    }
  
}