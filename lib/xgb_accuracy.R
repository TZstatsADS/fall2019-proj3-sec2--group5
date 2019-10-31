xgb_accuracy <- function(model, testdata, testlabel = test_label) {
  
  source("../lib/xgb_test.R")
  
  accuracy.df <- xgb_test(model = model, dat_test = testdata[, c(1:(length(testdata) - 1))]) %>% as.data.frame() %>% mutate(prediction = max.col(., ties.method = "last")-1, label = testlabel)
  
  accuracy.conf <- confusionMatrix(factor(accuracy.df$prediction), factor(accuracy.df$label), mode = "everything")
  
  return(accuracy.conf)
  
}