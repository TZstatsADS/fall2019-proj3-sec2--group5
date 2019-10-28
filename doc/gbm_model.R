##Train
source("../lib/gbm_train.R")
gbm.fit<-gbm_train(dat_train)
saveRDS(gbm.fit,"gbm.RDS")

##Test
source("../lib/gbm_test.R")
gbm.fit<-readRDS("gbm.RDS")
pred.gbm<-gbm_test(gbm.fit,dat_test)

##Predict class
pred.class<-apply(pred_gbm,1,which.max)

##Evaluation
mean(dat_test$emotion_idx==pred.class)
confusionMatrix(dat_test$emotion_idx,as.factor(pred.class))

