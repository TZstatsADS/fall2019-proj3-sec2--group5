

gbm_test<-function(model, dat_test){
	
	library(gbm)
	
	best_iter<-gbm.perf(model,method="cv")
	
	###Testing
	pred_gbm<-predict(object=model,
                  newdata=dat_test,
                  n.trees=best_iter,
                  type="response")
	
	return(pred_gbm)
}
