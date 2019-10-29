library(gbm)

gbm_tune<-function(dat_train){
  hyper_grid <- expand.grid(
    shrinkage = c(.01, .1, 0.05),
    bag.fraction = c(0.5, 0.65, 0.8), 
    optimal_trees = NA,               
    min_RMSE = NA,
    gbm.tune.time=NA
  )
  
  for (i in 1:dim(hyper_grid)[1]){
    time<-system.time(gbm.tune<-gbm(emotion_idx~.,
                                    distribution="multinomial",
                                    data=dat_train,
                                    n.trees = 200,
                                    interaction.depth=1,
                                    shrinkage = hyper_grid$shrinkage[i],
                                    bag.fraction=hyper_grid$bag.fraction[i],
                                    cv.folds=3))
    
    best_iter<-gbm.perf(gbm.tune,method="cv")
    
    hyper_grid$gbm.tune.time[i]<-time[3]
    hyper_grid$optimal_trees[i] <- best_iter
    hyper_grid$min_RMSE[i]<-sqrt(min(gbm.tune$cv.error))
  }
}
