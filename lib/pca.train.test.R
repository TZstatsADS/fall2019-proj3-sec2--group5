
pca.train.test <- function (train, test) {
  library(MASS)
  pr.out.train=prcomp(train[,-c(ncol(train))], center=TRUE, scale=TRUE)
  eigs <- pr.out.train$sdev^2
  
  selected_pc <- min(which(cumsum(eigs)/sum(eigs)>=0.99))
  
  pc.train <- pr.out.train$x[,1:selected_pc]
  pc.train <-as.data.frame(cbind(pc.train, train$emotion_idx))
  colnames(pc.train)[ncol(pc.train)] <- "emotion_idx"
  pc.train$emotion_idx <- as.factor(pc.train$emotion_idx)
  
 
  ## PCA on testing
  scaled.test <- scale(test[,-c(ncol(test))], center=TRUE, scale=TRUE)
  pc.test <- scaled.test %*% pr.out.train$rotation[,1:selected_pc]
  pc.test <- as.data.frame(cbind(pc.test, test$emotion_idx))
  colnames(pc.test)[ncol(pc.test)] <- "emotion_idx"
  pc.test$emotion_idx <- as.factor(pc.test$emotion_idx)
  
  return(list(train=pc.train, test=pc.test))

}


