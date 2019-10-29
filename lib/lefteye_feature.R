#############################################################
### Construct features and responses for training images  ###
#############################################################
library(rgeos)
library(geometry)
lefteyept<-list()
for (i in 1:2500){
  lefteyept[[i]]<-fiducial_pt_list[[i]][c(2,4,6,8),]
}

lefteyeareafeature <- function(input_list = lefteyept, index){
  
  
polygon_area <- function(vec){
    
    return(polyarea(x=c(vec[1,1], vec[2,1],vec[3,1],vec[4,1],vec[1,1]),y=c(vec[1,2], vec[2,2],vec[3,2],vec[4,2],vec[1,2])))
  }
  
polygon_area_result <-function(mat){
    
    return(as.vector(apply(mat, 2, polygon_area))) 
  }
  
  
  polygon_area_feature <- t(sapply(lefteyept[index], polygon_area_result))
  dim(polygon_area_feature) 
  
  lefteye_data <- cbind(polygon_area_feature, info$emotion_idx[index])
  
  colnames(lefteye_data) <- c(paste("feature", 1:(ncol(lefteye_data)-1), sep = ""), "emotion_idx")
  
  lefteye_data <- as.data.frame(lefteye_data)
  
  lefteye_data$emotion_idx <- as.factor(lefteye_data$emotion_idx)
  
  return(lefteyeareafeature_df = lefteye_data)
}
