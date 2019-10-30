#############################################################
### Construct features and responses for training images  ###
#############################################################
#library(rgeos)
library(geometry)
## lefteye area feature
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

## mouth area feature
mouth<-list()
for (i in 1:2500){
  mouth[[i]]<-fiducial_pt_list[[i]][c(50,52,54,56),]
}

mouthareafeature <- function(input_list = mouth, index){
  
  
  polygon_area <- function(vec){
    
    return(polyarea(x=c(vec[1,1], vec[2,1],vec[3,1],vec[4,1],vec[1,1]),y=c(vec[1,2], vec[2,2],vec[3,2],vec[4,2],vec[1,2])))
  }
  
  polygon_area_result <-function(mat){
    
    return(as.vector(apply(mat, 2, polygon_area))) 
  }
  
  
  mouth_area_feature <- t(sapply(mouth[index], polygon_area_result))
  dim(mouth_area_feature) 
  
  mouth_data <- cbind(mouth_area_feature, info$emotion_idx[index])
  
  colnames(mouth_data) <- c(paste("feature", 1:(ncol(mouth_data)-1), sep = ""), "emotion_idx")
  
  mouth_data <- as.data.frame(mouth_data)
  
  mouth_data$emotion_idx <- as.factor(mouth_data$emotion_idx)
  
  return(mouthareafeature_df = mouth_data)
}

## nose area feature
nose<-list()
for (i in 1:2500){
  nose[[i]]<-fiducial_pt_list[[i]][c(38,42,44,46),]
}


noseareafeature <- function(input_list = nose, index){
  
  
  polygon_area <- function(vec){
    
    return(polyarea(x=c(vec[1,1], vec[2,1],vec[3,1],vec[4,1],vec[1,1]),y=c(vec[1,2], vec[2,2],vec[3,2],vec[4,2],vec[1,2])))
  }
  
  polygon_area_result <-function(mat){
    
    return(as.vector(apply(mat, 2, polygon_area))) 
  }
  
  
  nose_area_feature <- t(sapply(nose[index], polygon_area_result))
  dim(nose_area_feature) 
  
  nose_data <- cbind(nose_area_feature, info$emotion_idx[index])
  
  colnames(nose_data) <- c(paste("feature", 1:(ncol(nose_data)-1), sep = ""), "emotion_idx")
  
  nose_data <- as.data.frame(nose_data)
  
  nose_data$emotion_idx <- as.factor(nose_data$emotion_idx)
  
  return(noseareafeature_df = nose_data)
}

## eyelid area feature
eyelid<-list()
for (i in 1:2500){
  eyelid[[i]]<-fiducial_pt_list[[i]][c(2,6,19,23),]
}

eyelidareafeature <- function(input_list = eyelid, index){
  
  
  polygon_area <- function(vec){
    
    return(polyarea(x=c(vec[1,1], vec[2,1],vec[3,1],vec[4,1],vec[1,1]),y=c(vec[1,2], vec[2,2],vec[3,2],vec[4,2],vec[1,2])))
  }
  
  polygon_area_result <-function(mat){
    
    return(as.vector(apply(mat, 2, polygon_area))) 
  }
  
  
  eyelid_area_feature <- t(sapply(eyelid[index], polygon_area_result))
  dim(eyelid_area_feature) 
  
  eyelid_data <- cbind(eyelid_area_feature, info$emotion_idx[index])
  
  colnames(eyelid_data) <- c(paste("feature", 1:(ncol(eyelid_data)-1), sep = ""), "emotion_idx")
  
  eyelid_data <- as.data.frame(eyelid_data)
  
  eyelid_data$emotion_idx <- as.factor(eyelid_data$emotion_idx)
  
  return(eyelidareafeature_df = eyelid_data)
}

