#############################################################
### Construct features and responses for training images  ###
#############################################################

## dat_train <- feature(fiducial_pt_list, train_idx)
## dat_test <- feature(fiducial_pt_list, test_idx)
## reduced_train <- feature(fiducial_pt_list, train_idx, df="reduced")
## reduced_test <- feature(fiducial_pt_list, test_idx, df="reduced")



 feature <- function(input_list = fiducial_pt_list, index, df="full"){
  
   if(df=="reduced"){
   
   select_pt <-function (df) {
     return(df[c(19, 20, 21, 22, 23, 27, 28, 29, 30, 31, 1, 2, 4, 6, 8, 10, 11, 13, 15, 17,35, 38, 42, 46,50, 54, 58, 59, 60, 61, 62, 63,67, 71, 75, 64, 78),])
   }
  
   input_list[index] <- lapply(input_list[index], select_pt)}
 
  ### Construct process features for training images 
  
  ### Input: a list of images or fiducial points; index: train index or test index

  ### Output: a data frame containing: features and a column of label
  
  ### here is an example of extracting pairwise distances between fiducial points
  ### Step 1: Write a function pairwise_dist to calculate pairwise distance of items in a vector
  pairwise_dist <- function(vec){
    ### input: a vector(length n), output: a vector containing pairwise distances(length n(n-1)/2)
    return(as.vector(dist(vec)))
  }
  
  ### Step 2: Write a function pairwise_dist_result to apply function in Step 1 to column of a matrix 
  pairwise_dist_result <-function(mat){
    ### input: a n*2 matrix(e.g. fiducial_pt_list[[1]]), output: a vector(length n(n-1))
    return(as.vector(apply(mat, 2, pairwise_dist))) 
  }
  
  
  
### Step 3: Apply function in Step 2 to selected index of input list, output: a feature matrix with ncol = n(n-1) = 78*77 = 6006
  pairwise_dist_feature <- t(sapply(input_list[index], pairwise_dist_result))
  dim(pairwise_dist_feature) 
  
  ### Step 4: construct a dataframe containing features and label with nrow = length of index
  ### column bind feature matrix in Step 3 and corresponding features
  pairwise_data <- cbind(pairwise_dist_feature, info$emotion_idx[index])
  ### add column names
  colnames(pairwise_data) <- c(paste("feature", 1:(ncol(pairwise_data)-1), sep = ""), "emotion_idx")
  ### convert matrix to data frame
  pairwise_data <- as.data.frame(pairwise_data)
  ### convert label column to factor
  pairwise_data$emotion_idx <- as.factor(pairwise_data$emotion_idx)
  
  return(feature_df = pairwise_data)
}
