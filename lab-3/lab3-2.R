install.packages("ISLR") # "png" package need to be installed to display png
# Load the ISLR package
library(ISLR)
library(class)
# Upload the Smarket data
data(Smarket)

# created a funtion for cross validation with KNN algorithm to use for each feature combination.
knnOperations <- function(descriptive_features){
  set.seed(19961)
  trial_sum <- numeric(50)
  trial_n <- numeric(50)
  
  for(i in 1:100){
    
    smar_sample <- sample(1:nrow(Smarket), size=nrow(Smarket)*0.75)
    smar_train <- Smarket[smar_sample, descriptive_features]
    smar_test <- Smarket[-smar_sample, descriptive_features]
    test_size <- nrow(smar_test)
    
    for(j in 1:50){
      predict <- knn(smar_train[,-length(descriptive_features)], smar_test[,-length(descriptive_features)], smar_train$Direction, k=j)
      trial_sum[j] <- trial_sum[j] + sum(predict==smar_test$Direction)
      trial_n[j] <- trial_n[j] + test_size
    }
  }
  
  cv_acc <- trial_sum / trial_n
  cat("Max accuracy value is", max(cv_acc), "\n")
  cat("Mean accuracy value is", mean(cv_acc), "\n")
  cat("Min accuracy value is", min(cv_acc), "\n")
  k_vals <- c(1:50)
  cat("K value that got highest accuracy is ", k_vals[cv_acc == max(cv_acc)], "\n") 
  cat("K value that got lowest accuracy is ",k_vals[cv_acc == min(cv_acc)], "\n") 
  
  plot(trial_sum / trial_n * 100, type="l", ylab="Accuracy (%)",xlab="K",main="Accuracy for Smarket With Varying K Values (1-50)")
}

des_features1 <- c("Lag1", "Lag2", "Lag3", "Direction")
des_features2 <- c("Lag3", "Lag4", "Lag5", "Direction")
des_features3 <- c("Lag1", "Lag2", "Lag3", "Lag4", "Lag5", "Direction")

knnOperations(des_features1)
knnOperations(des_features2)
knnOperations(des_features3)

#=========================SECOND PART====================================
