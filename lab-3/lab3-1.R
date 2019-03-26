#install.packages("png") # this library needs to be installed to read png files
library("png") 
library(tree)

# read txt file as dataframe
wheat_dataset <- read.table(file = "C:/Users/ABRA/Documents/RWorkingDirectory/Homeworks/hw3/wheat_types.txt", header = TRUE, sep = ";", dec = ".")
wheat_dataset$type <- as.factor(wheat_dataset$type)

# construct a DT with tree function using wheat_types dataset
original_DT <- tree(formula = type ~ ., data=wheat_dataset, split = "deviance")
original_DT
# display the results
summary(original_DT)
misclass.tree(original_DT)

# visualize DT 
plot(original_DT,  type = "uniform") 
text(original_DT)

# for reproducibility
set.seed(1233)

subset <- sample(1:nrow(wheat_dataset), size=nrow(wheat_dataset)*0.8)

wh.tr <- tree(type ~ ., data = wheat_dataset, subset = subset)
wh.tr
summary(wh.tr)
misclass.tree(wh.tr)

# plot final DT
plot(wh.tr,  type = "uniform")
text(wh.tr)

# Compute training performance of the DT by using only training samples (their indices were saved in the "subset" vector)
train_predict <- table(predict(wh.tr, wheat_dataset[subset, ], type = "class"), wheat_dataset[subset, "type"])
rownames(train_predict) <- paste("Actual", rownames(train_predict), sep = ":")
colnames(train_predict) <- paste("Predicted", colnames(train_predict), sep = ":")
print(train_predict)

# Compute test performance of the DT  by using only test samples
test_predict <- table(predict(wh.tr, wheat_dataset[-subset, ], type = "class"), wheat_dataset[-subset, "type"])
rownames(test_predict) <- paste("Actual", rownames(test_predict), sep = ":")
colnames(test_predict) <- paste("Predicted", colnames(test_predict), sep = ":")
print(test_predict)

#Cross-validation version - Construct a new DT for different partitions of the samples - 100 times
dt_accuracy <- numeric()
set.seed(12345)

for(i in 1:100){
  subset <- sample(1:nrow(wheat_dataset), size=nrow(wheat_dataset)*0.8)
  temp.tr <- tree(type ~ ., data = wheat_dataset, subset = subset)
  test_predict <- table(predict(temp.tr, wheat_dataset[-subset, ], type = "class"), wheat_dataset[-subset, "type"])
  dt_accuracy <- c(dt_accuracy, sum(diag(test_predict)) / sum(test_predict))
  
  dt_name <- sprintf('dt_%d.png', i) #iteration number to filename
  plot(temp.tr,  type = "uniform")
  text(temp.tr)
  dev.copy(png, dt_name) # save the DT with its iteration number
  dev.off()
}

cat("Mean accuracy is", mean(dt_accuracy), "\n")
cat("Min accuracy is", min(dt_accuracy), "\n")
cat("Max accuracy is", max(dt_accuracy), "\n")

plot(dt_accuracy*100, type="l", ylab="Accuracy (%)", xlab="Iterations", main="Accuracy Changings with Different Subsets of Data")

for(i in 1:100){
  if(dt_accuracy[i] == max(dt_accuracy)){
    dt_name <- sprintf('dt_%d.png', i) #iteration number
    cat(i, ". iteration has the highest accuracy.\n")
    img <- readPNG(dt_name) # read the DT that has the highest accuracy
    plot.new() 
    rasterImage(img,0,0,1,1) # display DT that got highest accuracy from cross validation
  }
}

