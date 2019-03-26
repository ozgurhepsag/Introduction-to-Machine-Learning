
inputControl <- function(vector){
  counter <- 0
  
  for(i in vector){
    
    if(i == "Ali")
      counter <- counter + 1
    
  }
  
  return(counter)
}

dim(iris)
apply(iris[,1:4], 2, mean)
myList <- lapply(iris[1:4], function(x) x[1:10])
retList <- lapply(myList, sum)
retList$Petal.Length
retVec <- sapply(myList, mean)

inp <- c("Veli", "Asli", "Kerem", "Leyla", "Ali", "Ozgur")
which(inp == "Ali")
vecwithNA <- c("Asli", "Veli", NA, "Leyla", NA, "Mecnun")
vecwithoutNA <- vecwithNA[!is.na(vecwithNA)]

proteinData <- as.data.frame(read.table(file = "C:/Users/ABRA/Documents/RWorkingDirectory/Homeworks/Protein.txt", header = TRUE, sep = "\t"))
dim(proteinData)
patientData <- as.data.frame(read.csv(file = "C:/Users/ABRA/Documents/RWorkingDirectory/Homeworks/Patient-Subtype.csv", header = TRUE, sep = ";"))
dim(patientData)
basalPatientID <- as.vector(patientData[patientData[["Sub.type"]] == "basal",1])
length(basalPatientID)
testData <- proteinData[basalPatientID]
apply(data.matrix(testData), 2, mean)
apply(data.matrix(testData), 2, sd)

barplot(table(mtcars$gear), xlab = "Gear Type", ylim = c(0,16))

hist(mtcars$mpg, col=rgb(0,0,1, seq(from = 0.1, to = 0.6, length = 5)), freq=FALSE, xlab="Petrol Consumption", ylim=c(0,0.08), main = NULL)
lines(density(mtcars$mpg), col = "black")

plot(main = "", density(mtcars$disp), col = "blue", xlab = "Distribution of engine horsepower and displacementvalues", ylim = c(0,0.006), lwd=2)
lines(density(mtcars$hp), col = "red", lwd = 2, xlim = c(0,600))
legend(500, 0.005, legend = c("hp", "disp"), col = c("red", "blue"), pch = c(19, 19))

plot(mtcars$hp, mtcars$qsec, pch = 7, xlab = "Horsepower", ylab = "Seconds to get 0.4 km")
help(rgb)

