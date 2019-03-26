name <- "John"
measure <- 5.7
fault <- TRUE

name_vector <- c("John", "Asli", "Can", "Berk", "Cansu")
num_vector <- c(3, -2, 4, -1, 5)
bool_vector <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
rand_num <- seq(from=3, to=100, length=30)

name_vector[c(2,3)]
num_vector[(num_vector > 3)]
name_vector[bool_vector]
pos_num <- num_vector[(num_vector>0)]

person_list <- list("John", 27, "Computer Engineer")
names(person_list) = c("name", "age", "occupation")
person_list <- c(person_list, salary = 4000)
person_list[["name"]]
person_list[["salary"]]

weather_matrix <- matrix(seq(from=5, to=30, length=15), byrow = TRUE, nrow = 5)
rownames(weather_matrix) <- c("day1", "day2", "day3", "day4", "day5")
colnames(weather_matrix) <- c("s1", "s2", "s3")
subB <- weather_matrix[c("day4", "day5"), c("s2", "s3")]

dim(mtcars)
smallc <- subset(mtcars, subset = cyl <= 6)
length(rownames(smallc))
mean(smallc[["hp"]])
rownames(smallc[smallc$gear == 5,])

inp_vec <- c(5, 2, 7, 6, 3, 19, 23, 78, 145, 3, 4, 6, 9, 12, 67)
for(i in inp_vec) {
  if(i %% 2 == 0) {
    cat(i, " is even\n")
  } 
  else if (i %% 2 == 1) {
    cat(i, " is odd\n")
  }
}

name <- c("Ali", "Cenk", "Mete")
age <- c(26, 32, 29) 
salary <- c(2700, 3200, 4900)
company <- data.frame(name=name, age=age, salary=salary)
highestSal <- 0
highestSalName <- ""
for(i in length(rownames(company))) {
  if(company[i, "salary"] > highestSal) {
    highestSal <- company[i, "salary"]
    highestSalName <- company[i, "name"] 
  } 
}
sprintf("%s gets the highest salary.\n", highestSalName)



