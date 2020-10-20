
####等价划分####


####方法区####

upStand <- function(x) {
  newx = (x-min(x) + 0.01)/(max(x)-min(x))
  return(newx)
}

meanLine <- function(x) {
  m <- mean(x)
  upm <- 1.5*m
  downm <- 0.5*m
  myList <- c(upm, m, downm)
  return(myList)
}

gradeFunc <- function(x, stand) {
  gradecity <- c()
  numA <- which(x > stand[1])
  numB <- which(x <= stand[1] & x > stand[2])
  numC <- which(x <= stand[2] & x > stand[3])
  numD <- which(x <= stand[3])
  gradecity[numA] <- "A"
  gradecity[numB] <- "B"
  gradecity[numC] <- "C"
  gradecity[numD] <- "D"
  return(gradecity)
}


####读取数据####
data <- read.csv("score.csv")
head(data)
rownames(data) <- data$X
data <- data[, -1]
colnames(data) <- c(2009:2018)

####设置等级####

newdata <- apply(data, 2, upStand)

standLine <- apply(data, 2, meanLine)

gradeMatrix <- matrix(0, ncol = 10, nrow = 71)

rownames(gradeMatrix) <- rownames(data)

for (i in c(1:10)) {
  temp <- newdata[, i]
  stanTemp <- standLine[, i]
  gradeTemp <- gradeFunc(temp, stanTemp)
  print(table(gradeTemp))
  gradeMatrix[, i] <- gradeTemp
  
}

write.csv(gradeMatrix, "./output/gradeMatrix.csv")




