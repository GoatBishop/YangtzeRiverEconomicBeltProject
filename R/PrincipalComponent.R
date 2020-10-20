
## 主成分+变异系数求权重
  
##前期准备

library(magrittr)
data <- read.csv("mydataTemp3.csv", stringsAsFactors = F)
head(data)
dim(data)
table(data$year)
colnames(data)[-c(1:3)]

  
  
##主成分法方法区

myStand <- function(x) {
  newx = (x-mean(x))/sd(x)
  return(newx)
}

downStand  <- function(x) {
  newx = myStand(-x)
  return(newx)
}

upStand2 <- function(x) {
  newx = (x-min(x) + 0.01)/(max(x)-min(x))
  return(newx)
}

downStand2  <- function(x) {
  newx = (max(x)-x + 0.01)/(max(x)-min(x))
  return(newx)
}

weightDemo <- function(x) {
  #这个问题以后再解决
  R <- (t(x) %*% x)/ dim(x)[1]
  e <- eigen(R, symmetric=T)
  pcMa <- abs(e$vectors)
  sqrtValue <- e$values^(0.5)
  pcVa <- e$values[1:10]/sum(e$values[1:10])
  #print(pcVa)
  weightTemp <- t(t(pcMa) / sqrtValue)[, 1:10]
  weight <- weightTemp %*% pcVa
  weight <- weight/sum(weight)
  return(weight)
}

  
##求权重以及得分

#构造权重矩阵
weight <- matrix(0 , ncol = 10, nrow  = 22)
colnames(weight) <- c(2018:2009)

weightObj <- matrix(0 , ncol = 10, nrow  = 22)
colnames(weightObj) <- c(2018:2009)

weightObj_cv <- matrix(0 , ncol = 10, nrow  = 22)
colnames(weightObj_cv) <- c(2018:2009)

#构造得分矩阵
score <- matrix(0 , ncol = 10, nrow  = 110)
colnames(score) <- c(2018:2009)

count = 0
for (y in c(2018:2009)) {
  count = count + 1
  tempdata <- subset(data, year == y)
  tempdata = tempdata[, -c(1:3)]
  tempData_up <- apply(tempdata[, -c(9, 10, 12, 17, 18)], 2, myStand)
  #正向
  tempData_down <- apply(tempdata[, c(9, 10, 12, 17, 18)], 2, downStand)
  #负向
  newData <- cbind(tempData_up, tempData_down)
  
  tempData_up2 <- apply(tempdata[, -c(9, 10, 12, 17, 18)], 2, upStand2 )
  tempData_down2 <- apply(tempdata[, c(9, 10, 12, 17, 18)], 2, downStand2)
  newData2 <- cbind(tempData_up2, tempData_down2)
  ## 计算权重
  w <- weightDemo(newData)
  #print(w)
  #变异系数法
  coefficient_variation_std_l <- apply(tempdata[, -c(9, 10, 12, 17, 18)], 2, sd)
  coefficient_variation_std_r <- apply(tempdata[, c(9, 10, 12, 17, 18)], 2, sd)
  coefficient_variation_std <- c(coefficient_variation_std_l, coefficient_variation_std_r)
  
  coefficient_variation_mean_l <- apply(tempdata[, -c(9, 10, 12, 17, 18)], 2, mean)
  coefficient_variation_mean_r <- apply(tempdata[, c(9, 10, 12, 17, 18)], 2, mean)
  coefficient_variation_mean <- c(coefficient_variation_mean_l, coefficient_variation_mean_r)
  
  mendW <- coefficient_variation_std/abs(coefficient_variation_mean)
  mendW <- mendW/sum(mendW)
  
  wave <- (w + mendW)/2
  #print(wave)
  
  #列中的所有行加总
  weightObj[, count] <- w
  weightObj_cv[, count] <- mendW
  weight[, count] <- wave
  #print(wave)
  
  ##计算得分
  s <- newData2 %*% wave
  score[, count] <- s
}

rownames(weightObj) <- colnames(newData)
rownames(weightObj_cv) <- colnames(newData)
rownames(weight) <- colnames(newData)
rownames(score) <- data[c(1:110), "city"]

##写出数据
  

write.csv(weight, "./output/weight1020.csv")
write.csv(weightObj_cv, "./output/weightObj_cv1020.csv")
write.csv(weightObj, "./output/weightObj1020.csv")
write.csv(score, "./output/score1020.csv")







