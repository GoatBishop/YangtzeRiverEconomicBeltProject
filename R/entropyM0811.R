#####熵值法#####

####自定义方法####

upStand <- function(x) {
  newx = (x-min(x))/(max(x)-min(x))
  return(newx)
}

downStand  <- function(x) {
  newx = (max(x)-x)/(max(x)-min(x))
  return(newx)
}

proportion <- function(data) {
  x <- c(data)
  for(i in 1:length(data))
    x[i] = data[i]/sum(data[])
  return(x)
}

entropy <- function(data) {
  x <- c(data)
  for(i in 1:length(data)){
    if(data[i] == 0){
      x[i] = 0
    }else{
      x[i] = data[i] * log(data[i])
    }
  }
  return(x)
}

####读取####
data <- read.csv("data0811.csv")
head(data)
str(data)
dim(data) #350, 21
names(data)[-c(1:3)]

####熵值法求权重并计算得分####

#构造权重矩阵
weight = matrix(0 , ncol = 14, nrow  = 18)
colnames(weight) <- c(2018:2005)

#构造得分矩阵
score <- matrix(0 , ncol = 14, nrow  = 25)
colnames(score) <- c(2018:2005)

count = 0
for (y in c(2018:2005)) {
  count = count + 1
  tempdata <- subset(data, year == y)
  tempdata = tempdata[, -c(1:3)]
  tempData_up <- apply(tempdata[, -c(12, 13, 15)], 2, upStand)
  tempData_down <- apply(tempdata[, c(12, 13, 15)], 2, downStand)
  newData <- cbind(tempData_up, tempData_down)
  
  ## 计算权重
  myproportion <- apply(newData, 2, proportion)
  myentropy = apply(myproportion, 2, entropy)
  k <- 1/log(length(myentropy[, 1]))
  d <- (-k)*colSums(myentropy)
  #列中的所有行加总
  d <- 1 - d
  w <- d/sum(d)
  weight[, count] <- w
  
  ##计算得分
  s <- newData %*% w
  score[, count] <- s
  
}

rownames(weight) <- colnames(newData)
rownames(score) <- data[c(1:25), "city"]

####输出结果####

write.csv(weight, "output/weightNew.csv")
write.csv(score, "output/scoreCityYearNew.csv")

