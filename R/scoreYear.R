####计算每年得分#####

####自定义方法####

upStand <- function(x) {
  newx = (x-min(x))/(max(x)-min(x))
  return(newx)
}

downStand  <- function(x) {
  newx = (max(x)-x)/(max(x)-min(x))
  return(newx)
}


####读取####

data <- read.csv("data0811.csv")
weight <- read.csv("weightNew.csv")[, -c(1)]
#weight

####计算平均权重####

weightAve <- apply(weight, 1, mean)

weight <- weightAve/sum(weightAve)
#平均权重
weight


####计算得分####
score = matrix(0 , ncol = 18, nrow  = 14)
rownames(score) <- c(2018:2005)


cityname <- unique(data$city)

for (c in c(1:length(cityname))) {
  tempdata <- subset(data, city == cityname[c])
  #print(cityname[c])
  tempdata <- tempdata[, -c(1:3)]
  tempData_up <- apply(tempdata[, -c(12, 13, 15)], 2, upStand)
  tempData_down <- apply(tempdata[, c(12, 13, 15)], 2, downStand)
  newData <- cbind(tempData_up, tempData_down)
  
  #计算得分
  s <- t(t(newData) * weight)
  score <- score + s
}

score <- score/25


####输出####
write.csv(score, "output/scoreYearNew.csv")


