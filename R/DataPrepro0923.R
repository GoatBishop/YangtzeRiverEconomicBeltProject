
####数据清理####


####导包####

library(VIM)
library(mice)
library(readr)
library(psych)
library(fpc)
library(lattice)
library(MASS)


####读取数据####

mydata <- read.csv("RawData0923.csv")
head(mydata)
str(mydata)

####整理数据####
#将数据中有#DIV/0!的设置为缺失值

mydata2 <- mydata


for (item in 2:length(mydata2)) {
  data_type <- class(mydata2[, item])
  if (data_type == "character") {
    temp_dataL <- mydata2[, item]
    temp_dataL[which(temp_dataL == "#DIV/0!")] = NA
    mydata2[, item] <- as.numeric(temp_dataL)
  }
}

str(mydata)
str(mydata2)


####绘制缺失值图####

png("images/缺失值图1.png")
aggr(mydata2, prop = F, numbers = T)
dev.off()

