#####空间计量#####


####导包####

library(spdep)
library(sp)
library(rgdal)
library(rgeos)



####方法区####

citysort <- function(cityname, refername) {
  nameList = c()
  for (i in 1:length(refername)) {
    num = which(refername[i] == cityname)
    nameList <- c(nameList, num)
  }
  
  return(nameList)
}



####输入及整理数据####

data <- read.csv("score.csv")
head(data)
rownames(data) <- data$X
data <- data[, -1]
colnames(data) <- c(2009:2018)

LandL <- read.csv("newLL.csv")
str(data)

colnames(LandL) <- c("city", "latitude", "longitude")
LandL <- LandL[citysort(LandL$city, rownames(data)), ]

head(LandL)

newLL <- as.data.frame(cbind(LandL$longitude, LandL$latitude))  

rownames(newLL) <- LandL$city
colnames(newLL) <- c("longitude", "latitude")




####空间相关性####




spt <- SpatialPoints(newLL)
spt_df <- SpatialPointsDataFrame(spt, data)
plot(spt)

dfnb <- knn2nb(knearneigh(spt_df, k = 4, longlat = T))
dfnb_s <- make.sym.nb(dfnb)
#col_W <- nb2listw(dfnb_s, style = "W")
#moi <- moran(spt_df$`2009`, col_W, length(spt_df$`2009`),
#             Szero(col_W))


#moran.test(spt_df$`2009`, listw = nb2listw(dfnb_s), randomisation = F)


#yearName <- c(2009:2018)

moiMat <- matrix(0, ncol = 5, nrow = 10)
colnames(moiMat) <- c("I", "Expectation", "Variance", 
                      "standard deviate", "p-value")
rownames(moiMat) <- c(2009:2018)

for (i in c(1:10)) {
  moi_test <- moran.test(data[, i], listw = nb2listw(dfnb_s))
  testL <- c(moi_test$estimate, moi_test$statistic, moi_test$p.value)
  moiMat[i, ] <- testL

}


####数据导出####
write.csv(moiMat, "./output/moiMat.csv")

