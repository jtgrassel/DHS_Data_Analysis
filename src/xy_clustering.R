library(factoextra)
library(dbscan)

temp <- filter(allData, Test=="3_1", Spammer==0)
tempLim <- filter(temp, Prompt==2)
tempLim <- na.omit(tempLim)

ds.temp <- dbscan(tempLim[c("x","y")], eps = 40, minPts = 2)
print(ds.temp)

tempLim$ds_cluster <- ds.temp$cluster

ggplot() +
  geom_point(data=tempLim, mapping=aes(x=x, y=y, color=factor(ds_cluster))) +
  xlim(c(0, 500)) +
  ylim(c(0, 500))



#k-means clustering
km.temp <- kmeans(tempLim[c("x","y")], 1, nstart = 25)
print(km.temp)

centersData <- data.frame(km.temp$centers)

ggplot() +
  geom_point(data=tempLim, mapping=aes(x=x, y=y)) +
  geom_point(data=centersData, mapping=aes(x=x, y=y),color="blue") +
  xlim(c(0, 500)) +
  ylim(c(0, 500))
