library(dbscan)

temp <- filter(allData, Test=="3_2", Prompt==8, Spammer==0)
ggplot() +
  geom_point(data=temp, mapping=aes(x=x, y=y)) +
  xlim(c(0, 500)) +
  ylim(c(0, 500))


#-----
temp <- filter(allData, Test=="3_2", Prompt==18, Spammer==0)
temp <- na.omit(temp)

ds.temp <- dbscan(temp[c("x","y")], eps = 25, minPts = 3)
print(ds.temp)

temp$ds_cluster <- ds.temp$cluster

ggplot() +
  geom_point(data=temp, mapping=aes(x=x, y=y, color=factor(ds_cluster)), size=4, alpha=.5) +
  xlim(c(0, 500)) +
  ylim(c(0, 500))





#k-means clustering
library(factoextra)
km.temp <- kmeans(tempLim[c("x","y")], 1, nstart = 25)
print(km.temp)

centersData <- data.frame(km.temp$centers)

ggplot() +
  geom_point(data=tempLim, mapping=aes(x=x, y=y)) +
  geom_point(data=centersData, mapping=aes(x=x, y=y),color="blue") +
  xlim(c(0, 500)) +
  ylim(c(0, 500))
