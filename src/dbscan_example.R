library(dbscan)

#-----
temp <- filter(allData, Test=="3_3", Prompt==5, Spammer==0)
temp <- na.omit(temp)

ds.temp <- dbscan(temp[c("x","y")], eps = 25, minPts = 3)
print(ds.temp)

temp$ds_cluster <- ds.temp$cluster

ggplot() +
  geom_point(data=temp, mapping=aes(x=x, y=y, color=factor(ds_cluster)), size=4, alpha=.5) +
  xlim(c(0, 500)) +
  ylim(c(0, 500))