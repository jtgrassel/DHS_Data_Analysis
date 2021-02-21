# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#Establishing a baseline
#------ Majority Voting With Thresholding ------#
df0 <- filter(allAggData, GroundTruth==0)
df1 <- filter(allAggData, GroundTruth==1)

plotDfS <- NULL
for (t in seq(0,1,0.002)) { #thresholding
  fp <- sum(df0$Standard>t)/nrow(df0)
  fn <- sum(df1$Standard<t)/nrow(df1)
  temp <- data.frame(Threshold=t, FP=fp, FN=fn)
  plotDfS <- rbind(plotDfS, temp)
}
plotDfS$Err <- (plotDfS$FP+plotDfS$FN)/2

#standard plots
minVal <- min(plotDfS$Err)
p1 <- ggplot(data=plotDfS) +
  geom_line(aes(Threshold, Err),size=1)+
  geom_hline(yintercept=minVal,size=1,col="darkblue")+
  geom_text(aes(x=0.05,y=minVal-0.02,
                label=paste0("Ideal Error Rate: ",as.character(round(minVal,3)))),
            col="darkblue")+
  labs(title = "Standard Aggregation of Simple Votes") +
  ylab("")+
  ylim(0,0.5)+
  theme_classic()

p2 <- ggplot(data=plotDfS) +
  geom_line(aes(Threshold, FP),size=1,col="blue") +
  geom_text(aes(x=0.15,y=0.95,label="False Positive Rate"),col="blue")+
  geom_line(aes(Threshold, FN),size=1,col="red") +
  geom_text(aes(x=0.65,y=0.75,label="False Negative Rate"),col="red")+
  ylab("")+
  theme_classic()


#------ Confidence Weighted -----#
df0 <- filter(allAggData, GroundTruth==0)
df1 <- filter(allAggData, GroundTruth==1)

plotDfC <- NULL
for (t in seq(-100, 100, 1)) { #thresholding
  fp <- sum(df0$ConfWeight>t)/nrow(df0)
  fn <- sum(df1$ConfWeight<t)/nrow(df1)
  temp <- data.frame(Threshold=t, FP=fp, FN=fn)
  plotDfC <- rbind(plotDfC, temp)
}
plotDfC$Err <- (plotDfC$FP+plotDfC$FN)/2

#standard plots
minVal <- min(plotDfC$Err)

p3 <- ggplot(data=plotDfC) +
  geom_line(aes(Threshold, Err),size=1)+
  geom_hline(yintercept=minVal,size=1,col="darkblue")+
  geom_text(aes(x=0.05,y=minVal-0.02,
                label=paste0("Ideal Error Rate: ",as.character(round(minVal,3)))),
            col="darkblue")+
  labs(title = "Standard Aggregation of Simple Votes") +
  ylab("")+
  ylim(0,0.5)+
  theme_classic()

p4 <- ggplot(data=plotDfC) +
  geom_line(aes(Threshold, FP),size=1,col="blue") +
  geom_text(aes(x=0.15,y=0.95,label="False Positive Rate"),col="blue")+
  geom_line(aes(Threshold, FN),size=1,col="red") +
  geom_text(aes(x=0.65,y=0.75,label="False Negative Rate"),col="red")+
  ylab("")+
  theme_classic()



multiplot(p1,p2,p3,p4,cols=2)
