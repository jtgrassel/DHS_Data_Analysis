#------ Majority Voting With Thresholding ------#
df0 <- filter(allAggData, GroundTruth==0)
df1 <- filter(allAggData, GroundTruth==1)

plotDfS <- NULL
for (t in seq(0,1,0.005)) { #thresholding
  fp <- sum(df0$Standard>t)/nrow(df0)
  fn <- sum(df1$Standard<t)/nrow(df1)
  temp <- data.frame(Threshold=t, FP=fp, FN=fn)
  plotDfS <- rbind(plotDfS, temp)
}
plotDfS$Err <- (plotDfS$FP+plotDfS$FN)/2

#standard plots
minValS <- min(plotDfS$Err)
p1 <- ggplot(data=plotDfS) +
  geom_line(aes(Threshold, Err),size=1)+
  geom_hline(yintercept=minValS,size=1,col="darkblue")+
  geom_text(aes(x=0.15,y=minValS-0.02,
                label=paste0("Minimum Error Rate: ",as.character(round(minValS,4)*100),"%")),
            col="darkblue",
            size=4.5)+
  geom_point(aes(x=Threshold[Err==minValS][1],y=minValS[1]),size=2,col="darkblue") +
  geom_vline(xintercept = 0.5,linetype="dashed")+
  labs(title = "Standard Aggregation of Simple Votes") +
  ylab("")+
  ylim(0,0.5)+
  theme_classic()


p2 <- ggplot(data=plotDfS) +
  geom_line(aes(Threshold, FP),size=1,col="blue") +
  geom_text(aes(x=0.19,y=0.95,label="False Positive Rate"),col="blue")+
  geom_line(aes(Threshold, FN),size=1,col="red") +
  geom_text(aes(x=0.8,y=0.65,label="False Negative Rate"),col="red")+
  geom_text(aes(x=0.58,y=0.85),label="Majority Voting")+
  geom_vline(xintercept = 0.5,linetype="dashed")+
  ylab("")+
  theme_classic()

p2

## --- Normalized Conf Weighted
df0 <- filter(allAggData, GroundTruth==0)
df1 <- filter(allAggData, GroundTruth==1)

plotDfC <- NULL
for (t in seq(-100, 100, 2)) { #thresholding
  fp <- sum(df0$LinMapConfWeight>t)/nrow(df0)
  fn <- sum(df1$LinMapConfWeight<t)/nrow(df1)
  temp <- data.frame(Threshold=t, FP=fp, FN=fn)
  plotDfC <- rbind(plotDfC, temp)
}
plotDfC$Err <- (plotDfC$FP+plotDfC$FN)/2

#standard plots
minValNC <- min(plotDfC$Err)

p5 <- ggplot(data=plotDfC) +
  geom_line(aes(Threshold, Err),size=1)+
  geom_hline(yintercept=minValNC,size=1,col="darkblue")+
  geom_text(aes(x=-60,y=minValNC-.02,
                label=paste0("Ideal Error Rate: ",as.character(round(minValNC,4)))),
            col="darkblue")+
  labs(title = "Standard Aggregation of Normalized Confidence Weighted Votes") +
  geom_vline(xintercept = 0,linetype="dashed")+
  ylab("")+
  ylim(0,0.5)+
  theme_classic()


p6 <- ggplot(data=plotDfC) +
  geom_line(aes(Threshold, FP),size=1,col="blue") +
  geom_text(aes(x=-35,y=0.95,label="False Positive Rate"),col="blue")+
  geom_line(aes(Threshold, FN),size=1,col="red") +
  geom_text(aes(x=25,y=0.75,label="False Negative Rate"),col="red")+
  geom_vline(xintercept = 0,linetype="dashed")+
  ylab("")+
  theme_classic()
