##################################################
######    Single Variable Models      ############
##################################################

#--------- GT~Standard ---------#
#trying multiple times
unknownSummaryDf <- data.frame()

for (i in seq(1:100)) {
  df <- allAggData
  
  set.seed(i)
  subsetIndex <- sample(seq(1:nrow(df)),0.5*nrow(df))
  
  trainDf <- df[subsetIndex,]
  unknownDf <- df[-subsetIndex,]
  
  #logistic regression model
  logitModel <- glm(
    factor(GroundTruth)~Standard, 
    data = trainDf, family = "binomial")
  
  unknownDf$logitPredict <- predict(logitModel,newdata=unknownDf,type="response")
  unknownDf <- mutate(unknownDf, logitCorrect=ifelse(ifelse(logitPredict>0.5,1,0)==GroundTruth,1,0))
  
  unknownSummaryDf <- rbind(unknownSummaryDf, unknownDf)
}

rfSummaryDf <- group_by(unknownSummaryDf, Test, Prompt, GroundTruth, PropCorrect) %>%
  summarise(logitProb=mean(logitPredict))

df0 <- filter(rfSummaryDf, GroundTruth==0)
df1 <- filter(rfSummaryDf, GroundTruth==1)

plotDfS <- NULL
for (t in seq(0,1,0.005)) { #thresholding
  fp <- sum(df0$logitProb>t)/nrow(df0)
  fn <- sum(df1$logitProb<t)/nrow(df1)
  temp <- data.frame(Threshold=t, FP=fp, FN=fn)
  plotDfS <- rbind(plotDfS, temp)
}
plotDfS$Err <- (plotDfS$FP+plotDfS$FN)/2

#standard plots
minVal0 <- min(plotDfS$Err)
p1 <- ggplot(data=plotDfS) +
  geom_line(aes(Threshold, Err),size=1)+
  geom_hline(yintercept=minVal0,size=1,col="darkblue")+
  geom_text(aes(x=0.15,y=minVal0-0.02,
                label=paste0("Ideal Error Rate: ",as.character(round(minVal0,4)))),
            col="darkblue")+
  labs(title = "Logistic Regression: GT~Standard") +
  ylab("")+
  ylim(0,0.5)+
  theme_classic()

p2 <- ggplot(data=plotDfS) +
  geom_line(aes(Threshold, FP),size=1,col="blue") +
  geom_text(aes(x=0.19,y=0.95,label="False Positive Rate"),col="blue")+
  geom_line(aes(Threshold, FN),size=1,col="red") +
  geom_text(aes(x=0.65,y=0.75,label="False Negative Rate"),col="red")+
  ylab("")+
  theme_classic()

#--------- GT~ConfWeight ---------#
#trying multiple times
unknownSummaryDf <- data.frame()

for (i in seq(1:100)) {
  df <- allAggData
  
  set.seed(i)
  subsetIndex <- sample(seq(1:nrow(df)),0.5*nrow(df))
  
  trainDf <- df[subsetIndex,]
  unknownDf <- df[-subsetIndex,]
  
  #logistic regression model
  logitModel <- glm(
    factor(GroundTruth)~ConfWeight, 
    data = trainDf, family = "binomial")
  
  unknownDf$logitPredict <- predict(logitModel,newdata=unknownDf,type="response")
  unknownDf <- mutate(unknownDf, logitCorrect=ifelse(ifelse(logitPredict>0.5,1,0)==GroundTruth,1,0))
  
  unknownSummaryDf <- rbind(unknownSummaryDf, unknownDf)
}

rfSummaryDf <- group_by(unknownSummaryDf, Test, Prompt, GroundTruth, PropCorrect) %>%
  summarise(logitProb=mean(logitPredict))

df0 <- filter(rfSummaryDf, GroundTruth==0)
df1 <- filter(rfSummaryDf, GroundTruth==1)

plotDfS <- NULL
for (t in seq(0,1,0.005)) { #thresholding
  fp <- sum(df0$logitProb>t)/nrow(df0)
  fn <- sum(df1$logitProb<t)/nrow(df1)
  temp <- data.frame(Threshold=t, FP=fp, FN=fn)
  plotDfS <- rbind(plotDfS, temp)
}
plotDfS$Err <- (plotDfS$FP+plotDfS$FN)/2

#standard plots
minVal1 <- min(plotDfS$Err)
p3 <- ggplot(data=plotDfS) +
  geom_line(aes(Threshold, Err),size=1)+
  geom_hline(yintercept=minVal1,size=1,col="darkblue")+
  geom_text(aes(x=0.15,y=minVal1-0.02,
                label=paste0("Ideal Error Rate: ",as.character(round(minVal1,4)))),
            col="darkblue")+
  labs(title = "Logistic Regression: GT~ConfWeighted") +
  ylab("")+
  ylim(0,0.5)+
  theme_classic()

p4 <- ggplot(data=plotDfS) +
  geom_line(aes(Threshold, FP),size=1,col="blue") +
  geom_text(aes(x=0.19,y=0.95,label="False Positive Rate"),col="blue")+
  geom_line(aes(Threshold, FN),size=1,col="red") +
  geom_text(aes(x=0.65,y=0.75,label="False Negative Rate"),col="red")+
  ylab("")+
  theme_classic()



#--------- GT~NormConfWeight ---------#
#trying multiple times
unknownSummaryDf <- data.frame()

for (i in seq(1:100)) {
  df <- allAggData
  
  set.seed(i)
  subsetIndex <- sample(seq(1:nrow(df)),0.5*nrow(df))
  
  trainDf <- df[subsetIndex,]
  unknownDf <- df[-subsetIndex,]
  
  #logistic regression model
  logitModel <- glm(
    factor(GroundTruth)~LinMapConfWeight, 
    data = trainDf, family = "binomial")
  
  unknownDf$logitPredict <- predict(logitModel,newdata=unknownDf,type="response")
  unknownDf <- mutate(unknownDf, logitCorrect=ifelse(ifelse(logitPredict>0.5,1,0)==GroundTruth,1,0))
  
  unknownSummaryDf <- rbind(unknownSummaryDf, unknownDf)
}

rfSummaryDf <- group_by(unknownSummaryDf, Test, Prompt, GroundTruth, PropCorrect) %>%
  summarise(logitProb=mean(logitPredict))

df0 <- filter(rfSummaryDf, GroundTruth==0)
df1 <- filter(rfSummaryDf, GroundTruth==1)

plotDfS <- NULL
for (t in seq(0,1,0.005)) { #thresholding
  fp <- sum(df0$logitProb>t)/nrow(df0)
  fn <- sum(df1$logitProb<t)/nrow(df1)
  temp <- data.frame(Threshold=t, FP=fp, FN=fn)
  plotDfS <- rbind(plotDfS, temp)
}
plotDfS$Err <- (plotDfS$FP+plotDfS$FN)/2

#standard plots
minVal2 <- min(plotDfS$Err)
p5 <- ggplot(data=plotDfS) +
  geom_line(aes(Threshold, Err),size=1)+
  geom_hline(yintercept=minVal2,size=1,col="darkblue")+
  geom_text(aes(x=0.15,y=minVal2-0.02,
                label=paste0("Ideal Error Rate: ",as.character(round(minVal2,4)))),
            col="darkblue")+
  labs(title = "Logistic Regression: GT~NormConfWeighted") +
  ylab("")+
  ylim(0,0.5)+
  theme_classic()

p6 <- ggplot(data=plotDfS) +
  geom_line(aes(Threshold, FP),size=1,col="blue") +
  geom_text(aes(x=0.19,y=0.95,label="False Positive Rate"),col="blue")+
  geom_line(aes(Threshold, FN),size=1,col="red") +
  geom_text(aes(x=0.65,y=0.75,label="False Negative Rate"),col="red")+
  ylab("")+
  theme_classic()


multiplot(p1,p2,p3,p4,p5,p6,cols=3)

##################################################
######    Multi Variable Models      #############
##################################################

#--------- GT~Standard+ConfWeight+ConfVar+linDiff+AverageTime+SPmean ---------#
#trying multiple times
unknownSummaryDf <- data.frame()

for (i in seq(1:100)) {
  df <- allAggData
  
  set.seed(i)
  subsetIndex <- sample(seq(1:nrow(df)),0.5*nrow(df))
  
  trainDf <- df[subsetIndex,]
  unknownDf <- df[-subsetIndex,]
  
  #logistic regression model
  logitModel <- glm(
    factor(GroundTruth)~Standard+ConfWeight+ConfVar+linDiff+AverageTime+SPmean, 
    data = trainDf, family = "binomial")
  
  unknownDf$logitPredict <- predict(logitModel,newdata=unknownDf,type="response")
  unknownDf <- mutate(unknownDf, logitCorrect=ifelse(ifelse(logitPredict>0.5,1,0)==GroundTruth,1,0))
  
  unknownSummaryDf <- rbind(unknownSummaryDf, unknownDf)
}

rfSummaryDf <- group_by(unknownSummaryDf, Test, Prompt, GroundTruth, PropCorrect) %>%
  summarise(logitProb=mean(logitPredict))

df0 <- filter(rfSummaryDf, GroundTruth==0)
df1 <- filter(rfSummaryDf, GroundTruth==1)

plotDfS <- NULL
for (t in seq(0,1,0.005)) { #thresholding
  fp <- sum(df0$logitProb>t)/nrow(df0)
  fn <- sum(df1$logitProb<t)/nrow(df1)
  temp <- data.frame(Threshold=t, FP=fp, FN=fn)
  plotDfS <- rbind(plotDfS, temp)
}
plotDfS$Err <- (plotDfS$FP+plotDfS$FN)/2

#standard plots
minVal0 <- min(plotDfS$Err)
p1 <- ggplot(data=plotDfS) +
  geom_line(aes(Threshold, Err),size=1)+
  geom_hline(yintercept=minVal0,size=1,col="darkblue")+
  geom_text(aes(x=0.15,y=minVal0-0.02,
                label=paste0("Ideal Error Rate: ",as.character(round(minVal0,4)))),
            col="darkblue")+
  labs(title = "Logistic Regression: GT~Standard+ConfWeight+ConfVar+linDiff+AverageTime+SPmean") +
  ylab("")+
  ylim(0,0.5)+
  theme_classic()

p2 <- ggplot(data=plotDfS) +
  geom_line(aes(Threshold, FP),size=1,col="blue") +
  geom_text(aes(x=0.19,y=0.95,label="False Positive Rate"),col="blue")+
  geom_line(aes(Threshold, FN),size=1,col="red") +
  geom_text(aes(x=0.65,y=0.75,label="False Negative Rate"),col="red")+
  ylab("")+
  theme_classic()

#--------- GT~Standard+SPmean ---------#
#trying multiple times
unknownSummaryDf <- data.frame()

for (i in seq(1:100)) {
  df <- allAggData
  
  set.seed(i)
  subsetIndex <- sample(seq(1:nrow(df)),0.5*nrow(df))
  
  trainDf <- df[subsetIndex,]
  unknownDf <- df[-subsetIndex,]
  
  #logistic regression model
  logitModel <- glm(
    factor(GroundTruth)~Standard+SPmean, 
    data = trainDf, family = "binomial")
  
  unknownDf$logitPredict <- predict(logitModel,newdata=unknownDf,type="response")
  unknownDf <- mutate(unknownDf, logitCorrect=ifelse(ifelse(logitPredict>0.5,1,0)==GroundTruth,1,0))
  
  unknownSummaryDf <- rbind(unknownSummaryDf, unknownDf)
}

rfSummaryDf <- group_by(unknownSummaryDf, Test, Prompt, GroundTruth, PropCorrect) %>%
  summarise(logitProb=mean(logitPredict))

df0 <- filter(rfSummaryDf, GroundTruth==0)
df1 <- filter(rfSummaryDf, GroundTruth==1)

plotDfS <- NULL
for (t in seq(0,1,0.005)) { #thresholding
  fp <- sum(df0$logitProb>t)/nrow(df0)
  fn <- sum(df1$logitProb<t)/nrow(df1)
  temp <- data.frame(Threshold=t, FP=fp, FN=fn)
  plotDfS <- rbind(plotDfS, temp)
}
plotDfS$Err <- (plotDfS$FP+plotDfS$FN)/2

#standard plots
minVal1 <- min(plotDfS$Err)
p3 <- ggplot(data=plotDfS) +
  geom_line(aes(Threshold, Err),size=1)+
  geom_hline(yintercept=minVal1,size=1,col="darkblue")+
  geom_text(aes(x=0.15,y=minVal1-0.02,
                label=paste0("Ideal Error Rate: ",as.character(round(minVal1,4)))),
            col="darkblue")+
  labs(title = "Logistic Regression: GT~Standard+SPmean") +
  ylab("")+
  ylim(0,0.5)+
  theme_classic()

p4 <- ggplot(data=plotDfS) +
  geom_line(aes(Threshold, FP),size=1,col="blue") +
  geom_text(aes(x=0.19,y=0.95,label="False Positive Rate"),col="blue")+
  geom_line(aes(Threshold, FN),size=1,col="red") +
  geom_text(aes(x=0.65,y=0.75,label="False Negative Rate"),col="red")+
  ylab("")+
  theme_classic()



#--------- GT~Standard+ConfSqWeight+ConfSqVar+linDiff ---------#
#trying multiple times
unknownSummaryDf <- data.frame()

for (i in seq(1:100)) {
  df <- allAggData
  
  set.seed(i)
  subsetIndex <- sample(seq(1:nrow(df)),0.5*nrow(df))
  
  trainDf <- df[subsetIndex,]
  unknownDf <- df[-subsetIndex,]
  
  #logistic regression model
  logitModel <- glm(
    factor(GroundTruth)~Standard+ConfSqWeight+ConfSqVar+linDiff, 
    data = trainDf, family = "binomial")
  
  unknownDf$logitPredict <- predict(logitModel,newdata=unknownDf,type="response")
  unknownDf <- mutate(unknownDf, logitCorrect=ifelse(ifelse(logitPredict>0.5,1,0)==GroundTruth,1,0))
  
  unknownSummaryDf <- rbind(unknownSummaryDf, unknownDf)
}

rfSummaryDf <- group_by(unknownSummaryDf, Test, Prompt, GroundTruth, PropCorrect) %>%
  summarise(logitProb=mean(logitPredict))

df0 <- filter(rfSummaryDf, GroundTruth==0)
df1 <- filter(rfSummaryDf, GroundTruth==1)

plotDfS <- NULL
for (t in seq(0,1,0.005)) { #thresholding
  fp <- sum(df0$logitProb>t)/nrow(df0)
  fn <- sum(df1$logitProb<t)/nrow(df1)
  temp <- data.frame(Threshold=t, FP=fp, FN=fn)
  plotDfS <- rbind(plotDfS, temp)
}
plotDfS$Err <- (plotDfS$FP+plotDfS$FN)/2

#standard plots
minVal2 <- min(plotDfS$Err)
p5 <- ggplot(data=plotDfS) +
  geom_line(aes(Threshold, Err),size=1)+
  geom_hline(yintercept=minVal2,size=1,col="darkblue")+
  geom_text(aes(x=0.15,y=minVal2-0.02,
                label=paste0("Ideal Error Rate: ",as.character(round(minVal2,4)))),
            col="darkblue")+
  labs(title = "Logistic Regression: GT~Standard+ConfSqWeight+ConfSqVar+linDiff") +
  ylab("")+
  ylim(0,0.5)+
  theme_classic()

p6 <- ggplot(data=plotDfS) +
  geom_line(aes(Threshold, FP),size=1,col="blue") +
  geom_text(aes(x=0.19,y=0.95,label="False Positive Rate"),col="blue")+
  geom_line(aes(Threshold, FN),size=1,col="red") +
  geom_text(aes(x=0.65,y=0.75,label="False Negative Rate"),col="red")+
  ylab("")+
  theme_classic()


multiplot(p1,p2,p3,p4,p5,p6,cols=3)

##################################################
######     Models with XY Clustering      ########
##################################################

#--------- GT~dsPropMain+dsPropNoise ---------#
unknownSummaryDf <- data.frame()

for (i in seq(1:100)) {
  df <- filter(allAggData, Test %in% c("3_1","3_2","3_3"))
  
  set.seed(i)
  subsetIndex <- sample(seq(1:nrow(df)),0.7*nrow(df))
  
  trainDf <- df[subsetIndex,]
  unknownDf <- df[-subsetIndex,]
  
  #logistic regression model
  logitModel <- glm(
    factor(GroundTruth)~dsPropMain+dsPropNoise, 
    data = trainDf, family = "binomial")
  
  unknownDf$logitPredict <- predict(logitModel,newdata=unknownDf,type="response")
  unknownDf <- mutate(unknownDf, logitCorrect=ifelse(ifelse(logitPredict>0.5,1,0)==GroundTruth,1,0))
  
  unknownSummaryDf <- rbind(unknownSummaryDf, unknownDf)
}

rfSummaryDf <- group_by(unknownSummaryDf, Test, Prompt, GroundTruth, PropCorrect) %>%
  summarise(logitProb=mean(logitPredict))

df0 <- filter(rfSummaryDf, GroundTruth==0)
df1 <- filter(rfSummaryDf, GroundTruth==1)

plotDfS <- NULL
for (t in seq(0,1,0.005)) { #thresholding
  fp <- sum(df0$logitProb>t)/nrow(df0)
  fn <- sum(df1$logitProb<t)/nrow(df1)
  temp <- data.frame(Threshold=t, FP=fp, FN=fn)
  plotDfS <- rbind(plotDfS, temp)
}
plotDfS$Err <- (plotDfS$FP+plotDfS$FN)/2

#standard plots
minVal0 <- min(plotDfS$Err)
p1 <- ggplot(data=plotDfS) +
  geom_line(aes(Threshold, Err),size=1)+
  geom_hline(yintercept=minVal0,size=1,col="darkblue")+
  geom_text(aes(x=0.15,y=minVal0-0.02,
                label=paste0("Ideal Error Rate: ",as.character(round(minVal0,4)))),
            col="darkblue")+
  labs(title = "Logistic Regression: GT~dsPropMain+dsPropNoise") +
  ylab("")+
  ylim(0,0.5)+
  theme_classic()

p2 <- ggplot(data=plotDfS) +
  geom_line(aes(Threshold, FP),size=1,col="blue") +
  geom_text(aes(x=0.19,y=0.95,label="False Positive Rate"),col="blue")+
  geom_line(aes(Threshold, FN),size=1,col="red") +
  geom_text(aes(x=0.65,y=0.75,label="False Negative Rate"),col="red")+
  ylab("")+
  theme_classic()

#--------- GT~ConfWeight+dsPropMain+dsPropNoise ---------#
#trying multiple times
unknownSummaryDf <- data.frame()

for (i in seq(1:100)) {
  df <- filter(allAggData, Test %in% c("3_1","3_2","3_3"))
  
  set.seed(i)
  subsetIndex <- sample(seq(1:nrow(df)),0.7*nrow(df))
  
  trainDf <- df[subsetIndex,]
  unknownDf <- df[-subsetIndex,]
  
  #logistic regression model
  logitModel <- glm(
    factor(GroundTruth)~ConfWeight+dsPropMain+dsPropNoise, 
    data = trainDf, family = "binomial")
  
  unknownDf$logitPredict <- predict(logitModel,newdata=unknownDf,type="response")
  unknownDf <- mutate(unknownDf, logitCorrect=ifelse(ifelse(logitPredict>0.5,1,0)==GroundTruth,1,0))
  
  unknownSummaryDf <- rbind(unknownSummaryDf, unknownDf)
}

rfSummaryDf <- group_by(unknownSummaryDf, Test, Prompt, GroundTruth, PropCorrect) %>%
  summarise(logitProb=mean(logitPredict))

df0 <- filter(rfSummaryDf, GroundTruth==0)
df1 <- filter(rfSummaryDf, GroundTruth==1)

plotDfS <- NULL
for (t in seq(0,1,0.005)) { #thresholding
  fp <- sum(df0$logitProb>t)/nrow(df0)
  fn <- sum(df1$logitProb<t)/nrow(df1)
  temp <- data.frame(Threshold=t, FP=fp, FN=fn)
  plotDfS <- rbind(plotDfS, temp)
}
plotDfS$Err <- (plotDfS$FP+plotDfS$FN)/2

#standard plots
minVal1 <- min(plotDfS$Err)
p3 <- ggplot(data=plotDfS) +
  geom_line(aes(Threshold, Err),size=1)+
  geom_hline(yintercept=minVal1,size=1,col="darkblue")+
  geom_text(aes(x=0.15,y=minVal1-0.02,
                label=paste0("Ideal Error Rate: ",as.character(round(minVal1,4)))),
            col="darkblue")+
  labs(title = "Logistic Regression: GT~ConfWeight+dsPropMain+dsPropNoise") +
  ylab("")+
  ylim(0,0.5)+
  theme_classic()

p4 <- ggplot(data=plotDfS) +
  geom_line(aes(Threshold, FP),size=1,col="blue") +
  geom_text(aes(x=0.19,y=0.95,label="False Positive Rate"),col="blue")+
  geom_line(aes(Threshold, FN),size=1,col="red") +
  geom_text(aes(x=0.65,y=0.75,label="False Negative Rate"),col="red")+
  ylab("")+
  theme_classic()



#--------- GT~ConfWeight+ConfVar+linDiff+dsPropMain+dsPropNoise ---------#
#trying multiple times
unknownSummaryDf <- data.frame()

for (i in seq(1:100)) {
  df <- filter(allAggData, Test %in% c("3_1","3_2","3_3"))
  
  set.seed(i)
  subsetIndex <- sample(seq(1:nrow(df)),0.7*nrow(df))
  
  trainDf <- df[subsetIndex,]
  unknownDf <- df[-subsetIndex,]
  
  #logistic regression model
  logitModel <- glm(
    factor(GroundTruth)~ConfWeight+ConfVar+linDiff+dsPropMain+dsPropNoise, 
    data = trainDf, family = "binomial")
  
  unknownDf$logitPredict <- predict(logitModel,newdata=unknownDf,type="response")
  unknownDf <- mutate(unknownDf, logitCorrect=ifelse(ifelse(logitPredict>0.5,1,0)==GroundTruth,1,0))
  
  unknownSummaryDf <- rbind(unknownSummaryDf, unknownDf)
}

rfSummaryDf <- group_by(unknownSummaryDf, Test, Prompt, GroundTruth, PropCorrect) %>%
  summarise(logitProb=mean(logitPredict))

df0 <- filter(rfSummaryDf, GroundTruth==0)
df1 <- filter(rfSummaryDf, GroundTruth==1)

plotDfS <- NULL
for (t in seq(0,1,0.005)) { #thresholding
  fp <- sum(df0$logitProb>t)/nrow(df0)
  fn <- sum(df1$logitProb<t)/nrow(df1)
  temp <- data.frame(Threshold=t, FP=fp, FN=fn)
  plotDfS <- rbind(plotDfS, temp)
}
plotDfS$Err <- (plotDfS$FP+plotDfS$FN)/2

#standard plots
minVal2 <- min(plotDfS$Err)
p5 <- ggplot(data=plotDfS) +
  geom_line(aes(Threshold, Err),size=1)+
  geom_hline(yintercept=minVal2,size=1,col="darkblue")+
  geom_text(aes(x=0.15,y=minVal2-0.02,
                label=paste0("Ideal Error Rate: ",as.character(round(minVal2,4)))),
            col="darkblue")+
  labs(title = "Logistic Regression: GT~ConfWeight+ConfVar+linDiff+dsPropMain+dsPropNoise") +
  ylab("")+
  ylim(0,0.5)+
  theme_classic()

p6 <- ggplot(data=plotDfS) +
  geom_line(aes(Threshold, FP),size=1,col="blue") +
  geom_text(aes(x=0.19,y=0.95,label="False Positive Rate"),col="blue")+
  geom_line(aes(Threshold, FN),size=1,col="red") +
  geom_text(aes(x=0.65,y=0.75,label="False Negative Rate"),col="red")+
  ylab("")+
  theme_classic()


multiplot(p1,p2,p3,p4,p5,p6,cols=3)