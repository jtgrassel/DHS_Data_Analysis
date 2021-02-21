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

rfSummaryDf <- group_by(unknownSummaryDf, Test, Prompt, PropCorrect) %>%
  summarise(rfCorrect=mean(rfCorrect),logitCorrect=mean(logitCorrect))

view(rfSummaryDf)