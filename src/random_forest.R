#library(tidyverse)
#library(randomForest)

treeData <- allAggData

#set.seed(1)
train = sample(1:nrow(treeData), 1*nrow(treeData))

rf.aggData = randomForest(factor(GroundTruth)~LinMapConfWeight+Standard+ConfVar+SPmean+linDiff+AverageTime, 
                          data = treeData, subset = train, ntree=1000, mtry=3)
rf.aggData

treeData$predicted <- predict(rf.aggData, treeData)
treeData$predictCorrect <- ifelse(treeData$predicted==treeData$GroundTruth, 1, 0)
varImpPlot(rf.aggData)




#Try to use logistic regression to get a good starting prediction
df <- allAggData

set.seed(1)
subsetIndex <- sample(seq(1:nrow(df)),0.5*nrow(df))

trainDf <- df[subsetIndex,]
unknownDf <- df[-subsetIndex,]

logitModel <- glm(GroundTruth ~ LinMapConfWeight, data=trainDf, family="binomial")
summary(logitModel)

trainDf$logitProb <- predict(logitModel,newdata=trainDf,type="response")
unknownDf$logitProb <- predict(logitModel,newdata=unknownDf,type="response")

rf.trainDf <- randomForest(
  factor(GroundTruth)~logitProb,
  data = trainDf, ntree=1000, mtry=1)

rf.trainDf

trainDf$rfPredict <- predict(rf.trainDf, trainDf)
unknownDf$rfPredict <- predict(rf.trainDf, unknownDf)

trainDf <- mutate(trainDf, rfCorrect=ifelse(rfPredict==GroundTruth, 1, 0))
unknownDf <- mutate(unknownDf, rfCorrect=ifelse(rfPredict==GroundTruth, 1, 0))

#trying multiple times
trainSummaryDf <- data.frame()
unknownSummaryDf <- data.frame()

for (i in seq(1:100)) {
  df <- allAggData
  
  set.seed(i)
  subsetIndex <- sample(seq(1:nrow(df)),0.8*nrow(df))
  
  trainDf <- df[subsetIndex,]
  unknownDf <- df[-subsetIndex,]
  
  logitModel <- glm(GroundTruth ~ LinMapConfWeight, data=trainDf, family="binomial")
  summary(logitModel)
  
  trainDf$logitProb <- predict(logitModel,newdata=trainDf,type="response")
  unknownDf$logitProb <- predict(logitModel,newdata=unknownDf,type="response")
  
  rf.trainDf <- randomForest(
    factor(GroundTruth)~ConfWeight,
    data = trainDf, ntree=5000, mtry=1)
  
  rf.trainDf
  
  trainDf$rfPredict <- predict(rf.trainDf, trainDf)
  unknownDf$rfPredict <- predict(rf.trainDf, unknownDf)
  
  trainDf <- mutate(trainDf, rfCorrect=ifelse(rfPredict==GroundTruth, 1, 0))
  unknownDf <- mutate(unknownDf, rfCorrect=ifelse(rfPredict==GroundTruth, 1, 0))
  
  trainSummaryDf <- rbind(trainSummaryDf, trainDf)
  unknownSummaryDf <- rbind(unknownSummaryDf, unknownDf)
}

rfSummaryDf <- group_by(unknownSummaryDf, Test, Prompt, PropCorrect) %>%
  summarise(rfCorrect=mean(rfCorrect))

view(rfSummaryDf)