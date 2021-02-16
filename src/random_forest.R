library(tidyverse)
library(randomForest)

#Single random forest
treeData <- filter(allAggData, Test %in% c("3_1","3_2","3_3"))

rf.aggData = randomForest(factor(GroundTruth)~LinMapConfWeight+dsPropMain+dsPropSecondary+dsPropNoise+dsPropNegative, 
                          data = treeData, ntree=2000, mtry=3)
rf.aggData

treeData$predicted <- predict(rf.aggData, treeData)
treeData$predictCorrect <- ifelse(treeData$predicted==treeData$GroundTruth, 1, 0)
varImpPlot(rf.aggData)

#Single logistic regression
df <-allAggData

logitModel <- glm(
  factor(GroundTruth)~LinMapConfWeight, 
  data = df, family = "binomial")

df$logitPredict <- predict(logitModel,newdata=df,type="response")

ggplot(data=df,aes(x=factor(GroundTruth),y=logitPredict)) +
  geom_dotplot(binaxis="y",stackdir="center",binwidth=0.03)

ggplot(data=df,aes(x=LinMapConfWeight,y=logitPredict,color=factor(GroundTruth))) +
  scale_color_manual(values = c("dark blue","orange"),aesthetics = "color") +
  geom_point(alpha=0.5,size=3) + 
  labs(title="GT~LinMapConfWeight")



#trying multiple times
unknownSummaryDf <- data.frame()

for (i in seq(1:100)) {
  df <- filter(allAggData, Test %in% c("3_1","3_2","3_3"))
  
  set.seed(i)
  subsetIndex <- sample(seq(1:nrow(df)),0.6*nrow(df))
  
  trainDf <- df[subsetIndex,]
  unknownDf <- df[-subsetIndex,]
  
  #random forest model
  rf.trainDf <- randomForest(
    factor(GroundTruth)~LinMapConfWeight+LinMapConfVar+dsPropMain+dsPropSecondary+dsPropNoise+dsPropNegative,
    data = trainDf, ntree=1000, mtry=3)

  unknownDf$rfPredict <- predict(rf.trainDf, unknownDf)
  unknownDf <- mutate(unknownDf, rfCorrect=ifelse(rfPredict==GroundTruth, 1, 0))
  
  #logistic regression model
  logitModel <- glm(
    factor(GroundTruth)~LinMapConfWeight, 
    data = trainDf, family = "binomial")
  
  unknownDf$logitPredict <- predict(logitModel,newdata=unknownDf,type="response")
  unknownDf <- mutate(unknownDf, logitCorrect=ifelse(ifelse(logitPredict>0.5,1,0)==GroundTruth,1,0))
  
  unknownSummaryDf <- rbind(unknownSummaryDf, unknownDf)
}

rfSummaryDf <- group_by(unknownSummaryDf, Test, Prompt, PropCorrect) %>%
  summarise(rfCorrect=mean(rfCorrect),logitCorrect=mean(logitCorrect))

view(rfSummaryDf)
