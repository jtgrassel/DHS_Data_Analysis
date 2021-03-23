#library(tidyverse)
#library(InformationValue)

df <- allAggData
df <- filter(df, Test %in% c("3_1","3_2","3_3"))
outDf <- allAggData
outDf$logitPredict <- NA
outDf$pred <- NA

for (i in 1:nrow(df)) {
  trainDf <- df[-i,]
  testDf <- df[i,]
  
  logitModel <- glm(
    factor(GroundTruth)~LinMapConfWeight, 
    data = trainDf, family = "binomial")
  
  trainDf$logitPredict <- predict(logitModel,newdata=trainDf,type="response")
  optimal <- optimalCutoff(trainDf$GroundTruth,trainDf$logitPredict)
  
  outDf$logitPredict[i] <- predict(logitModel,newdata=outDf[i,],type="response")
  outDf$pred[i] <- ifelse(outDf$logitPredict[i]>=optimal, 1, 0)
}
outDf$pred <- factor(outDf$pred,levels = c(1,0))
outDf$GroundTruth <- factor(outDf$GroundTruth,levels = c(1,0))

round(prop.table(table(outDf$pred,outDf$GroundTruth),margin=2),2)
