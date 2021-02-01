#agregate table

allAggData <- group_by(allData, Test, Prompt, GroundTruth, Scale, Density, Image, Color) %>%
  summarize(
    Responses=n(),
    AverageTime=mean(Time),
    PropCorrect=mean(Correct),
    SPmean=mean(Q3),
    Standard=mean(Q1),
    ConfWeight=mean(Confidence),
    ConfVar=var(Confidence),
    ConfSqWeight=mean(ConfidenceSq),
    ConfSqVar=var(ConfidenceSq),
    LinMapConfWeight=mean(LinMapConf),
    LinMapConfVar=var(LinMapConf)
  )


#linear regression difficulty estimation
diffModel <- lm(PropCorrect~AverageTime+Scale+Density, data=allAggData)
summary(diffModel)
allAggData$linDiff <- predict(diffModel,newdata=allAggData,type="response")
