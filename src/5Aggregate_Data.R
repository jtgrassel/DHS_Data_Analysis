#agregate table

allAggData <- allData %>% filter(Spammer == 0) %>%
  group_by(Test, Prompt, GroundTruth, Scale, Density, Image, Color) %>%
  dplyr::summarize(
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
    LinMapConfVar=var(LinMapConf),
    dsPropMain=sum(ClusterCategory=="Main", na.rm = TRUE)/n(),
    dsPropSecondary=sum(ClusterCategory=="Secondary", na.rm = TRUE)/n(),
    dsPropNoise=sum(ClusterCategory=="Noise", na.rm = TRUE)/n(),
    dsPropNegative=sum(is.na(ClusterCategory))/n()
  )


#linear regression difficulty estimation
diffModel <- lm(PropCorrect~Scale+Density, data=allAggData)
summary(diffModel)
allAggData$linDiff <- predict(diffModel,newdata=allAggData,type="response")
