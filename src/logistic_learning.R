#Make the dataframe used
df <- allData
df <- group_by(df, Test, Prompt, GroundTruth, Scale, Density, Image, Color) %>%
  summarize(SPmean=mean(Q3), PropCorrect=mean(Correct), Standard=mean(Q1), ConfWeighted=mean(Confidence),ConfSqWeighted=mean(ConfidenceSq), LinMapConfWeighted=mean(LinMapConf), AverageTime=mean(Time))

#Make the training and testing df
trainSize <- round(nrow(df)*0.5)
trainSubset <- sample(seq(1:nrow(df)), trainSize)

trainDf <-df[trainSubset,]
unknownDf <- df[-trainSubset,]

#Make a linear model for "difficulty"
linModel <- lm(PropCorrect ~ AverageTime+Scale+Density, data=trainDf)
summary(linModel)

trainDf$linDiff <- predict(linModel,newdata=trainDf,type="response")
unknownDf$linDiff <- predict(linModel,newdata=unknownDf,type="response")

#train logistic regression on the training set, then apply it to the unknown data
logitModel <- glm(GroundTruth ~ ConfSqWeighted, data=trainDf, family="binomial")
summary(logitModel)

trainDf$LogitProb <- predict(logitModel,newdata=trainDf,type="response")
unknownDf$LogitProb <- predict(logitModel,newdata=unknownDf,type="response")

ggplot(unknownDf, aes(x=factor(GroundTruth), y=LogitProb)) + 
  geom_dotplot(binaxis="y",stackdir="center",binwidth=0.1,aes(fill=factor(Test)))
