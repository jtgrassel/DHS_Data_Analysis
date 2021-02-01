#Add correct or not column
allData <- mutate(allData, Correct=ifelse(Q1==GroundTruth, 1, 0))


#Add  column indicating spammer or not
data <- allData
data <- mutate(data, LowTime=ifelse(Time<8, 1, 0))
data <- group_by(data, UserID) %>% summarise(PropCorrect=mean(Correct), AveTime=mean(Time), LowTimeCount=sum(LowTime), QuestionCount=n()) %>% mutate(LowTimeProp=LowTimeCount/QuestionCount)

data <- mutate(data, Spammer=ifelse(LowTimeProp>0.5, 1, 0)) %>% select(UserID, Spammer)

allData <- left_join(allData, data, by="UserID")
rm(data)

#Confidence
allData <- mutate(allData, Confidence = ifelse(Q1==1, Q2, -1*Q2))

#Confidence squared
allData <- mutate(allData, ConfidenceSq = ifelse(Q1==1, (Q2^2)/100, (-1*(Q2^2))/100))

#Remapped Confidence
#Linear Remapping
data <- allData
data <- group_by(data, UserID) %>%
  summarize(Min=min(Q2), Max=max(Q2))
data <- left_join(allData, data, by="UserID")
data <- mutate(data, LinMapConf=ifelse(Min==Max, Max, (Q2-Min)*(100/(Max-Min))))
data <- mutate(data, LinMapConf=ifelse(Q1==1, LinMapConf, -1*LinMapConf))
allData <- left_join(allData, select(data, Test, Prompt, UserID, LinMapConf), by=c("Test", "Prompt", "UserID"))
rm(data)

#Get distance sq to the mean of answers
data <- allData
data <- group_by(data, Test, Prompt) %>%
  summarize(
    StandardMean=mean(Q1),
    ConfMean=mean(Confidence),
    ConfSqMean=mean(ConfidenceSq),
    LinMapConfMean=mean(LinMapConf)
  )
data <- left_join(allData, data, by=c("Test", "Prompt"))


data <- mutate(data, 
  ConfDistSq=(Confidence-ConfMean)^2,
  ConfSqDistSq=(ConfidenceSq-ConfSqMean)^2,
  LinMapConfDistSq=(LinMapConf-LinMapConfMean)^2
    )

data <- select(data, Test:Prompt, ConfDistSq:LinMapConfDistSq)
allData <- left_join(allData, data, by=c("Test", "UserID", "Prompt"))
rm(data)
