#Add correct or not column
allData <- mutate(allData, Correct=ifelse(Q1==GroundTruth, 1, 0))


#Add  olumn indicating spammer or not
data <- allData
data <- mutate(data, LowTime=ifelse(Time<10, 1, 0))
data <- group_by(data, UserID) %>% summarise(PropCorrect=mean(Correct), AveTime=mean(Time), LowTimeCount=sum(LowTime), QuestionCount=n()) %>% mutate(LowTimeProp=LowTimeCount/QuestionCount)

data <- mutate(data, Spammer=ifelse(LowTimeProp>0.5, 1, 0)) %>% select(UserID, Spammer)

allData <- left_join(allData, data, by="UserID")
rm(data)

#Remapped Confidence
#Linear Remapping
data <- allData
data <- group_by(data, UserID) %>%
  summarize(Min=min(Q2), Max=max(Q2))
data <- left_join(allData, data, by="UserID")
#data <- mutate(data, LinMapConf=(Q2-Min)*(100/(Max-Min)))
data <- mutate(data, LinMapConf=ifelse(Min==Max, Max, (Q2-Min)*(100/(Max-Min))))
allData <- left_join(allData, select(data, Test, Prompt, UserID, LinMapConf), by=c("Test", "Prompt", "UserID"))
