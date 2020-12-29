allData <- rbind(jsonTablePilot, jsonTable2_1, jsonTable2_2)
allData <- left_join(allData, additional_params, by=c("Test", "Prompt"))

allData <- mutate(allData, Correct=ifelse(Q1==GroundTruth, 1, 0))

data <- allData
data <- mutate(data, LowTime=ifelse(Time<10, 1, 0))
data <- group_by(data, UserID) %>% summarise(PropCorrect=mean(Correct), AveTime=mean(Time), LowTimeCount=sum(LowTime), QuestionCount=n()) %>% mutate(LowTimeProp=LowTimeCount/QuestionCount)

data <- mutate(data, Spammer=ifelse(LowTimeProp>0.5, 1, 0)) %>% select(UserID, Spammer)

allData <- left_join(allData, data, by="UserID")
