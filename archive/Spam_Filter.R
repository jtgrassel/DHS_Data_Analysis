##-- DEPRECATED --#

data <- allData
data <- mutate(data, LowTime=ifelse(Time<10, 1, 0))
data <- group_by(data, UserID) %>% summarise(PropCorrect=mean(Correct), AveTime=mean(Time), LowTimeCount=sum(LowTime), QuestionCount=n()) %>% mutate(LowTimeProp=LowTimeCount/QuestionCount)

data <- mutate(data, Spammer=ifelse(LowTimeProp>0.5, 1, 0)) %>% filter(Spammer==1)

Spammers <- as.vector(data['UserID'])
