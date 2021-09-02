#Add correct or not column
allData <- mutate(allData, Correct=ifelse(Q1==GroundTruth, 1, 0))


#Add  column indicating spammer or not
data <- allData
data <- mutate(data, LowTime=ifelse(Time<=10, 1, 0))
data <- group_by(data, UserID) %>% 
  dplyr::summarise(PropCorrect=mean(Correct), AveTime=mean(Time), LowTimeCount=sum(LowTime), QuestionCount=n(), Q1SUM = sum(Q1)) %>% 
  mutate(LowTimeProp=LowTimeCount/QuestionCount, MonoAnswer = Q1SUM/QuestionCount)

data <- mutate(data, Spammer=ifelse(LowTimeProp>=0.75 | MonoAnswer == 1 | MonoAnswer == 0, 1, 0)) %>% select(UserID, Spammer)

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
  dplyr::summarize(Min=min(Q2), Max=max(Q2))
data <- left_join(allData, data, by="UserID")
data <- mutate(data, LinMapConf=ifelse(Min==Max, Max, (Q2-Min)*(100/(Max-Min))))
data <- mutate(data, LinMapConf=ifelse(Q1==1, LinMapConf, -1*LinMapConf))
allData <- left_join(allData, select(data, Test, Prompt, UserID, LinMapConf), by=c("Test", "Prompt", "UserID"))
rm(data)

#DSCAN clustering
library(dbscan)
dsdata <- filter(allData, Spammer==0) %>% select(Test, Prompt, UserID, x, y)

dsdata <- na.omit(dsdata)
outputDf <- tibble(Test=character(),Prompt=numeric(),UserID=character(),cluster=numeric())

test_list <- dsdata %>% group_by(Test) %>% summarise()
test_list <- as.vector(test_list$Test)

for (test in test_list) {
  prompt_list <- dsdata %>% filter(Test==test) %>% group_by(Prompt) %>% summarise()
  prompt_list <- as.vector(prompt_list$Prompt)
  
  for (prompt in prompt_list) {
    
    temp <- filter(dsdata, Test==test, Prompt==prompt)
    temp <- na.omit(temp)
    
    if (nrow(temp) > 3) {
      ds.temp <- dbscan(temp[c("x","y")], eps=35, minPts=3)
      temp$cluster <- ds.temp$cluster
      
      vector <- as.vector(table(ds.temp$cluster))
      
      if (length(vector)==1 & ds.temp$cluster[[1]]==1) {
        temp$ClusterCategory <- "Main"
      }
      else if (length(vector)==1 & ds.temp$cluster[[1]]==0) {
        temp$ClusterCategory <- "Noise"
      }
      else {
        vector <- vector[2:length(vector)]
        maxCluster <- which.max(vector)
        
        temp$ClusterCategory[temp$cluster==0] <- "Noise"
        temp$ClusterCategory[temp$cluster==maxCluster] <- "Main"
        temp$ClusterCategory[temp$cluster != 0 & temp$cluster != maxCluster] <- "Secondary"
      }
      outputDf <- rbind(outputDf, temp[c("Test", "Prompt","UserID","cluster","ClusterCategory")])
    }
  }
}



allData <- left_join(allData, outputDf, by=c("Test","Prompt","UserID"))
rm(list = c("dsdata", "outputDf", "test_list", "prompt_list", "temp", "ds.temp", "maxCluster","prompt","test","vector"))
