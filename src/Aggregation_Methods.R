summaryBaic <- group_by(final, Prompt, Scale, Density, GroundTruth) %>% 
  summarize(ProportionCorrect=mean(Correct))


HPusers <- filter(final, Prompt %in% c(4, 8, 16, 20))
HPusers <- filter(HPusers, Correct==1)
HPusers <- HPusers$UserID

HPfinal <- filter(final, UserID %in% HPusers)

summaryHP <- group_by(HPfinal, Prompt, Scale, Density, GroundTruth) %>% 
  summarize(ProportionCorrect=mean(Correct))

#-- Attempt at a user weighted response --#
#Random sample of plots
trainPrompts1 <- sample(1:12,5,replace=F)
trainPrompts0 <- sample(13:24,5,replace=F)

#Calculate merit
meritUsers <- filter(final, Prompt %in% c(trainPrompts1, trainPrompts0)) %>% 
  group_by(UserID) %>% 
  summarize(propCorrect = mean(Correct))

#summarize merit weighted responses
summaryMerit <- left_join(final, meritUsers, by="UserID") %>% 
  filter((!Prompt %in% c(trainPrompts0, trainPrompts1))) %>%
  mutate(meritResponse=ifelse(Q1==1, 1*propCorrect, -1*propCorrect)) %>%
  group_by(Prompt, GroundTruth) %>% summarise(meritWeightedMean=mean(meritResponse))
view(summaryMerit)


#--- Threshold Voting---#
minVoteThresholds <- tibble(Threshold=numeric(), AggType=character(), Category=character(), Proportion=numeric())

minVoteData <- allData
minVoteData <- filter(allData, Spammer==0)
minVoteData <-filter(minVoteData, Test=="2_3") #Choose which test

for (threshold in seq(0, 1, 0.025)) {
  confThreshold <- threshold*200-100
  confSqThreshold <- confThreshold*100
  
  minVote <- minVoteData %>% 
    mutate(ConfAnswer = ifelse(Q1==1, Q2, -1*Q2)) %>%
    mutate(ConfSqAnswer = ifelse(ConfAnswer<0, -1*ConfAnswer^2, ConfAnswer^2)) %>%
    mutate(LinMapConfAnswer = ifelse(Q1==1, LinMapConf, -1*LinMapConf)) %>%
    group_by(Test, Prompt, GroundTruth) %>%
    summarize(
      PosProp = mean(Q1), 
      ConfPosProp = mean(ConfAnswer), 
      ConfSqPosProp = mean(ConfSqAnswer), 
      LinMapConfPosProp = mean(LinMapConfAnswer))
  
minVote <- mutate(minVote, 
      PosPropGuess = ifelse(PosProp>threshold, 1, 0), 
      ConfPosPropGuess = ifelse(ConfPosProp>confThreshold, 1, 0), 
      ConfSqPosPropGuess = ifelse(ConfSqPosProp>confSqThreshold, 1, 0), 
      LinMapConfPosPropGuess = ifelse(LinMapConfPosProp>confThreshold, 1, 0))

  minVote <- mutate(minVote, 
      PosPropCorrect=ifelse(PosPropGuess==GroundTruth, 1, 0), 
      ConfPosPropCorrect=ifelse(ConfPosPropGuess==GroundTruth, 1, 0), 
      ConfSqPosPropCorrect=ifelse(ConfSqPosPropGuess==GroundTruth, 1, 0), 
      LinMapConfPosPropCorrect=ifelse(LinMapConfPosPropGuess==GroundTruth, 1, 0))
  
  minVoteSummary <- minVote %>% group_by(GroundTruth) %>%
    summarize(
      ThresholdPropCorrect=mean(PosPropCorrect), 
      ConfThresholdPropCorrect=mean(ConfPosPropCorrect), 
      ConfSqThresholdPropCorrect=mean(ConfSqPosPropCorrect), 
      LinMapConfThresholdPropCorrect=mean(LinMapConfPosPropCorrect))
  
  minVoteThresholds <- minVoteThresholds %>% 
    add_row(Threshold=threshold, AggType="Standard", Category="TrueNeg", Proportion=minVoteSummary$ThresholdPropCorrect[[1]]) %>%
    add_row(Threshold=threshold, AggType="Standard", Category="TruePos", Proportion=minVoteSummary$ThresholdPropCorrect[[2]]) %>%
    
    add_row(Threshold=threshold, AggType="Confidence", Category="TrueNeg", Proportion=minVoteSummary$ConfThresholdPropCorrect[[1]]) %>%
    add_row(Threshold=threshold, AggType="Confidence", Category="TruePos", Proportion=minVoteSummary$ConfThresholdPropCorrect[[2]]) %>%
    
    add_row(Threshold=threshold, AggType="ConfidenceSq", Category="TrueNeg", Proportion=minVoteSummary$ConfSqThresholdPropCorrect[[1]]) %>%
    add_row(Threshold=threshold, AggType="ConfidenceSq", Category="TruePos", Proportion=minVoteSummary$ConfSqThresholdPropCorrect[[2]]) %>%
    
    add_row(Threshold=threshold, AggType="LinMapConf", Category="TrueNeg", Proportion=minVoteSummary$LinMapConfThresholdPropCorrect[[1]]) %>%
    add_row(Threshold=threshold, AggType="LinMapConf", Category="TruePos", Proportion=minVoteSummary$LinMapConfThresholdPropCorrect[[2]])
}

plotData <- minVoteThresholds
ggplot(data=plotData, mapping=aes(x=Threshold, y=Proportion, colour=AggType, linetype=Category, group=interaction(Category, AggType))) + 
          #geom_smooth() + 
          geom_line(size=1.5) + 
          geom_point(size=3) + 
          scale_x_continuous(limits=c(0, 1), breaks=seq(0, 1, 0.1)) +
          scale_y_continuous(name="Proportion Correct", limits=c(0, 1), breaks=seq(0, 1, 0.1))

plotData <- group_by(minVoteThresholds, Threshold, AggType) %>% summarize(Proportion=mean(Proportion))
ggplot(data=plotData, mapping=aes(x=Threshold, y=Proportion, colour=AggType, group=AggType)) + 
  #geom_smooth() + 
  geom_line(size=1.5) + 
  geom_point(size=3) +
  scale_x_continuous(limits=c(0, 1), breaks=seq(0, 1, 0.1)) +
  scale_y_continuous(name="Proportion Correct", limits=c(0.5, 1), breaks=seq(0.5, 1, 0.1))
       