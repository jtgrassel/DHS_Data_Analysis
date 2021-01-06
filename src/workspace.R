ggplot(data=allData) + 
  geom_histogram(aes(x=Q2,y=stat(density)),bins=10) +
  facet_wrap( ~ UserID)

ggplot(data=allData) +
  geom_histogram(aes(x=Q2,y=stat(density)),bins=10)

ggplot(data=allData) +
  geom_histogram(aes(x=LinMapConf,y=stat(density)),bins=10)

mutate(allData, Majority=Q1, Conf=ifelse(Q1==1, Q2, -1*Q2), LinMapConf=ifelse(Q1==1, LinMapConf, -1*LinMapConf)) %>%
  group_by(Test, Prompt, GroundTruth) %>%
  summarize(Majority=mean(Majority), Conf=mean(Conf), LinMapConf=mean(LinMapConf)) %>%
  arrange(desc(GroundTruth), Majority) %>%
  view()
