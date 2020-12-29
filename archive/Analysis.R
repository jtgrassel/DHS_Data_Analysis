allDataSummary <- filter(allData, Test=="2_2") %>%
  group_by(Test, Prompt, GroundTruth, Scale, Density, Image, Color) %>%
  summarize(PropCorrect = mean(Correct))

fit <- lm(PropCorrect ~ Image, data=allDataSummary)
summary(fit)

