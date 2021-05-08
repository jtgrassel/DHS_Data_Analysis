df <- filter(allAggData, GroundTruth == 1)

linearModel <- lm(PropCorrect~Scale+Density, df)
summary(linearModel)
