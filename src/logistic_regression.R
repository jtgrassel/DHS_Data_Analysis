logitFinal <- allData
logitFinal$UserID <- factor(logitFinal$UserID)
logitFinal$GroundTruth <- factor(logitFinal$GroundTruth)
logitFinal$Image <- factor(logitFinal$Image)
logitFinal$Color <- factor(logitFinal$Color)


logitFinal <- filter(logitFinal, Test=="2_2")


logitMode <- glm(Correct ~ Image, data = logitFinal, family="binomial")
summary(logitMode)
confint(logitMode)
exp(cbind(OR=coef(logitMode), confint(logitMode)))
