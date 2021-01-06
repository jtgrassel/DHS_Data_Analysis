allData <- rbind(jsonTablePilot, jsonTable2_1, jsonTable2_2)
allData <- left_join(allData, additional_params, by=c("Test", "Prompt"))


