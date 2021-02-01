allData <- rbind(jsonTablePilot, jsonTable2_1, jsonTable2_2, jsonTable2_3)

rm(jsonTablePilot, jsonTable2_1, jsonTable2_2, jsonTable2_3)

additional_params <- read_csv("data/additional_params.csv")

allData <- left_join(allData, additional_params, by=c("Test", "Prompt"))