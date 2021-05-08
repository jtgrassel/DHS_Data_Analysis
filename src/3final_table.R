allData <- plyr::rbind.fill(
  jsonTablePilot, 
  jsonTable2_1, 
  jsonTable2_2, 
  jsonTable2_3, 
  jsonTable3_1, 
  jsonTable3_2, 
  jsonTable3_3,
  jsonTable4_1_1,
  jsonTable4_1_2)

rm(jsonTablePilot, 
   jsonTable2_1, 
   jsonTable2_2, 
   jsonTable2_3, 
   jsonTable3_1, 
   jsonTable3_2,
   jsonTable3_3,
   jsonTable4_1_1,
   jsonTable4_1_2)

additional_params <- read_csv("data/additional_params.csv")

allData <- left_join(allData, additional_params, by=c("Test", "Prompt"))
