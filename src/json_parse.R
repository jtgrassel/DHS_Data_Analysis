library(tidyverse)

#---- Parse the Pilot data ----#
jsonTablePilot <- tibble(Test=character(), UserID=character(), Prompt=numeric(),  Q1=numeric(), Q2=numeric(), Q3=numeric(), Time=numeric())
for (i in seq(1, length(jsonRawPilot))) {
  for (j in seq(0, 13)) {
    jsonTablePilot <- add_row(jsonTablePilot,
                            Test = "Pilot",
                            Prompt = j,
                            UserID=jsonRawPilot[[i]]["name"][[1]], 
                            Q1 = jsonRawPilot[[i]][as.character(j)][[1]]$q1, 
                            Q2 = jsonRawPilot[[i]][as.character(j)][[1]]$q2, 
                            Q3 = jsonRawPilot[[i]][as.character(j)][[1]]$q3, 
                            Time = jsonRawPilot[[i]][as.character(j)][[1]]$time
    )
  }
} 
rm(i, j)

#---- Parse the 2_1 data ----#
jsonTable2_1 <- tibble(Test=character(), UserID=character(), Prompt=numeric(),  Q1=numeric(), Q2=numeric(), Q3=numeric(), Time=numeric())
for (i in seq(1, length(jsonRaw2_1))) {
  for (j in seq(1, length(jsonRaw2_1[[i]])-2)) {
    jsonTable2_1 <- add_row(jsonTable2_1, 
                          Test = "2_1",
                        Prompt = j,
                         UserID=jsonRaw2_1[[i]]["name"][[1]], 
                         Q1 = jsonRaw2_1[[i]][[j]]$q1, 
                         Q2 = jsonRaw2_1[[i]][[j]]$q2, 
                         Q3 = jsonRaw2_1[[i]][[j]]$q3, 
                         Time = jsonRaw2_1[[i]][[j]]$time
                           )
  }
} 
rm(i, j)

#---- Parse the 2_2 data ----#
jsonTable2_2 <- tibble(Test=character(), UserID=character(), Prompt=numeric(),  Q1=numeric(), Q2=numeric(), Q3=numeric(), Time=numeric())
for (i in seq(1, length(jsonRaw2_2))) {
  for (j in seq(1, length(jsonRaw2_2[[i]])-2)) {
    jsonTable2_2 <- add_row(jsonTable2_2, 
                            Test = "2_2",
                            Prompt = j,
                            UserID=jsonRaw2_2[[i]]["name"][[1]], 
                            Q1 = jsonRaw2_2[[i]][[j]]$q1, 
                            Q2 = jsonRaw2_2[[i]][[j]]$q2, 
                            Q3 = jsonRaw2_2[[i]][[j]]$q3, 
                            Time = jsonRaw2_2[[i]][[j]]$time
    )
  }
} 
rm(i, j)