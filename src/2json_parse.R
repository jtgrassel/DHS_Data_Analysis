library(tidyverse)
# Parse the json files into tables, then remove them.
#The tables have the following columns:
#Test, UserID, Prompt, Q1, Q2, Q3, Time, x, y

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
rm(jsonRawPilot)

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
rm(jsonRaw2_1)

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
rm(jsonRaw2_2)

#---- Parse the 2_3 data ----#
jsonTable2_3 <- tibble(Test=character(), UserID=character(), Prompt=numeric(),  Q1=numeric(), Q2=numeric(), Q3=numeric(), Time=numeric())
for (i in seq(1, length(jsonRaw2_3))) {
  for (j in seq(1, length(jsonRaw2_3[[i]])-2)) {
    jsonTable2_3 <- add_row(jsonTable2_3, 
                            Test = "2_3",
                            Prompt = j,
                            UserID=jsonRaw2_3[[i]]["name"][[1]], 
                            Q1 = jsonRaw2_3[[i]][[j]]$q1, 
                            Q2 = jsonRaw2_3[[i]][[j]]$q2, 
                            Q3 = jsonRaw2_3[[i]][[j]]$q3, 
                            Time = jsonRaw2_3[[i]][[j]]$time
    )
  }
} 
rm(i, j)
rm(jsonRaw2_3)

#---- Parse the 3_1 data ----#
jsonTable3_1 <- tibble(Test=character(), UserID=character(), Prompt=numeric(),  Q1=numeric(), Q2=numeric(), Q3=numeric(), Time=numeric(), x=numeric(), y=numeric())
for (i in seq(1, length(jsonRaw3_1))) {
  for (j in seq(1, length(jsonRaw3_1[[i]])-2)) {
    jsonTable3_1 <- add_row(jsonTable3_1, 
                            Test = "3_1",
                            Prompt = j,
                            UserID=jsonRaw3_1[[i]]["name"][[1]], 
                            Q1 = jsonRaw3_1[[i]][[j]]$q1, 
                            Q2 = jsonRaw3_1[[i]][[j]]$q2, 
                            Q3 = jsonRaw3_1[[i]][[j]]$q3, 
                            Time = jsonRaw3_1[[i]][[j]]$time,
                            x = ifelse(is.null(jsonRaw3_1[[i]][[j]]$x), NA, jsonRaw3_1[[i]][[j]]$x),
                            y = ifelse(is.null(jsonRaw3_1[[i]][[j]]$y), NA, jsonRaw3_1[[i]][[j]]$y)
    )
  }
} 
rm(i, j)
rm(jsonRaw3_1)

#---- Parse the 3_2 data ----#
jsonTable3_2 <- tibble(Test=character(), UserID=character(), Prompt=numeric(),  Q1=numeric(), Q2=numeric(), Q3=numeric(), Time=numeric(), x=numeric(), y=numeric())
for (i in seq(1, length(jsonRaw3_2))) {
  for (j in seq(1, length(jsonRaw3_2[[i]])-2)) {
    jsonTable3_2 <- add_row(jsonTable3_2, 
                            Test = "3_2",
                            Prompt = j,
                            UserID=jsonRaw3_2[[i]]["name"][[1]], 
                            Q1 = jsonRaw3_2[[i]][[j]]$q1, 
                            Q2 = jsonRaw3_2[[i]][[j]]$q2, 
                            Q3 = jsonRaw3_2[[i]][[j]]$q3, 
                            Time = jsonRaw3_2[[i]][[j]]$time,
                            x = ifelse(is.null(jsonRaw3_2[[i]][[j]]$x), NA, jsonRaw3_2[[i]][[j]]$x),
                            y = ifelse(is.null(jsonRaw3_2[[i]][[j]]$y), NA, jsonRaw3_2[[i]][[j]]$y)
    )
  }
} 
rm(i, j)
rm(jsonRaw3_2)

#---- Parse the 3_3 data ----#
jsonTable3_3 <- tibble(Test=character(), UserID=character(), Prompt=numeric(),  Q1=numeric(), Q2=numeric(), Q3=numeric(), Time=numeric(), x=numeric(), y=numeric())
for (i in seq(1, length(jsonRaw3_3))) {
  for (j in seq(1, length(jsonRaw3_3[[i]])-2)) {
    jsonTable3_3 <- add_row(jsonTable3_3, 
                            Test = "3_3",
                            Prompt = j,
                            UserID=jsonRaw3_3[[i]]["name"][[1]], 
                            Q1 = jsonRaw3_3[[i]][[j]]$q1, 
                            Q2 = jsonRaw3_3[[i]][[j]]$q2, 
                            Q3 = jsonRaw3_3[[i]][[j]]$q3, 
                            Time = jsonRaw3_3[[i]][[j]]$time,
                            x = ifelse(is.null(jsonRaw3_3[[i]][[j]]$x), NA, jsonRaw3_3[[i]][[j]]$x),
                            y = ifelse(is.null(jsonRaw3_3[[i]][[j]]$y), NA, jsonRaw3_3[[i]][[j]]$y)
    )
  }
} 
rm(i, j)
rm(jsonRaw3_3)

#---- Parse the 4_1_1 data ----#
jsonTable4_1_1 <- tibble(Test=character(), UserID=character(), Prompt=numeric(),  Q1=numeric(), Q2=numeric(), Q3=numeric(), Time=numeric(), x=numeric(), y=numeric())
for (i in seq(1, length(jsonRaw4_1_1))) {
  for (j in seq(1, length(jsonRaw4_1_1[[i]])-2)) {
    sj <- toString(j)
    
    jsonTable4_1_1 <- add_row(jsonTable4_1_1, 
                            Test = "4_1_1",
                            Prompt = j,
                            UserID=jsonRaw4_1_1[[i]]["name"][[1]], 
                            Q1 = jsonRaw4_1_1[[i]][[sj]]$q1, 
                            Q2 = jsonRaw4_1_1[[i]][[sj]]$q2, 
                            Q3 = jsonRaw4_1_1[[i]][[sj]]$q3, 
                            Time = jsonRaw4_1_1[[i]][[sj]]$time,
                            x = ifelse(is.null(jsonRaw4_1_1[[i]][[sj]]$x), NA, jsonRaw4_1_1[[i]][[sj]]$x),
                            y = ifelse(is.null(jsonRaw4_1_1[[i]][[sj]]$y), NA, jsonRaw4_1_1[[i]][[sj]]$y)
    )
  }
} 
rm(i, j, sj)
rm(jsonRaw4_1_1)

#---- Parse the 4_1_2 data ----#
jsonTable4_1_2 <- tibble(Test=character(), UserID=character(), Prompt=numeric(),  Q1=numeric(), Q2=numeric(), Q3=numeric(), Time=numeric(), x=numeric(), y=numeric())
for (i in seq(1, length(jsonRaw4_1_2))) {
  for (j in seq(1, length(jsonRaw4_1_2[[i]])-2)) {
    sj <- toString(j)
    
    jsonTable4_1_2 <- add_row(jsonTable4_1_2, 
                              Test = "4_1_2",
                              Prompt = j,
                              UserID=jsonRaw4_1_2[[i]]["name"][[1]], 
                              Q1 = jsonRaw4_1_2[[i]][[sj]]$q1, 
                              Q2 = jsonRaw4_1_2[[i]][[sj]]$q2, 
                              Q3 = jsonRaw4_1_2[[i]][[sj]]$q3, 
                              Time = jsonRaw4_1_2[[i]][[sj]]$time,
                              x = ifelse(is.null(jsonRaw4_1_2[[i]][[sj]]$x), NA, jsonRaw4_1_2[[i]][[sj]]$x),
                              y = ifelse(is.null(jsonRaw4_1_2[[i]][[sj]]$y), NA, jsonRaw4_1_2[[i]][[sj]]$y)
    )
  }
} 
rm(i, j, sj)
rm(jsonRaw4_1_2)

#---- Parse the 4_1_3 data ----#
jsonTable4_1_3 <- tibble(Test=character(), UserID=character(), Prompt=numeric(),  Q1=numeric(), Q2=numeric(), Q3=numeric(), Time=numeric(), x=numeric(), y=numeric())
for (i in seq(1, length(jsonRaw4_1_3))) {
  for (j in seq(1, length(jsonRaw4_1_3[[i]])-2)) {
    sj <- toString(j)
    
    jsonTable4_1_3 <- add_row(jsonTable4_1_3, 
                              Test = "4_1_3",
                              Prompt = j,
                              UserID=jsonRaw4_1_3[[i]]["name"][[1]], 
                              Q1 = jsonRaw4_1_3[[i]][[sj]]$q1, 
                              Q2 = jsonRaw4_1_3[[i]][[sj]]$q2, 
                              Q3 = jsonRaw4_1_3[[i]][[sj]]$q3, 
                              Time = jsonRaw4_1_3[[i]][[sj]]$time,
                              x = ifelse(is.null(jsonRaw4_1_3[[i]][[sj]]$x), NA, jsonRaw4_1_3[[i]][[sj]]$x),
                              y = ifelse(is.null(jsonRaw4_1_3[[i]][[sj]]$y), NA, jsonRaw4_1_3[[i]][[sj]]$y)
    )
  }
} 
rm(i, j, sj)
rm(jsonRaw4_1_3)
