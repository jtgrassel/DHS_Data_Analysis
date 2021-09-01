library(tidyverse)

demo <- filter(allData,Test != 'Pilot', Spammer == 0) %>% group_by(UserID, Gender, Age, Edu) %>% summarise()

demo %>% group_by(Gender) %>% summarise(n = n())

demo %>% group_by(Edu) %>% summarise(n=n())

summary(demo$Age)

view(demo)
