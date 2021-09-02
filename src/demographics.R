data <- filter(allData, Test != 'Pilot')
view(group_by(data, UserID) %>% summarise())
view(filter(data, Spammer == 1) %>% group_by(UserID) %>% summarise())

view(data)
