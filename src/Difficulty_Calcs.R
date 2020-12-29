cw_model <- mutate(table, "q1i" = ifelse(Q1==1, 1, -1)) %>%
  mutate(CW = Q2*q1i)

cw_model <- group_by(cw_model, Prompt) %>% summarize(CW = sum(CW), n=n(), G = max(Ground_Truth), imgNum = first(imgNum))
cw_model <- mutate(cw_model, G = ifelse(G==1, 100, -100))

cw_model <- mutate(cw_model, CW_diff = abs(CW/n - G))
