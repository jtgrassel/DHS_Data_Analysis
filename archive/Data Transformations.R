###-- DEPRECATED ---#

raw <- read_csv("11_11_Data.csv")

img_set <- tribble(
  ~Prompt, ~imgNum,
  0, "img2",
  1, "img26",
  2, "img27",
  3, "img29",
  4, "img34",
  5, "img40",
  6, "img50",
  7, "img2",
  8, "img26",
  9, "img27",
  10, "img29",
  11, "img34",
  12, "img40",
  13, "img50",
)

table <- left_join(raw, img_set, by="Prompt")

params <- tribble(
  ~imgNum, ~scale_midpoint, ~density, ~rotation, ~color, ~alpha_variation,
  "img2", 0.2, 125, "rand", "rand", "low",
  "img26", 0.3, 125, "rand", "rand", "low",
  "img27", 0.3, 150, "rand", "rand", "low",
  "img29", 0.3, 125, "rand", "rand", "high",
  "img34", 0.3, 100, "rand", "mono", "low",
  "img40", 0.3, 100, "vertical", "rand", "high",
  "img50", 0.4, 125, "rand", "rand", "low"
)

table <- left_join(table, params, by="imgNum")

table <- mutate(table, "Correct" = ifelse(Q1==Ground_Truth, 1, 0))