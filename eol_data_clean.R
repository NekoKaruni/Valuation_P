library(tidyverse)
df <- read_csv("Prime_Bank/5838.csv",col_names = c("項目","削除","金額","年度"), locale = locale(encoding = "Shift-JIS"))

df_cl <- df %>% 
  select(-削除)

df_addYear <- df_cl %>% 
  mutate(yeargroup = cumsum(項目 == "現金預け金")) %>% 
  mutate(
    年度= 2024 -(yeargroup - 1)
  ) %>% 
  select(-yeargroup)
