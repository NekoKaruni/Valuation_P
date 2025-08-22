library(tidyverse)

# CSV読み込み（列名なし）
df <- read_csv("Machine/M7003.csv",
               locale = locale(encoding = "CP932"),
               col_names = FALSE)

# 列名付与
colnames(df) <- c("項目", "英語", "金額", "年度_temp")

# 英語列削除、金額を数値化、NA行削除
df <- df %>% 
  select(項目, 金額) %>%
  mutate(金額 = as.numeric(gsub(",", "", 金額))) %>%
  filter(!is.na(金額))

# 年度リスト（5年分）
years <- 2024:2020

# 現金及び預金の行番号
split_idx <- which(df$項目 == "現金及び預金")

# 年度列初期化
df$年度 <- NA_integer_

# 5回繰り返してブロックに年度割り当て
for(i in seq_along(years)) {
  # 開始行
  start <- split_idx[i]
  # 終了行：次の現金及び預金の手前、または最後まで
  end <- if(i < length(split_idx)) split_idx[i+1]-1 else nrow(df)
  
  # 年度を割り当て
  df$年度[start:end] <- years[i]
}

# 確認
df
