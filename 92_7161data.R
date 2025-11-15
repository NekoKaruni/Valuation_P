library(tidyverse)
library(dplyr)
library(stringr)

# 1. データ読み込み
df <- read_csv("ProcessedBankData/7161_processed_data.csv")

# 2. 年度列作成 & 金額NA除外 & 負債集計
result <- df %>%
  mutate(年度 = str_sub(ファイル名, 5, 8)) %>%
  filter(!is.na(`金額（百万円）`), `金額（百万円）` != 0) %>%
  group_by(年度) %>%
  summarise(
    D  = sum(`金額（百万円）`[項目 %in% c("預金", "借用金", "社債")], na.rm = TRUE),
    DU = sum(`金額（百万円）`[項目 %in% c("預金利息", "借用金利息", "社債利息")], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(D_cost = DU / D)

# 3. 株数・株価・NP・rE追加
Price <- c(2290, 1990, 1980, 1950, 1250, 1130, 821, 642, 440, 545)
N <- 72840263
rF <- 0.01570
beta <- 0.3849
Er <- 0.0486
MRP <- Er - rF
rE <- rF + beta * MRP
T <- 0.2320
D_costAve <- mean(result$D_cost, na.rm = TRUE)

result <- result %>%
  mutate(
    N = N,
    P = Price,
    NP = round((N * P) / 1e6,2),  # 百万円単位
    rE = round(rE, 4)
  ) %>%
  mutate(
    WACC = round(D / (D + NP) * D_costAve * (1 - T) + NP / (D + NP) * rE, 4)
  )

# 4. CFOとCFI抽出 & FCF計算
CFO <- df %>% filter(項目 == "営業活動によるキャッシュ・フロー") %>% rename(年度 = ファイル名)
CFI <- df %>% filter(項目 == "投資活動によるキャッシュ・フロー") %>% rename(年度 = ファイル名)

FCF_combined_df <- inner_join(
  CFO %>% select(年度, CFO_金額 = `金額（百万円）`),
  CFI %>% select(年度, CFI_金額 = `金額（百万円）`),
  by = "年度"
) %>%
  mutate(FCF = CFO_金額 - CFI_金額,
         FCF = replace_na(FCF, 0)) %>%
  arrange(年度)

result <- result %>% mutate(FCF = FCF_combined_df$FCF)

# 5. 過去10年FCF → 指数加重 → 将来FCF予測
past_FCF <- tail(result$FCF, 10)
n_years <- length(past_FCF)
alpha <- 0.2
weights <- exp(alpha * (1:n_years)); weights <- weights / sum(weights)
weighted_avg_FCF <- sum(past_FCF * weights)
forecast_years <- 10
g <- 0.01
FCF_future <- weighted_avg_FCF * (1 + g)^(1:forecast_years)
discount_factors <- (1 + rE)^(1:forecast_years)
PV_FCFE <- FCF_future / discount_factors

# 6. result に追加
result <- result %>%
  mutate(
    PV_FCF =round(PV_FCFE, 2)
  )

# 7. 株主価値計算
Equity_value_weighted <- sum(PV_FCFE)
cat("加重DCFによる株主価値（Equity Value）:", round(Equity_value_weighted, 2), "百万円\n")



if(!dir.exists("data")) dir.create("data")
write_csv(result, "data/result7161.csv")

