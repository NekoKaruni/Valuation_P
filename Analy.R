library(tidyverse)

df7150 <- read_csv("ProcessedBankData/7150_processed_data.csv")

result <- df7150 |> 
  mutate(年度 = str_sub(ファイル名, 5, 8)) %>%
  filter(!is.na(`金額（百万円）`), `金額（百万円）` != 0) %>%
  group_by(年度) %>%
  summarise(
    D  = sum(`金額（百万円）`[項目 %in% c("預金", "借用金", "社債")], na.rm = TRUE),
    DU = sum(`金額（百万円）`[項目 %in% c("預金利息", "借用金利息", "社債利息")], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(rD = DU / D)

Net_income <- df7150 |> 
  filter(項目 %in% c("当期純利益", "当期純利益又は当期純損失（△）")) |> 
  slice(seq(1, n(), by = 2)) |>   # 奇数行抽出
  pull(`金額（百万円）`)

Net_asset <- df7150 |> 
  filter(項目 == "純資産の部合計") |> 
  pull(`金額（百万円）`)

CFO_V <- df7150 |> 
  filter(項目=="営業活動によるキャッシュ・フロー") |> 
  pull(`金額（百万円）`)

CFI_V <- df7150 |> 
  filter(項目=="投資活動によるキャッシュ・フロー") |> 
  pull(`金額（百万円）`)

result <- result |> 
  mutate(
    Net_income = Net_income,
    Net_asset  = Net_asset,
    CFO = CFO_V,
    CFI = CFI_V,
    FCF = CFO_V + CFI_V
  )

write_csv(result,"bf/bf7150.csv")

ーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーー

df7161 <- read_csv("ProcessedBankData/7161_processed_data.csv")

result <- df7161 |> 
  mutate(年度 = str_sub(ファイル名, 5, 8)) %>%
  filter(!is.na(`金額（百万円）`), `金額（百万円）` != 0) %>%
  group_by(年度) %>%
  summarise(
    D  = sum(`金額（百万円）`[項目 %in% c("預金", "借用金", "社債")], na.rm = TRUE),
    DU = sum(`金額（百万円）`[項目 %in% c("預金利息", "借用金利息", "社債利息")], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(rD = DU / D)

Net_income <- df7161 |> 
  filter(項目 %in% c("当期純利益", 	"当期純利益又は当期純損失（△）")) |> 
  slice(seq(1, n(), by = 2)) |>   # 奇数行抽出
  pull(`金額（百万円）`)

Net_income <- c(Net_income,-23462)

Net_asset <- df7161 |> 
  filter(項目 == "純資産の部合計") |> 
  pull(`金額（百万円）`)

CFO_V <- df7161 |> 
  filter(項目=="営業活動によるキャッシュ・フロー") |> 
  pull(`金額（百万円）`)

CFI_V <- df7161 |> 
  filter(項目=="投資活動によるキャッシュ・フロー") |> 
  pull(`金額（百万円）`)

result <- result |> 
  mutate(
    Net_income = Net_income,
    Net_asset  = Net_asset,
    CFO = CFO_V,
    CFI = CFI_V,
    FCF = CFO_V + CFI_V
  )

write_csv(result,"bf/bf7161.csv") 



ーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーー

df8349 <- read_csv("ProcessedBankData/8349_processed_data.csv")

result <- df8349 |> 
  mutate(年度 = str_sub(ファイル名, 5, 8)) %>%
  filter(!is.na(`金額（百万円）`), `金額（百万円）` != 0) %>%
  group_by(年度) %>%
  summarise(
    D  = sum(`金額（百万円）`[項目 %in% c("預金", "借用金", "社債")], na.rm = TRUE),
    DU = sum(`金額（百万円）`[項目 %in% c("預金利息", "借用金利息", "社債利息")], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(rD = DU / D)

Net_income <- df8349 |> 
  filter(項目 %in% c("当期純利益", "当期純利益又は当期純損失（△）")) |> 
  slice(seq(1, n(), by = 2)) |>  
  pull(`金額（百万円）`)

Net_asset <- df8349 |> 
  filter(項目 == "純資産の部合計") |> 
  pull(`金額（百万円）`)

CFO_V <- df8349 |> 
  filter(項目=="営業活動によるキャッシュ・フロー") |> 
  pull(`金額（百万円）`)

CFI_V <- df8349 |> 
  filter(項目=="投資活動によるキャッシュ・フロー") |> 
  pull(`金額（百万円）`)

result <- result |> 
  mutate(
    Net_income = Net_income,
    Net_asset  = Net_asset,
    CFO = CFO_V,
    CFI = CFI_V,
    FCF = CFO_V + CFI_V
  )

write_csv(result,"bf/bf8349.csv")


ーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーー

df8365 <- read_csv("ProcessedBankData/8365_processed_data.csv")

result <- df8365 |> 
  mutate(年度 = str_sub(ファイル名, 5, 8)) %>%
  filter(!is.na(`金額（百万円）`), `金額（百万円）` != 0) %>%
  group_by(年度) %>%
  summarise(
    D  = sum(`金額（百万円）`[項目 %in% c("預金", "借用金", "社債")], na.rm = TRUE),
    DU = sum(`金額（百万円）`[項目 %in% c("預金利息", "借用金利息", "社債利息")], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(rD = DU / D)

Net_income <- df8365 |> 
  filter(項目 %in% c("当期純利益", "当期純利益又は当期純損失（△）")) |> 
  slice(seq(1, n(), by = 2)) |>  
  pull(`金額（百万円）`)

Net_asset <- df8365 |> 
  filter(項目 == "純資産の部合計") |> 
  pull(`金額（百万円）`)

CFO_V <- df8365 |> 
  filter(項目=="営業活動によるキャッシュ・フロー") |> 
  pull(`金額（百万円）`)

CFI_V <- df8365 |> 
  filter(項目=="投資活動によるキャッシュ・フロー") |> 
  pull(`金額（百万円）`)

result <- result |> 
  mutate(
    Net_income = Net_income,
    Net_asset  = Net_asset,
    CFO = CFO_V,
    CFI = CFI_V,
    FCF = CFO_V + CFI_V
  )

write_csv(result,"bf/bf8365.csv")

ーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーー

df8383 <- read_csv("ProcessedBankData/8383_processed_data.csv")

result <- df8383 |> 
  mutate(年度 = str_sub(ファイル名, 5, 8)) %>%
  filter(!is.na(`金額（百万円）`), `金額（百万円）` != 0) %>%
  group_by(年度) %>%
  summarise(
    D  = sum(`金額（百万円）`[項目 %in% c("預金", "借用金", "社債")], na.rm = TRUE),
    DU = sum(`金額（百万円）`[項目 %in% c("預金利息", "借用金利息", "社債利息")], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(rD = DU / D)

Net_income <- df8383 |> 
  filter(項目 %in% c("当期純利益", "当期純利益又は当期純損失（△）")) |> 
  slice(seq(1, n(), by = 2)) |>  
  pull(`金額（百万円）`)

Net_asset <- df8383 |> 
  filter(項目 == "純資産の部合計") |> 
  pull(`金額（百万円）`)

CFO_V <- df8383 |> 
  filter(項目=="営業活動によるキャッシュ・フロー") |> 
  pull(`金額（百万円）`)

CFI_V <- df8383 |> 
  filter(項目=="投資活動によるキャッシュ・フロー") |> 
  pull(`金額（百万円）`)

# 9行目に追加する行を作成
new_row <- tibble(
  年度 = "2023",
  D     = NA,
  DU    = NA,
  rD    = NA,
)

# 8行目まで + 新しい9行目 + 残り
result <- bind_rows(
  result[1:8, ],   # 元の1〜8行目
  new_row,         # 9行目に2023とNA
  result[9:nrow(result), ]  # 元の9行目以降を10行目以降にずらす
)



# 各列ベクトルを作成
Net_income_vec <- c(Net_income[1:8], NA, Net_income[9:length(Net_income)])
Net_asset_vec  <- c(Net_asset[1:8], NA, Net_asset[9:length(Net_asset)])
CFO_vec        <- c(CFO_V[1:8], NA, CFO_V[9:length(CFO_V)])
CFI_vec        <- c(CFI_V[1:8], NA, CFI_V[9:length(CFI_V)])
FCF_vec        <- CFO_vec + CFI_vec

# result に追加
result <- result |> 
  mutate(
    Net_income = Net_income_vec,
    Net_asset  = Net_asset_vec,
    CFO        = CFO_vec,
    CFI        = CFI_vec,
    FCF        = FCF_vec
  )


write_csv(result,"bf/bf8383.csv")
ーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーー

df8416 <- read_csv("ProcessedBankData/8416_processed_data.csv")

result <- df8416 |> 
  mutate(年度 = str_sub(ファイル名, 5, 8)) %>%
  filter(!is.na(`金額（百万円）`), `金額（百万円）` != 0) %>%
  group_by(年度) %>%
  summarise(
    D  = sum(`金額（百万円）`[項目 %in% c("預金", "借用金", "社債")], na.rm = TRUE),
    DU = sum(`金額（百万円）`[項目 %in% c("預金利息", "借用金利息", "社債利息")], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(rD = DU / D)

Net_income <- df8416 |> 
  filter(項目 %in% c("当期純利益", "当期純利益又は当期純損失（△）")) |> 
  slice(seq(1, n(), by = 2)) |>   # 奇数行抽出
  pull(`金額（百万円）`)

Net_asset <- df8416 |> 
  filter(項目 == "純資産の部合計") |> 
  pull(`金額（百万円）`)

CFO_V <- df8416 |> 
  filter(項目=="営業活動によるキャッシュ・フロー") |> 
  pull(`金額（百万円）`)

CFI_V <- df8416 |> 
  filter(項目=="投資活動によるキャッシュ・フロー") |> 
  pull(`金額（百万円）`)

result <- result |> 
  mutate(
    Net_income = Net_income,
    Net_asset  = Net_asset,
    CFO = CFO_V,
    CFI = CFI_V,
    FCF = CFO_V + CFI_V
  )

write_csv(result, "bf/bf8416.csv")

ーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーー

df8537 <- read_csv("ProcessedBankData/8537_processed_data.csv")

result <- df8537 |> 
  mutate(年度 = str_sub(ファイル名, 5, 8)) %>%
  filter(!is.na(`金額（百万円）`), `金額（百万円）` != 0) %>%
  group_by(年度) %>%
  summarise(
    D  = sum(`金額（百万円）`[項目 %in% c("預金", "借用金", "社債")], na.rm = TRUE),
    DU = sum(`金額（百万円）`[項目 %in% c("預金利息", "借用金利息", "社債利息")], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(rD = DU / D)

Net_income <- df8537 |> 
  filter(項目 %in% c("当期純利益", "当期純利益又は当期純損失（△）")) |> 
  slice(seq(1, n(), by = 2)) |>   # 奇数行抽出
  pull(`金額（百万円）`)

Net_asset <- df8537 |> 
  filter(項目 == "純資産の部合計") |> 
  pull(`金額（百万円）`)

CFO_V <- df8537 |> 
  filter(項目=="営業活動によるキャッシュ・フロー") |> 
  pull(`金額（百万円）`)

CFI_V <- df8537 |> 
  filter(項目=="投資活動によるキャッシュ・フロー") |> 
  pull(`金額（百万円）`)

result <- result |> 
  mutate(
    Net_income = Net_income,
    Net_asset  = Net_asset,
    CFO = CFO_V,
    CFI = CFI_V,
    FCF = CFO_V + CFI_V
  )

write_csv(result, "bf/bf8537.csv")

ーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーー

df8542 <- read_csv("ProcessedBankData/8542_processed_data.csv")

result <- df8542 |> 
  mutate(年度 = str_sub(ファイル名, 5, 8)) %>%
  filter(!is.na(`金額（百万円）`), `金額（百万円）` != 0) %>%
  group_by(年度) %>%
  summarise(
    D  = sum(`金額（百万円）`[項目 %in% c("預金", "借用金", "社債")], na.rm = TRUE),
    DU = sum(`金額（百万円）`[項目 %in% c("預金利息", "借用金利息", "社債利息")], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(rD = DU / D)

Net_income <- df8542 |> 
  filter(項目 %in% c("当期純利益", "当期純利益又は当期純損失（△）")) |> 
  slice(seq(1, n(), by = 2)) |>  
  pull(`金額（百万円）`)

Net_asset <- df8542 |> 
  filter(項目 == "純資産の部合計") |> 
  pull(`金額（百万円）`)

CFO_V <- df8542 |> 
  filter(項目=="営業活動によるキャッシュ・フロー") |> 
  pull(`金額（百万円）`)

CFI_V <- df8542 |> 
  filter(項目=="投資活動によるキャッシュ・フロー") |> 
  pull(`金額（百万円）`)

# 8行目に追加する行を作成（7行目まで + 8行目NA）
new_row <- tibble(
  年度 = "2022",
  D     = NA,
  DU    = NA,
  rD    = NA
)

# 7行目まで + 新しい8行目 + 残りを9行目以降にずらす
result <- bind_rows(
  result[1:7, ],
  new_row,
  result[8:nrow(result), ]
)

# 各列ベクトルを作成
Net_income_vec <- c(Net_income[1:7], NA, Net_income[8:length(Net_income)])
Net_asset_vec  <- c(Net_asset[1:7], NA, Net_asset[8:length(Net_asset)])
CFO_vec        <- c(CFO_V[1:7], NA, CFO_V[8:length(CFO_V)])
CFI_vec        <- c(CFI_V[1:7], NA, CFI_V[8:length(CFI_V)])
FCF_vec        <- CFO_vec + CFI_vec

# result に追加
result <- result |> 
  mutate(
    Net_income = Net_income_vec,
    Net_asset  = Net_asset_vec,
    CFO        = CFO_vec,
    CFI        = CFI_vec,
    FCF        = FCF_vec
  )

write_csv(result, "bf/bf8542.csv")
ーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーー

df8562 <- read_csv("ProcessedBankData/8562_processed_data.csv")

result <- df8562 |> 
  mutate(年度 = str_sub(ファイル名, 5, 8)) %>%
  filter(!is.na(`金額（百万円）`), `金額（百万円）` != 0) %>%
  group_by(年度) %>%
  summarise(
    D  = sum(`金額（百万円）`[項目 %in% c("預金", "借用金", "社債")], na.rm = TRUE),
    DU = sum(`金額（百万円）`[項目 %in% c("預金利息", "借用金利息", "社債利息")], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(rD = DU / D)

Net_income <- df8562 |> 
  filter(項目 %in% c("当期純利益", "当期純利益又は当期純損失（△）")) |> 
  slice(seq(1, n(), by = 2)) |>   # 奇数行抽出
  pull(`金額（百万円）`)

Net_asset <- df8562 |> 
  filter(項目 == "純資産の部合計") |> 
  pull(`金額（百万円）`)

CFO_V <- df8562 |> 
  filter(項目=="営業活動によるキャッシュ・フロー") |> 
  pull(`金額（百万円）`)

CFI_V <- df8562 |> 
  filter(項目=="投資活動によるキャッシュ・フロー") |> 
  pull(`金額（百万円）`)

result <- result |> 
  mutate(
    Net_income = Net_income,
    Net_asset  = Net_asset,
    CFO = CFO_V,
    CFI = CFI_V,
    FCF = CFO_V + CFI_V
  )

write_csv(result, "bf/bf8562.csv")

ーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーー

df8563 <- read_csv("ProcessedBankData/8563_processed_data.csv")

result <- df8563 |> 
  mutate(年度 = str_sub(ファイル名, 5, 8)) %>%
  filter(!is.na(`金額（百万円）`), `金額（百万円）` != 0) %>%
  group_by(年度) %>%
  summarise(
    D  = sum(`金額（百万円）`[項目 %in% c("預金", "借用金", "社債")], na.rm = TRUE),
    DU = sum(`金額（百万円）`[項目 %in% c("預金利息", "借用金利息", "社債利息")], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(rD = DU / D)

Net_income <- df8563 |> 
  filter(項目 %in% c("当期純利益", "当期純利益又は当期純損失（△）")) |> 
  slice(seq(1, n(), by = 2)) |>   # 奇数行抽出
  pull(`金額（百万円）`)

Net_asset <- df8563 |> 
  filter(項目 == "純資産の部合計") |> 
  pull(`金額（百万円）`)

CFO_V <- df8563 |> 
  filter(項目=="営業活動によるキャッシュ・フロー") |> 
  pull(`金額（百万円）`)

CFI_V <- df8563 |> 
  filter(項目=="投資活動によるキャッシュ・フロー") |> 
  pull(`金額（百万円）`)

result <- result |> 
  mutate(
    Net_income = Net_income,
    Net_asset  = Net_asset,
    CFO = CFO_V,
    CFI = CFI_V,
    FCF = CFO_V + CFI_V
  )

write_csv(result, "bf/bf8563.csv")

ーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーー

df5838 <- read_csv("Processed_PrimeBank/5838.csv") |> 
  mutate(金額 = str_replace_all(金額, ",", ""),  # カンマを除去
         金額 = as.numeric(金額))               # 数値に変換

result <- df5838 |> 
  mutate(年度 = str_sub(ファイル名, 5, 8)) %>%
  filter(!is.na(金額), 金額 != 0) %>%
  group_by(年度) %>%
  summarise(
    D  = sum(金額[項目 %in% c("預金", "借用金", "社債")], na.rm = TRUE),
    DU = sum(金額[項目 %in% c("預金利息", "借用金利息", "社債利息")], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(rD = DU / D)

Net_income <- df5838 |> 
  filter(項目 %in% c("当期純利益", "当期純利益又は当期純損失（△）")) |> 
  slice(seq(1, n(), by = 2)) |>  
  pull(金額)

Net_asset <- df5838 |> 
  filter(項目 == "純資産の部合計") |> 
  pull(金額)

CFO_V <- df5838 |> 
  filter(項目=="営業活動によるキャッシュ・フロー") |> 
  pull(金額)

CFI_V <- df5838 |> 
  filter(項目=="投資活動によるキャッシュ・フロー") |> 
  pull(金額)

result <- result |> 
  mutate(
    Net_income = Net_income,
    Net_asset  = Net_asset,
    CFO = CFO_V,
    CFI = CFI_V,
    FCF = CFO_V + CFI_V
  )

write_csv(result, "bf/bf5838.csv")



