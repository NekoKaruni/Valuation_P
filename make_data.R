settings <- list(
  
  "7150" = list(
    file = "ProcessedBankData/7150_processed_data.csv",
    price = c(1395, 1426, 1370, 1380, 658, 661, 619, 543, 460, 515),   # 10年分
    N = 8416000,           # 発行済株式数
    beta = 1.0803,
    Pay_ratio = c(0.4946, 0.4299, 0.2924, 0.4520, 0.3431, 0, 0.1302, 0.3003, 0.2187, 0.2157)
  ),
  
  "7161" = list(
    file = "ProcessedBankData/7161_processed_data.csv",
    price = c(2290,1990,1980,1950,1250,1130,821,642,440,545),
    N = 72840263,
    beta = 0.3849,
    Pay_ratio = c(0.896, 0.8976, 0.8771,  0.9041, 0.8865, 0.7246, 1.0757, 1.8867, 0, 0)
  ),
  
  "8349" = list(
    file = "ProcessedBankData/8349_processed_data.csv",
    price = c(1570, 1580, 1580, 1513, 1111, 1027, 1125, 1006, 1026, 1170),
    N = 9509963,
    beta = 0.6125,
    Pay_ratio = c(0.3501, 0.2679, 0.2556, 0.4387, 0.3650, 0.3412, 0.4008, 0.3701, 0.3342, 0.3580)
  ),
  
  "8365" = list(
    file = "ProcessedBankData/8365_processed_data.csv",
    price = c(2500, 4440, 4500, 4360, 3095, 2412, 2944, 1791, 1764, 1695),
    N = 5444400,
    beta = 1.0889,
    Pay_ratio = c(0.2384, 0.2827, 0.2405, 0.2141, 0.1965, 0.3824, 0.5156, 0.8902, 0.2862, 0.4442)
  ),
  
  "8383" = list(
    file = "ProcessedBankData/8383_processed_data.csv",
    price = c(2380, 2170, 1936, 1808, 1424, 1387, 1133, 1163, 1146, 1358),
    N = 9619398,
    beta = 0.5806,
    Pay_ratio = c(0.2586, 0.2667, 0.4340, 0.5135, 0.5989, 0.5236, 0.4829, 0.5389, 0.4658, 0.4506)
  ),
  
  "8416" = list(
    file = "ProcessedBankData/8416_processed_data.csv",
    price = c(1590, 1420, 1340, 1319, 740, 889, 722, 770, 716, 935 ),
    N = 10244800,
    beta = 0.8731,
    Pay_ratio = c(0.707, 0.932, 0.1322, 0.1730, 0.3535, 0.2356, 0.4178, 0.1878, 0.1958, 0.2475)
  ),
  
  "8537" = list(
    file = "ProcessedBankData/8537_processed_data.csv",
    price = c(2210, 2460, 2630, 2520, 1781, 1672, 1339, 1360, 1254, 1282),
    N = 9671400,
    beta = 0.6017,
    Pay_ratio = c(0.3001, 0.1672, 0.1551, 0.1346, 0.1872, 0.4200, 0.3054, 0.2326, 0.3744, 0.2814)
  ),
  
  "8542" = list(
    file = "ProcessedBankData/8542_processed_data.csv",
    price = c(1780, 1740, 1585, 1558, 1081, 1074, 1060, 1070, 1045, 1155),
    N = 11679030,
    beta = 0.5599,
    Pay_ratio = c(0.3558, 0.3178, 0.3136, 0.4206, 0.5189, 0.5502, 0.4768, 0.4226, 0.3724, 0.4613)
  ),
  
  "8562" = list(
    file = "ProcessedBankData/8562_processed_data.csv",
    price = c(940, 960, 980, 944, 390, 273, 198, 235, 258, 235),
    N = 34900000,
    beta = 1.1149,
    Pay_ratio = c(0.1045, 0.1959, 0.4255, 0, 0.1390, 0.1358, 0 , 0.1693, 0.1769, 0.1245)
  ),
  
  "8563" = list(
    file = "ProcessedBankData/8563_processed_data.csv",
    price = c(1410, 2080, 1680, 1651, 601, 648, 639, 761, 681, 740),
    N = 12701000,
    beta = 0.8016,
    Pay_ratio = c(0.1426, 0.1532, 0.2764, 0.2986, 0.3195, 0.3742, 0.4017, 0.3294, 0.3121, 0.3131)
  ),
  
  "5838" = list(
    file = "Processed_PrimeBank/5838.csv",
    price = c(1856, 1856, 1856,1856, 1856, 1856,1856, 1856, 1856,2119),
    N = 174498580,
    beta = 1,
    Pay_ratio = c(0, 0, 0, 0, 0, 0, 0.4946, 0.4299, 0.2924, 0.4520)#仮に１とおく
  ),
  
  "8331" = list(
    file = "Processed_PrimeBank/8331.csv",
    price = c(786, 853, 727, 957, 604, 615, 568, 668, 968, 1018),
    N = 805521087,
    beta = 0.17,
    Pay_ratio = c(0.4946, 0.4299, 0.2924, 0.4520, 0.3431, 0, 0.1302, 0.3003, 0.2187, 0.2157)
  ),
  
  "8334" = list(
    file = "Processed_PrimeBank/8334.csv",
    price = c(784, 706, 648, 688, 451, 380, 318, 355, 509, 696),
    N = 395888177,
    beta = 0.36,
    Pay_ratio = c(0.4946, 0.4299, 0.2924, 0.4520, 0.3431, 0, 0.1302, 0.3003, 0.2187, 0.2157)
  ),
  
  "8410" = list(
    file = "Processed_PrimeBank/8410.csv",
    price = c(505, 524, 338, 391, 304, 354, 218, 240, 263, 300),
    N = 1179308000,
    beta = 0.3,
    Pay_ratio = c(0.4946, 0.4299, 0.2924, 0.4520, 0.3431, 0, 0.1302, 0.3003, 0.2187, 0.2157)
  )
  
)


library(tidyverse)
library(dplyr)
library(stringr)
library(tidyverse)

if(!dir.exists("data")) dir.create("data")

# -----------------------------
# 1. 企業ごとの設定情報を読み取り、基本表作成
# -----------------------------
basic_info <- tibble(
  code = character(),
  file = character(),
  N = numeric(),
  beta = numeric(),
  price = I(list()),
  Pay_ratio = I(list())
)

for(code in names(settings)) {
  s <- settings[[code]]
  basic_info <- basic_info %>%
    add_row(
      code = code,
      file = s$file,
      N = s$N,
      beta = s$beta,
      price = list(s$price),
      Pay_ratio = list(s$Pay_ratio)
    )
}

# -----------------------------
# 2. 各企業のCSVを読み込み、10年分の表に整形して保存
# -----------------------------
for(i in 1:nrow(basic_info)) {
  code <- basic_info$code[i]
  file <- basic_info$file[i]
  
  df <- read_csv(file)
  
  # 年度を作る（2015年～2024年の10年分）
  years <- as.character(2015:2024)
  
  # 例：負債、利息、当期純利益、株主資本を抽出して10行に
  D <- df %>% filter(項目 %in% c("預金", "借用金", "社債")) %>%
    slice(1:10) %>% pull(`金額（百万円）`)
  DU <- df %>% filter(項目 %in% c("預金利息", "借用金利息", "社債利息")) %>%
    slice(1:10) %>% pull(`金額（百万円）`)
  Net_Income <- df %>% filter(項目 %in% c("当期純利益", "当期純利益又は当期純損失（△）")) %>%
    slice(1:10) %>% pull(`金額（百万円）`)
  Equity <- df %>% filter(項目 %in% c("株主資本合計", "その他の包括利益累計額合計")) %>%
    slice(1:10) %>% pull(`金額（百万円）`)
  
  # 10行の表作成
  result <- tibble(
    年度 = years,
    D = D,
    DU = DU,
    D_cost = DU / D,
    Net_Income = Net_Income,
    Equity = Equity
  )
  
  # 保存
  write_csv(result, file.path("data", paste0("result_", code, ".csv")))
}


df1 <- read_csv("data/result_7150.csv")

df1 <- df1 |> 
  mutate(
    price = c(1395, 1426, 1370, 1380, 658, 661, 619, 543, 460, 515),   # 10年分
    N = 8416000,           # 発行済株式数
    beta = 1.0803,
    Pay_ratio = c(0.4946, 0.4299, 0.2924, 0.4520, 0.3431, 0, 0.1302, 0.3003, 0.2187, 0.2157)
  )
