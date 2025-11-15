library(tidyverse)

df7150 <- read_csv("af/7150.csv")
df7161 <- read_csv("af/7161.csv")
df8349 <- read_csv("af/8349.csv")
df8365 <- read_csv("af/8365.csv")
df8383 <- read_csv("af/8383.csv")
df8416 <- read_csv("af/8416.csv")
df8537 <- read_csv("af/8537.csv")
df8542 <- read_csv("af/8542.csv")
df8562 <- read_csv("af/8562.csv")
df8563 <- read_csv("af/8563.csv")


calc_wacc <- function(df, Rf = 0.0157, Rm = 0.05, Tax = 0.2320) {
  
  wacc <- df |> 
    mutate(
      rE = Rf + beta * (Rm - Rf),     # CAPMで株主資本コスト
      V  = D + NP,                    # 企業価値（負債＋株主資本）
      WACC = (NP / V) * rE + (D / V) * rD * (1 - Tax)
    )
  
  return(wacc)
}


wacc7150 <- calc_wacc(df7150)

wacc7161 <- calc_wacc(df7161)

wacc8349 <- calc_wacc(df8349)

wacc8365 <- calc_wacc(df8365)

wacc8383 <- calc_wacc(df8383)

wacc8416 <- calc_wacc(df8416)

wacc8537 <- calc_wacc(df8537)

wacc8542 <- calc_wacc(df8542)

wacc8562 <- calc_wacc(df8562)

wacc8563 <- calc_wacc(df8563)
# -----------------------------
# 企業価値計算関数まとめ
# -----------------------------

calculate_valuation_env <- function(codes, g = 0.02, rE = 0.08, path = "Valuation_results.csv") {
  
  # DCF計算（過去データ加重平均、将来5年割引）
  DCF_value <- function(df, g) {
    fcf <- df$FCF
    years <- df$年度
    weights <- ifelse(is.na(years), 0, seq_along(fcf))
    weights <- weights / sum(weights)
    FCF_base <- sum(fcf * weights, na.rm = TRUE)
    
    FCF_future <- FCF_base * (1 + g)^(1:5)
    discount_factors <- (1 + df$WACC[1])^(1:5)
    sum(FCF_future / discount_factors)
  }
  
  # RIM計算（Net_income の加重平均使用）
  RIM_value <- function(df, rE, g) {
    netinc <- df$Net_income
    years <- df$年度
    weights <- ifelse(is.na(years), 0, seq_along(netinc))
    weights <- weights / sum(weights)
    NetIncome_base <- sum(netinc * weights, na.rm = TRUE)
    
    df$Net_asset[nrow(df)] + (NetIncome_base * (1 + g)) / rE
  }
  
  # DDM計算（配当ベース）
  DDM_value <- function(df, rE, g) {
    netinc <- df$Net_income
    payratio <- df$Pay_ratio
    years <- df$年度
    weights <- ifelse(is.na(years), 0, seq_along(netinc))
    weights <- weights / sum(weights)
    
    Div_base <- sum(netinc * payratio * weights, na.rm = TRUE)
    Div_future <- Div_base * (1 + g)^(1:5)
    discount_factors <- (1 + rE)^(1:5)
    sum(Div_future / discount_factors)
  }
  
  # 結果格納用
  results <- tibble(df = character(),
                    DCF_value = numeric(),
                    RIM_value = numeric(),
                    DDM_value = numeric(),
                    g = numeric(),
                    NP_latest = numeric())
  
  # 企業ごとにループ
  for(code in codes) {
    # 環境からデータフレーム取得
    df_name <- paste0("wacc", code)
    if(!exists(df_name)) next
    df <- get(df_name)
    
    # 各モデル計算
    dcf <- DCF_value(df, g)
    rim <- RIM_value(df, rE, g)
    ddm <- DDM_value(df, rE, g)
    np_latest <- tail(df$NP, 1)  # 最新年度のNP
    
    results <- bind_rows(results,
                         tibble(df = code,
                                DCF_value = dcf,
                                RIM_value = rim,
                                DDM_value = ddm,
                                g = g,
                                NP_latest = np_latest))
  }
  
  write_csv(results, path)
  return(results)
}

# 使用例
codes <- c("7150","7161","8349","8365","8383","8416","8537","8542","8562","8563")
valuation_results <- calculate_valuation_env(codes, g = 0.02, rE = 0.08)

write_csv(valuation_results,"results/val_results.csv")

# 企業コードのベクトル
companies <- c("7150","7161","8349","8365","8383","8416","8537","8542","8562","8563")

# 空のリストで結果を格納
results_list <- list()

for(code in companies){
  # データフレーム名を作成
  df_name <- paste0("wacc", code)
  
  # get() で名前からデータフレームを取得
  df <- get(df_name)
  
  # 2024年のNPを抽出
  NP_2024 <- df %>%
    filter(年度 == "2024") %>%
    pull(NP)
  
  # 結果をデータフレーム化してリストに追加
  results_list[[code]] <- tibble(
    df  = df_name,
    NP  = NP_2024
  )
}

# リストを結合して1つのデータフレームに
final_result <- bind_rows(results_list)



ALL_df <- read_csv("Valuation_results.csv")
ALL_df <- ALL_df |> 
  mutate(
    RIMDDM = (RIM_value + DDM_value) / 2
  )
