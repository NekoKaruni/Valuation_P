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


#関数実行例　
## DCF_value(df$FCF, g = 0.01, wacc = 0.07)
## RIM_value(df$Net_income, df$Net_asset, rE = 0.08, g = 0.01)
## DDM_value(df$Net_income, df$Pay_ratio, rE = 0.08, g = 0.01)

##ーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーー##
# dfには FCF, WACC の列が含まれていることを前提
DCF_value <- function(df, g = 0.01) {
  
  # 過去FCFを加重平均（最新ほど重み大）
  n <- nrow(df)
  weights <- seq(1, n) # 1,2,...,n
  weights <- weights / sum(weights)
  FCF_base <- sum(df$FCF * weights)
  
  # 将来5年分の予測FCF
  FCF_future <- FCF_base * (1 + g)^(1:5)
  
  # 割引係数
  discount_factors <- (1 + df$WACC[1])^(1:5)
  
  # 各年を現在価値に割り引き
  PV <- FCF_future / discount_factors
  
  # 合計 = 企業価値
  sum(PV)
}




# 使い方
# DCF_value(wacc7150,g = 0.01)


##ーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーー##
RIM_value <- function(df, g = 0.01) {
  NI <- df$Net_income
  NA <- df$Net_asset
  rE <- df$rE
  
  residual_income <- NI - rE * NA
  equity_value <- NA + residual_income * (1 + g) / (rE - g)
  return(equity_value)
}

##ーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーー##

DDM_value <- function(df, g = 0.01) {
  NI   <- df$Net_income
  pay  <- df$Pay_ratio
  rE   <- df$rE
  
  Div <- NI * pay
  equity_value <- Div * (1 + g) / (rE - g)
  return(equity_value)
}
