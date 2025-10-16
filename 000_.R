# ==============================================================================
# 企業価値評価およびモンテカルロ・シミュレーション用 Rスクリプト
# ==============================================================================

# 0. 必要なライブラリの読み込み
# ------------------------------------------------------------------------------
# 初回のみ install.packages("tidyverse") などを実行してください
suppressPackageStartupMessages({
  library(tidyverse)
  library(conflicted)
})

# dplyrの関数を優先的に使用する設定
conflict_prefer("filter", "dplyr")
conflict_prefer("lag",    "dplyr")


# ==============================================================================
# Part 1: パラメータ設定 (Assumptions & Parameters)
# ==============================================================================
# 分析の前提条件はすべてこのセクションで設定します。
# 別の企業を分析する際は、主にこの部分を書き換えます。

# 企業データ関連
company_code <- "7150"
file_path_financials <- paste0("ProcessedBankData/", company_code, "_processed_data.csv")
file_path_topix <- "^tpx_y.csv"

# 時価総額 (E) の算出用パラメータ
N <- 8416000     # 発行済み株式総数
P <- 459         # 現在の株価（円）

# 株主資本コスト (rE) の算出用パラメータ (CAPM)
rF <- 0.01570    # リスクフリーレート（10年国債利回りなど）
beta <- 1.08     # ベータ値
T_tax <- 0.2320  # 実効税率

# FCF予測とターミナルバリュー (TV) の算出用パラメータ
forecast_period <- 5 # FCFの明示的予測期間（年）
g <- 0.00          # 永久成長率


# ==============================================================================
# Part 2: 関数定義 (Function Definitions)
# ==============================================================================
# 計算の各ステップを独立した関数として定義します。

#' データの読み込み
load_data <- function(financials_path, topix_path) {
  df <- read_csv(financials_path, show_col_types = FALSE)
  topix <- read_csv(topix_path, show_col_types = FALSE)
  return(list(df = df, topix = topix))
}

#' 株主資本コスト (rE) の計算
calculate_rE <- function(rF, beta, topix, verbose = TRUE) {
  topix_open <- topix$Close[1:(nrow(topix)-1)]
  topix_close <- topix$Close[2:nrow(topix)]
  topix_return <- (topix_close - topix_open) / topix_open
  Er <- mean(topix_return)
  market_risk_premium <- Er - rF
  rE <- rF + beta * market_risk_premium
  
  if (verbose) {
    cat("--- 株主資本コスト (rE) 関連 ---\n")
    cat(sprintf("市場の期待リターン (Er): %.3f%%\n", Er * 100))
    cat(sprintf("市場リスクプレミアム: %.3f%%\n", market_risk_premium * 100))
    cat(sprintf("株主資本コスト (rE): %.3f%%\n\n", rE * 100))
  }
  return(rE)
}

#' WACC（加重平均資本コスト）の計算
calculate_wacc <- function(df, E, rE, T_tax, verbose = TRUE) {
  d_items <- c("預金", "借用金", "社債")
  D <- sum(as.numeric(df$`金額（百万円）`[df$項目 %in% d_items]))
  
  rd_items <- c("資金調達費用", "預金利息", "借用金利息", "社債利息")
  interest_cost <- sum(as.numeric(df$`金額（百万円）`[df$項目 %in% rd_items]))
  rD <- interest_cost / D
  
  wacc <- (D / (D + E)) * rD * (1 - T_tax) + (E / (D + E)) * rE
  
  if (verbose) {
    cat("--- WACC 関連 ---\n")
    cat(sprintf("負債総額 (D): %s 百万円\n", format(round(D, 0), big.mark = ",")))
    cat(sprintf("時価総額 (E): %s 百万円\n", format(round(E / 1e6, 0), big.mark = ",")))
    cat(sprintf("負債コスト (rD): %.3f%%\n", rD * 100))
    cat(sprintf("WACC: %.3f%%\n\n", wacc * 100))
  }
  return(wacc)
}

#' 企業価値 (Enterprise Value) の算出
calculate_enterprise_value <- function(df, wacc, g, forecast_period, verbose = TRUE) {
  cfo_df <- df %>% filter(項目 == "営業活動によるキャッシュ・フロー") %>% rename(年度 = ファイル名, 金額 = `金額（百万円）`)
  cfi_df <- df %>% filter(項目 == "投資活動によるキャッシュ・フロー") %>% rename(年度 = ファイル名, 金額 = `金額（百万円）`)
  fcf_df <- inner_join(cfo_df, cfi_df, by = "年度", suffix = c("_cfo", "_cfi")) %>%
    mutate(FCF = 金額_cfo - 金額_cfi) %>%
    arrange(年度)
  
  last_fcf <- tail(fcf_df$FCF, 1)
  
  fcf_forecast <- numeric(forecast_period)
  if (forecast_period > 0) {
    fcf_forecast[1] <- last_fcf * (1 + g)
    for (i in 2:forecast_period) {
      fcf_forecast[i] <- fcf_forecast[i-1] * (1 + g)
    }
  }
  
  fcf_n_plus_1 <- tail(fcf_forecast, 1) * (1 + g)
  tv <- fcf_n_plus_1 / (wacc - g)
  
  pv_fcf <- sum(fcf_forecast / (1 + wacc)^(1:forecast_period))
  pv_tv <- tv / (1 + wacc)^forecast_period
  enterprise_value <- pv_fcf + pv_tv
  
  if (verbose) {
    cat("--- 企業価値評価関連 ---\n")
    cat(sprintf("最終年度のFCF実績: %s 百万円\n", format(round(last_fcf, 0), big.mark = ",")))
    cat(sprintf("ターミナルバリュー (TV): %s 百万円\n", format(round(tv, 0), big.mark = ",")))
  }
  return(enterprise_value)
}


# ==============================================================================
# Part 3: 基本分析の実行 (Base Case Execution)
# ==============================================================================
# パラメータを使い、定義した関数を順番に呼び出して、基本となる企業価値を算出します。

cat("=========================================\n")
cat("          基本分析 (Base Case)\n")
cat("=========================================\n")

# 1. データの読み込み
datasets <- load_data(file_path_financials, file_path_topix)
df_main <- datasets$df
df_topix <- datasets$topix

# 2. 時価総額 (E) の計算
E_value <- N * P

# 3. 株主資本コスト (rE) の計算
rE_value <- calculate_rE(rF, beta, df_topix)

# 4. WACCの計算
wacc_value <- calculate_wacc(df_main, E_value, rE_value, T_tax)

# 5. 企業価値の算出
enterprise_value_base <- calculate_enterprise_value(df_main, wacc_value, g, forecast_period)

# 6. 最終結果の表示
cat(sprintf("\nDCF法による企業価値 (基本分析): %s 億円\n", format(round(enterprise_value_base / 100, 2), big.mark = ",")))
cat("=========================================\n\n")


# ==============================================================================
# Part 4: モンテカルロ・シミュレーション (Monte Carlo Simulation)
# ==============================================================================
# 主要な変数を確率分布として捉え、企業価値の分布を算出します。

cat("=========================================\n")
cat("   モンテカルロ・シミュレーション\n")
cat("=========================================\n")

# 1. シミュレーションの設定
set.seed(123) # 乱数を固定し、毎回同じ結果を再現
n_sims <- 10000 # シミュレーションの試行回数

beta_mean <- beta; beta_sd <- 0.10
g_mean <- g; g_sd <- 0.005

# 2. シミュレーションの実行
simulation_results <- numeric(n_sims)
for (i in 1:n_sims) {
  sim_beta <- rnorm(1, mean = beta_mean, sd = beta_sd)
  sim_g    <- rnorm(1, mean = g_mean, sd = g_sd)
  
  sim_rE   <- calculate_rE(rF, sim_beta, df_topix, verbose = FALSE)
  sim_wacc <- calculate_wacc(df_main, E_value, sim_rE, T_tax, verbose = FALSE)
  
  if (sim_wacc <= sim_g) {
    simulation_results[i] <- NA; next
  }
  
  simulation_results[i] <- calculate_enterprise_value(df_main, sim_wacc, sim_g, forecast_period, verbose = FALSE)
}

# 3. シミュレーション結果の分析
# ------------------------------------------------------------------------------
valid_results <- na.omit(simulation_results)
cat(sprintf("計算が成功したシミュレーションの数: %d / %d\n\n", length(valid_results), n_sims))

cat("--- 要約統計量 (単位: 億円) ---\n")
print(summary(valid_results / 100))


# ★★★ ここから修正 ★★★

# 95%信頼区間を計算
confidence_interval_95 <- quantile(valid_results / 100, probs = c(0.025, 0.975))

cat("\n--- 95%信頼区間 ---\n")
cat(sprintf("下限値 (2.5パーセンタイル): %.2f 億円\n", confidence_interval_95[1]))
cat(sprintf("上限値 (97.5パーセンタイル): %.2f 億円\n\n", confidence_interval_95[2]))
cat(sprintf("中央値:%.2f 億円\n\n",median(valid_results / 100)))
cat(sprintf("現在の時価総額: %.2f 億円\n\n",E_value/100000000))
# ★★★ ここまで修正 ★★★


# 4. 結果の可視化
# ------------------------------------------------------------------------------
hist(valid_results / 100, breaks = 50,
     main = "企業価値のモンテカルロ・シミュレーション結果 (95%信頼区間)",
     xlab = "企業価値（億円）", ylab = "頻度", col = "skyblue", border = "white")

# 信頼区間のラインをプロット
abline(v = confidence_interval_95[1], col = "blue", lty = 2, lwd = 2)
abline(v = confidence_interval_95[2], col = "blue", lty = 2, lwd = 2)
abline(v = median(valid_results / 100), col = "red", lwd = 2)

legend("topright",
       legend = c("中央値", "95%信頼区間"), # 凡例も修正
       col = c("red", "blue"),
       lwd = 2, lty = c(1, 2), cex = 0.8)