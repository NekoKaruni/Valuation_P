# ==============================================================================
# 企業価値評価（配当割引モデル）およびモンテカルロ・シミュレーション用 Rスクリプト
# ==============================================================================

# 0. 必要なライブラリの読み込み
# ------------------------------------------------------------------------------
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

# --- 企業データ関連 ---
company_code <- "7150"
file_path_financials <- paste0("ProcessedBankData/", company_code, "_processed_data.csv")
file_path_topix <- "^tpx_y.csv"

# --- 株主資本コスト (rE) の算出用パラメータ (CAPM) ---
rF <- 0.01570   # リスクフリーレート
beta <- 1.08    # ベータ値（基本分析用）

# --- DDM評価および継続価値 (TV) の算出用パラメータ ---
future_payout_ratio <- 0.30 # 将来の配当性向の仮定値
g <- 0.01                   # 永久成長率（基本分析用）

# --- 株式価値・時価総額の算出用 ---
N <- 8416000      # 発行済株式総数
P <- 450          # 現在の株価（円）※分析時点の株価に設定

# --- モンテカルロ・シミュレーションの設定 ---
n_sims <- 10000  # シミュレーション回数
set.seed(123)    # 乱数の固定化（結果の再現性のため）


# ==============================================================================
# Part 2: 関数定義 (Function Definitions)
# ==============================================================================

#' データの読み込みと前処理
load_and_prepare_data <- function(financials_path) {
  df <- read_csv(financials_path, show_col_types = FALSE)
  
  # 当期純利益の抽出
  net_income_df <- df %>%
    filter(項目 %in% c("当期純利益", "当期純利益又は当期純損失（△）")) %>%
    slice(which(row_number() %% 2 != 0)) %>%
    select(No, `金額（百万円）`)
  
  return(net_income_df)
}

#' 株主資本コスト (rE) の計算
calculate_rE <- function(rF, beta, topix_path, verbose = TRUE) {
  topix <- read_csv(topix_path, show_col_types = FALSE)
  topix_open <- topix$Close[1:(nrow(topix) - 1)]
  topix_close <- topix$Close[2:nrow(topix)]
  topix_return <- (topix_close - topix_open) / topix_open
  
  Er <- mean(topix_return, na.rm = TRUE)
  market_risk_premium <- Er - rF
  rE <- rF + beta * market_risk_premium
  
  if (verbose) {
    cat("--- 株主資本コスト (rE) 関連 ---\n")
    cat(sprintf("株主資本コスト (rE): %.3f%%\n\n", rE * 100))
  }
  return(rE)
}

#' DDMによる株式価値の算出
calculate_ddm_value <- function(net_income_df, rE, payout_ratio, g, verbose = TRUE) {
  forecast_period <- nrow(net_income_df)
  
  # 将来の配当を予測
  future_dividends <- net_income_df$`金額（百万円）` * payout_ratio
  
  # 予測期間の配当の現在価値合計
  t <- 1:forecast_period
  pv_dividends <- sum(future_dividends / (1 + rE)^t)
  
  # 継続価値（Terminal Value）の計算
  last_dividend <- tail(future_dividends, 1)
  terminal_value <- last_dividend * (1 + g) / (rE - g)
  pv_terminal_value <- terminal_value / (1 + rE)^forecast_period
  
  # 株式価値の計算
  equity_value <- pv_dividends + pv_terminal_value
  
  if (verbose) {
    cat("--- DDMによる企業価値評価結果 ---\n")
    cat(sprintf("株式価値: %s 百万円\n", format(round(equity_value, 2), big.mark = ",")))
    cat(sprintf("  - 予測期間の配当PV合計: %s 百万円\n", format(round(pv_dividends, 2), big.mark = ",")))
    cat(sprintf("  - 継続価値のPV: %s 百万円\n\n", format(round(pv_terminal_value, 2), big.mark = ",")))
  }
  
  return(equity_value)
}


# ==============================================================================
# Part 3: 基本分析の実行 (Base Case Execution)
# ==============================================================================
cat("=========================================\n")
cat("       基本分析 (Base Case) - DDM\n")
cat("=========================================\n")

# 1. データの読み込み
net_income_base <- load_and_prepare_data(file_path_financials)

# 2. 株主資本コスト (rE) の計算
rE_base <- calculate_rE(rF, beta, file_path_topix)

# 3. DDMによる株式価値の算出
equity_value_base <- calculate_ddm_value(net_income_base, rE_base, future_payout_ratio, g)

# 4. 現在の時価総額と比較
market_cap_oku <- (N * P) / 100000000
equity_value_base_oku <- equity_value_base / 100

cat(sprintf("DDMによる株式価値 (基本分析): %.2f 億円\n", equity_value_base_oku))
cat(sprintf("現在の時価総額: %.2f 億円\n\n", market_cap_oku))


# ==============================================================================
# Part 4: モンテカルロ・シミュレーション (Monte Carlo Simulation)
# ==============================================================================
cat("=========================================\n")
cat("    モンテカルロ・シミュレーション - DDM\n")
cat("=========================================\n")

# 1. 変動させるパラメータ（betaとg）の平均と標準偏差を設定
beta_mean <- beta; beta_sd <- 0.10
g_mean <- g;    g_sd <- 0.005

# 2. シミュレーションの実行
simulation_results <- numeric(n_sims)
for (i in 1:n_sims) {
  # betaとgの乱数を生成
  sim_beta <- rnorm(1, mean = beta_mean, sd = beta_sd)
  sim_g    <- rnorm(1, mean = g_mean,    sd = g_sd)
  
  # sim_betaに基づき、ループ内でrEを再計算
  sim_rE <- calculate_rE(rF, sim_beta, file_path_topix, verbose = FALSE)
  
  # rE <= g となる場合は計算不能
  if (sim_rE <= sim_g) {
    simulation_results[i] <- NA
    next
  }
  
  # 配当性向は固定値（future_payout_ratio）を使用
  simulation_results[i] <- calculate_ddm_value(net_income_base, sim_rE, future_payout_ratio, sim_g, verbose = FALSE)
}

# 3. シミュレーション結果の分析
valid_results <- na.omit(simulation_results) # 単位は百万円
cat(sprintf("計算が成功したシミュレーションの数: %d / %d\n\n", length(valid_results), n_sims))

# 結果を億円に変換
valid_values_oku <- valid_results / 100

cat("--- 株式価値の要約統計量 (単位: 億円) ---\n")
print(summary(valid_values_oku))

confidence_interval <- quantile(valid_values_oku, probs = c(0.025, 0.975))
cat("\n--- 95%信頼区間 ---\n")
cat(sprintf("株式価値は95%%の確率で %.2f 億円 から %.2f 億円 の範囲に収まる\n\n",
            confidence_interval[1], confidence_interval[2]))

# 4. 結果の可視化
hist(valid_values_oku, breaks = 50,
     main = "株式価値のモンテカルロ・シミュレーション結果 (DDM)",
     xlab = "株式価値（億円）", ylab = "頻度", col = "skyblue", border = "white")

abline(v = confidence_interval[1], col = "blue", lty = 2, lwd = 2)
abline(v = confidence_interval[2], col = "blue", lty = 2, lwd = 2)
abline(v = median(valid_values_oku), col = "red", lwd = 2)
legend("topright",
       legend = c("中央値", "95%信頼区間"),
       col = c("red", "blue"),
       lwd = 2, lty = c(1, 2), cex = 0.8)