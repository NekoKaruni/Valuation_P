# ==============================================================================
# 企業価値評価（残余利益モデル）およびモンテカルロ・シミュレーション用 Rスクリプト
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
beta <- 1.08    # ベータ値

# --- RIM評価および継続価値 (TV) の算出用パラメータ ---
payout_ratio <- 0.30  # 配当性向（将来の自己資本の計算に使用）
g <- 0.02             # 残余利益の永久成長率

# --- モンテカルロ・シミュレーションの設定 ---
n_sims <- 10000  # シミュレーション回数
set.seed(123)    # 乱数の固定化（結果の再現性のため）

# --- 時価総額の算出用（結果比較のため） ---
N <- 8416000      # 発行済株式総数
P <- 450          # 現在の株価（円）※分析時点の株価に設定してください
E_value <- N * P  # 時価総額（円）

# ==============================================================================
# Part 2: 関数定義 (Function Definitions)
# ==============================================================================

#' データの読み込みと前処理
load_and_prepare_data <- function(financials_path) {
  df <- read_csv(financials_path, show_col_types = FALSE)
  
  # 当期純利益の抽出
  net_income_df <- df %>%
    filter(項目 %in% c("当期純利益", "当期純利益又は当期純損失（△）")) %>%
    # 奇数行のみを抽出（有報と短信の重複を避けるためと仮定）
    slice(which(row_number() %% 2 != 0)) %>%
    select(No, `金額（百万円）`)
  
  # 自己資本簿価の抽出
  net_assets_df <- df %>%
    filter(項目 %in% c("株主資本合計", "その他の包括利益累計額合計")) %>%
    group_by(No) %>%
    summarise(自己資本 = sum(`金額（百万円）`), .groups = 'drop')
  
  # C0 (最も直近の自己資本簿価) を定義
  C0 <- tail(net_assets_df$自己資本, 1)
  
  return(list(net_income_df = net_income_df, net_assets_df = net_assets_df, C0 = C0))
}


#' 株主資本コスト (rE) の計算
calculate_rE <- function(rF, beta, topix_path, verbose = TRUE) {
  topix <- read_csv(topix_path, show_col_types = FALSE)
  
  # TOPIXのリターンを計算（データに合わせて範囲を調整）
  topix_open <- topix$Close[1:(nrow(topix) - 1)]
  topix_close <- topix$Close[2:nrow(topix)]
  topix_return <- (topix_close - topix_open) / topix_open
  
  Er <- mean(topix_return, na.rm = TRUE)
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


#' RIMによる株式価値の算出
calculate_rim_value <- function(C0, net_income_df, rE, payout_ratio, g, verbose = TRUE) {
  n <- nrow(net_income_df)
  
  C <- c(C0, rep(NA, n))
  pv_residual_income <- rep(NA, n)
  
  # ループで各期の残余利益とPVを計算
  # 元のスクリプトのロジックを再現：過去の純利益データを将来の予測値と見なして計算
  for (t in 1:n) {
    A_t <- net_income_df$`金額（百万円）`[t]
    residual_income_t <- A_t - rE * C[t]
    pv_residual_income[t] <- residual_income_t / (1 + rE)^t
    
    # クリーン・サープラス関係に基づき次期の自己資本を更新
    if (t <= n) {
      C[t+1] <- C[t] + A_t * (1 - payout_ratio)
    }
  }
  
  # 継続価値（Terminal Value）の計算
  last_net_income <- tail(net_income_df$`金額（百万円）`, 1)
  last_book_value <- C[n+1]
  
  last_residual_income <- last_net_income - rE * last_book_value
  terminal_value <- last_residual_income * (1 + g) / (rE - g)
  pv_terminal_value <- terminal_value / (1 + rE)^n
  
  # 株式価値の計算
  equity_value <- C0 + sum(pv_residual_income, na.rm = TRUE) + pv_terminal_value
  
  if (verbose) {
    cat("--- RIMによる企業価値評価結果 ---\n")
    cat(sprintf("株式価値: %s 百万円\n", format(round(equity_value, 2), big.mark = ",")))
    cat(sprintf("  - C0（現在の自己資本簿価）: %s 百万円\n", format(round(C0, 2), big.mark = ",")))
    cat(sprintf("  - 将来の残余利益の現在価値合計: %s 百万円\n", format(round(sum(pv_residual_income, na.rm = TRUE), 2), big.mark = ",")))
    cat(sprintf("  - 継続価値の現在価値: %s 百万円\n\n", format(round(pv_terminal_value, 2), big.mark = ",")))
  }
  
  return(equity_value)
}


# ==============================================================================
# Part 3: 基本分析の実行 (Base Case Execution)
# ==============================================================================
cat("=========================================\n")
cat("       基本分析 (Base Case) - RIM\n")
cat("=========================================\n")

# 1. データの読み込みと準備
prepared_data <- load_and_prepare_data(file_path_financials)
C0_base <- prepared_data$C0
net_income_base <- prepared_data$net_income_df

# 2. 株主資本コスト (rE) の計算
rE_base <- calculate_rE(rF, beta, file_path_topix)

# 3. RIMによる株式価値の算出
equity_value_base <- calculate_rim_value(C0_base, net_income_base, rE_base, payout_ratio, g)


# ==============================================================================
# Part 4: モンテカルロ・シミュレーション (Monte Carlo Simulation)
# ==============================================================================
cat("=========================================\n")
cat("    モンテカルロ・シミュレーション - RIM\n")
cat("=========================================\n")

# 1. 変動させるパラメータの平均と標準偏差を設定
beta_mean <- beta; beta_sd <- 0.10
g_mean <- g;     g_sd <- 0.005

# 2. シミュレーションの実行
simulation_results <- numeric(n_sims)
for (i in 1:n_sims) {
  sim_beta <- rnorm(1, mean = beta_mean, sd = beta_sd)
  sim_g    <- rnorm(1, mean = g_mean,    sd = g_sd)
  
  sim_rE <- calculate_rE(rF, sim_beta, file_path_topix, verbose = FALSE)
  
  # rE <= g となると継続価値が計算できないため、その試行はNAとする
  if (sim_rE <= sim_g) {
    simulation_results[i] <- NA
    next
  }
  
  simulation_results[i] <- calculate_rim_value(C0_base, net_income_base, sim_rE, payout_ratio, sim_g, verbose = FALSE)
}

# 3. シミュレーション結果の分析
# ------------------------------------------------------------------------------
valid_results <- na.omit(simulation_results)
cat(sprintf("計算が成功したシミュレーションの数: %d / %d\n\n", length(valid_results), n_sims))

cat("--- 要約統計量 (単位: 百万円) ---\n")
print(summary(valid_results))

# 95%信頼区間の計算
confidence_interval <- quantile(valid_results, probs = c(0.025, 0.975))

# 結果の詳細出力
cat("\n--- 評価結果サマリー ---\n")
# 結果を億円単位に変換
C0_oku <- C0_base / 100
median_oku <- median(valid_results) / 100
lower_oku <- confidence_interval[1] / 100
upper_oku <- confidence_interval[2] / 100
market_cap_oku <- E_value / 100000000

cat(sprintf("現在の自己資本 (C0): %.2f 億円\n", C0_oku))
cat(sprintf("現在の時価総額: %.2f 億円\n\n", market_cap_oku))
cat(sprintf("RIM評価額（中央値）: %.2f 億円\n", median_oku))
cat(sprintf("RIM評価額（95%%信頼区間）: %.2f 億円 〜 %.2f 億円\n", lower_oku, upper_oku))

# 4. 結果の可視化
# ( ...この後の可視化コードは変更ありません... )
# 4. 結果の可視化
hist(valid_results, breaks = 50,
     main = "株式価値のモンテカルロ・シミュレーション結果 (RIM)",
     xlab = "株式価値（百万円）", ylab = "頻度", col = "skyblue", border = "white")

abline(v = confidence_interval[1], col = "blue", lty = 2, lwd = 2)
abline(v = confidence_interval[2], col = "blue", lty = 2, lwd = 2)
abline(v = median(valid_results), col = "red", lwd = 2)
legend("topright",
       legend = c("中央値", "95%信頼区間"),
       col = c("red", "blue"),
       lwd = 2, lty = c(1, 2), cex = 0.8)