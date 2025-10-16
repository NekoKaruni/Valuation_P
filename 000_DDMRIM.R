# ==============================================================================
# 企業価値評価（DDM & RIM）統合版
# Part 3: 感応度分析
# Part 4: モンテカルロ・シミュレーション
# ==============================================================================

# 0. 必要なライブラリの読み込み
# ------------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(conflicted)
})
conflict_prefer("filter", "dplyr")
conflict_prefer("lag",    "dplyr")

# ==============================================================================
# Part 1: パラメータ設定 (Assumptions & Parameters)
# ==============================================================================

# --- 企業データ関連 ---
file_path_financials <- "ProcessedBankData/7150_processed_data.csv"

# --- 基本パラメータ ---
future_payout_ratio <- 0.30

# --- Part 3: 感応度分析用のシナリオ ---
re_scenarios <- c(0.04, 0.05, 0.06)
g_scenarios  <- c(0.01, 0.02)

# --- Part 4: モンテカルロ・シミュレーション用の設定 ---
n_sims <- 10000
set.seed(123)

# rEとgを確率分布として設定（標準偏差を大きくして分布を分かりやすくする）
re_mean <- 0.05;  re_sd <- 0.02
g_mean  <- 0.015; g_sd  <- 0.01


# ==============================================================================
# Part 2: 関数定義 (Function Definitions)
# ==============================================================================
load_and_prepare_data <- function(financials_path) {
  df <- read_csv(financials_path, show_col_types = FALSE)
  net_income_df <- df %>%
    filter(項目 %in% c("当期純利益", "当期純利益又は当期純損失（△）")) %>%
    slice(which(row_number() %% 2 != 0)) %>%
    select(No, `金額（百万円）`)
  net_assets_df <- df %>%
    filter(項目 %in% c("株主資本合計", "その他の包括利益累計額合計")) %>%
    group_by(No) %>%
    summarise(自己資本 = sum(`金額（百万円）`), .groups = 'drop')
  C0 <- tail(net_assets_df$自己資本, 1)
  return(list(net_income_df = net_income_df, C0 = C0))
}

calculate_ddm_value <- function(net_income_df, rE, payout_ratio, g) {
  if (rE <= g) return(NA)
  future_dividends <- net_income_df$`金額（百万円）` * payout_ratio
  pv_dividends <- sum(future_dividends / (1 + rE)^(1:nrow(net_income_df)))
  last_dividend <- tail(future_dividends, 1)
  terminal_value <- last_dividend * (1 + g) / (rE - g)
  pv_terminal_value <- terminal_value / (1 + rE)^nrow(net_income_df)
  return(pv_dividends + pv_terminal_value)
}

calculate_rim_value <- function(C0, net_income_df, rE, payout_ratio, g) {
  if (rE <= g) return(NA)
  n <- nrow(net_income_df)
  C <- c(C0, rep(NA, n)); pv_residual_income <- rep(NA, n)
  for (t in 1:n) {
    A_t <- net_income_df$`金額（百万円）`[t]
    residual_income_t <- A_t - rE * C[t]
    pv_residual_income[t] <- residual_income_t / (1 + rE)^t
    if (t <= n) C[t+1] <- C[t] + A_t * (1 - payout_ratio)
  }
  last_net_income <- tail(net_income_df$`金額（百万円）`, 1)
  last_book_value <- C[n+1]
  last_residual_income <- last_net_income - rE * last_book_value
  terminal_value <- last_residual_income * (1 + g) / (rE - g)
  pv_terminal_value <- terminal_value / (1 + rE)^n
  return(C0 + sum(pv_residual_income, na.rm = TRUE) + pv_terminal_value)
}


# ==============================================================================
# Part 3: 感応度分析の実行
# ==============================================================================
# (省略... Part4のインプットには影響しないため、ここでは省略します)


# ==============================================================================
# Part 4: モンテカルロ・シミュレーションの実行と可視化
# ==============================================================================
# 1. データの準備
prepared_data <- load_and_prepare_data(file_path_financials)
net_income_base <- prepared_data$net_income_df
C0_base <- prepared_data$C0

# 2. シミュレーションの実行
ddm_sim_results <- numeric(n_sims)
rim_sim_results <- numeric(n_sims)
for (i in 1:n_sims) {
  sim_rE <- rnorm(1, mean = re_mean, sd = re_sd)
  sim_g  <- rnorm(1, mean = g_mean,  sd = g_sd)
  ddm_sim_results[i] <- calculate_ddm_value(net_income_base, sim_rE, future_payout_ratio, sim_g)
  rim_sim_results[i] <- calculate_rim_value(C0_base, net_income_base, sim_rE, future_payout_ratio, sim_g)
}

# 3. シミュレーション結果の整形
ddm_valid_oku <- na.omit(ddm_sim_results) / 100
rim_valid_oku <- na.omit(rim_sim_results) / 100
plot_data <- tibble(
  value = c(ddm_valid_oku, rim_valid_oku),
  model = c(rep("DDM", length(ddm_valid_oku)), rep("RIM", length(rim_valid_oku)))
)

# --- ★★★ ここからが新しいグラフのコードです ★★★ ---
cat("\n\n========================================================\n")
cat("          Part 4: モンテカルロ・シミュレーション結果\n")
cat("========================================================\n")

cat("--- DDM シミュレーション結果 (単位: 億円) ---\n")
# print(summary(ddm_valid_oku)) # summaryに関しても表示されるのでコメントアウトしても良い
cat(sprintf("中央値: %.2f 億円\n", median(ddm_valid_oku))) # ← 中央値の出力を追加
cat(sprintf("95%%信頼区間: %.2f 億円 〜 %.2f 億円\n", quantile(ddm_valid_oku, 0.025), quantile(ddm_valid_oku, 0.975)))

cat("\n--- RIM シミュレーション結果 (単位: 億円) ---\n")
# print(summary(rim_valid_oku))
cat(sprintf("中央値: %.2f 億円\n", median(rim_valid_oku))) # ← 中央値の出力を追加
cat(sprintf("95%%信頼区間: %.2f 億円 〜 %.2f 億円\n", quantile(rim_valid_oku, 0.025), quantile(rim_valid_oku, 0.975)))
# 4. 結果の可視化 (重ね合わせた密度プロット)
# グラフの表示範囲を、データの1%点から99%点に設定して外れ値の影響を抑える
x_limits <- quantile(plot_data$value, probs = c(0.01, 0.99), na.rm = TRUE)

# グラフ描画
ggplot(plot_data, aes(x = value, fill = model)) +
  # 密度プロットを描画
  geom_density(alpha = 0.6) +
  # 中央値を示す破線を追加
  geom_vline(aes(xintercept = median(ddm_valid_oku), color = "DDM"), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(rim_valid_oku), color = "RIM"), linetype = "dashed", size = 1) +
  # 表示範囲を調整
  coord_cartesian(xlim = x_limits) +
  labs(
    title = "DDM vs RIM 評価額の確率密度",
    subtitle = "破線は各モデルの中央値を示す",
    x = "株式価値（億円）",
    y = "密度"
  ) +
  theme_minimal(base_size = 14) +
  # 凡例（はんれい）の調整
  scale_fill_manual(name = "モデル", values = c("DDM" = "skyblue", "RIM" = "salmon")) +
  scale_color_manual(name = "中央値", values = c("DDM" = "blue", "RIM" = "red"))



----------------------------------------------------------------------------------------
  
# ==============================================================================
# 市場期待の逆算分析 Rスクリプト
# ==============================================================================

# 0. 必要なライブラリの読み込み
# ------------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(conflicted)
})
conflict_prefer("filter", "dplyr")
conflict_prefer("lag",    "dplyr")

# ==============================================================================
# Part 1: パラメータ設定 (Assumptions & Parameters)
# ==============================================================================

# --- 企業データ関連 ---
file_path_financials <- "ProcessedBankData/7150_processed_data.csv"

# --- 基本パラメータ ---
future_payout_ratio <- 0.30

# --- 時価総額の定義 ---
N <- 8416000      # 発行済株式総数
P <- 450          # 現在の株価（円）
market_cap_yen <- N * P
# 分析で使う目標時価総額（百万円単位）
market_cap_target <- market_cap_yen / 1000000

# --- ★★★ 逆算分析でテストする株主資本コスト(rE)の水準 ★★★ ---
re_to_test <- c(0.04, 0.05, 0.06, 0.07)


# ==============================================================================
# Part 2: 関数定義 (Function Definitions)
# ==============================================================================
# データの読み込みと準備
load_and_prepare_data <- function(financials_path) {
  df <- read_csv(financials_path, show_col_types = FALSE)
  net_income_df <- df %>%
    filter(項目 %in% c("当期純利益", "当期純利益又は当期純損失（△）")) %>%
    slice(which(row_number() %% 2 != 0)) %>%
    select(No, `金額（百万円）`)
  net_assets_df <- df %>%
    filter(項目 %in% c("株主資本合計", "その他の包括利益累計額合計")) %>%
    group_by(No) %>%
    summarise(自己資本 = sum(`金額（百万円）`), .groups = 'drop')
  C0 <- tail(net_assets_df$自己資本, 1)
  return(list(net_income_df = net_income_df, C0 = C0))
}

# DDMによる株式価値の算出
calculate_ddm_value <- function(net_income_df, rE, payout_ratio, g) {
  if (rE <= g) return(NA)
  future_dividends <- net_income_df$`金額（百万円）` * payout_ratio
  pv_dividends <- sum(future_dividends / (1 + rE)^(1:nrow(net_income_df)))
  last_dividend <- tail(future_dividends, 1)
  terminal_value <- last_dividend * (1 + g) / (rE - g)
  pv_terminal_value <- terminal_value / (1 + rE)^nrow(net_income_df)
  return(pv_dividends + pv_terminal_value)
}

# RIMによる株式価値の算出
calculate_rim_value <- function(C0, net_income_df, rE, payout_ratio, g) {
  if (rE <= g) return(NA)
  n <- nrow(net_income_df)
  C <- c(C0, rep(NA, n)); pv_residual_income <- rep(NA, n)
  for (t in 1:n) {
    A_t <- net_income_df$`金額（百万円）`[t]
    residual_income_t <- A_t - rE * C[t]
    pv_residual_income[t] <- residual_income_t / (1 + rE)^t
    if (t <= n) C[t+1] <- C[t] + A_t * (1 - payout_ratio)
  }
  last_net_income <- tail(net_income_df$`金額（百万円）`, 1)
  last_book_value <- C[n+1]
  last_residual_income <- last_net_income - rE * last_book_value
  terminal_value <- last_residual_income * (1 + g) / (rE - g)
  pv_terminal_value <- terminal_value / (1 + rE)^n
  return(C0 + sum(pv_residual_income, na.rm = TRUE) + pv_terminal_value)
}


# ==============================================================================
# Part 3: 市場期待の逆算分析
# ==============================================================================
cat("========================================================\n")
cat("          市場期待の逆算分析\n")
cat("========================================================\n")
cat(sprintf("目標とする時価総額: %.2f 億円\n", market_cap_target / 100))

# 1. データの準備
prepared_data <- load_and_prepare_data(file_path_financials)
net_income_base <- prepared_data$net_income_df
C0_base <- prepared_data$C0

# 2. 結果を格納するデータフレームを準備
implied_g_df <- tibble(
  `株主資本コスト(rE)` = re_to_test,
  `DDMの市場期待成長率(g)` = NA_real_,
  `RIMの市場期待成長率(g)` = NA_real_
)

# 3. 各rEの水準で、時価総額と一致するgを計算
for (i in 1:nrow(implied_g_df)) {
  re_val <- implied_g_df$`株主資本コスト(rE)`[i]
  
  # DDMのgを逆算する
  # (計算値 - 目標値) = 0 となるgを探す
  ddm_objective_func <- function(g) {
    calculate_ddm_value(net_income_base, re_val, future_payout_ratio, g) - market_cap_target
  }
  # uniroot関数で方程式の解（=g）を探す
  ddm_solution <- try(uniroot(ddm_objective_func, interval = c(-0.02, re_val - 0.001))$root, silent = TRUE)
  if (!inherits(ddm_solution, "try-error")) implied_g_df$`DDMの市場期待成長率(g)`[i] <- ddm_solution
  
  # RIMのgを逆算する
  rim_objective_func <- function(g) {
    calculate_rim_value(C0_base, net_income_base, re_val, future_payout_ratio, g) - market_cap_target
  }
  rim_solution <- try(uniroot(rim_objective_func, interval = c(-0.02, re_val - 0.001))$root, silent = TRUE)
  if (!inherits(rim_solution, "try-error")) implied_g_df$`RIMの市場期待成長率(g)`[i] <- rim_solution
}

# 4. 結果を表形式で表示
cat("\n--- 市場が織り込む期待成長率（Implied Growth Rate）---\n")
print(implied_g_df %>% mutate(across(everything(), ~ scales::percent(., accuracy = 0.01))))