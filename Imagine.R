# 必要なライブラリ
library(ggplot2)
library(scales)

# --- 1. シミュレーションの基本設定 ---
set.seed(123) # 乱数を固定（軌跡の形を決定）

n_years <- 7
n_days <- n_years * 252
days_per_month <- 252 / 12

initial_investment <- 100000
monthly_investment <- 10000

# 結果を格納するベクトル
prices_trust_raw <- numeric(n_days) # (A) 補正前のシミュレーション結果
invested_capital <- numeric(n_days) # 投資元本

prices_trust_raw[1] <- initial_investment
invested_capital[1] <- initial_investment

# --- 2. (A) 補正前のシミュレーション実行 ---
annual_drift <- 0.17
daily_drift <- annual_drift / 252
changes <- c(-0.30, -0.10, -0.05, -0.01, 0.01, 0.05, 0.10, 0.30)
weights <- c(  1  ,   3  ,  10  ,  50  ,  50  ,  10  ,   3  ,   1  )

for (i in 2:n_days) {
  # (1) 価格変動
  sampled_change <- sample(changes, 1, prob = weights)
  daily_return <- daily_drift + sampled_change
  current_value <- prices_trust_raw[i-1] * (1 + daily_return)
  
  # (2) 積立
  if (i %% round(days_per_month) == 0) {
    current_value <- current_value + monthly_investment 
    invested_capital[i] <- invested_capital[i-1] + monthly_investment
  } else {
    invested_capital[i] <- invested_capital[i-1]
  }
  
  prices_trust_raw[i] <- max(1, current_value) 
}

# --- 3. (A) 最終着地点を「元本の3倍」に補正 ---
final_capital <- tail(invested_capital, 1) # 940,000円
target_value <- final_capital * 3 # 2,820,000円
raw_final_value <- tail(prices_trust_raw, 1) 
profit_raw_timeseries <- prices_trust_raw - invested_capital
target_final_profit <- target_value - final_capital 
raw_final_profit <- raw_final_value - final_capital 
correction_ratio <- target_final_profit / raw_final_profit 
profit_scaled_timeseries <- profit_raw_timeseries * correction_ratio
prices_trust_final <- invested_capital + profit_scaled_timeseries
prices_trust_final <- pmax(prices_trust_final, invested_capital) 

# --- 4. (C) ミックス（レンジ）の計算 (補正後ライン基準) ---
years_axis <- (1:n_days) / 252 
max_vol_amount <- 500000 # 7年で±50万の幅
volatility_coefficient <- max_vol_amount / sqrt(n_years)
volatility_mix <- volatility_coefficient * sqrt(years_axis)

y_mix_upper <- prices_trust_final + volatility_mix
y_mix_lower <- prices_trust_final - volatility_mix
y_mix_lower <- pmax(y_mix_lower, 1)

# --- 5. グラフ描画用のデータ準備 ---
df <- data.frame(
  Year = years_axis,
  Trust_Price = prices_trust_final, 
  Capital = invested_capital,
  Mix_Lower = y_mix_lower,
  Mix_Upper = y_mix_upper
)

# --- 6. ggplot2 によるグラフ描画 (★ ラベル名変更) ---

# ご指定のラベル名を定義
label_mix <- "効率と学び (投資信託と個別株)"
label_trust <- "効率重視 (投資信託)"
label_capital <- "投資元本 (積立総額)"

p <- ggplot(df, aes(x = Year)) +
  
  # C: ミックス（レンジ）
  geom_ribbon(
    aes(ymin = Mix_Lower, ymax = Mix_Upper, fill = label_mix),
    alpha = 0.25
  ) +
  
  # A: 投資信託（基準）
  geom_line(
    aes(y = Trust_Price, color = label_trust),
    linewidth = 0.7
  ) +
  
  # 投資元本
  geom_line(
    aes(y = Capital, color = label_capital),
    linetype = "dotted", 
    linewidth = 1
  ) +
  
  # ラベルとタイトル (★ 変更)
  labs(
    title = "投資シミュレーション",
    subtitle = "初期投資10万円＋月1万円積立 投資信託(7年で3倍になるように設定)",
    x = "経過年",
    y = "資産評価額"
  ) +
  scale_x_continuous(breaks = 0:n_years, labels = paste0(0:n_years, "年")) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-4, suffix = "万円")) +
  
  # 色と凡例 (★ ラベル名変更)
  scale_fill_manual(values = c(setNames("green", label_mix))) +
  scale_color_manual(values = c(
    setNames("blue", label_trust),
    setNames("grey50", label_capital)
  )) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(), # 凡例のタイトルはなし
    plot.title = element_text(hjust = 0.5, size=16),
    plot.subtitle = element_text(hjust = 0.5),
    text = element_text(family = "Yu Gothic") # フォント指定
  )

# グラフを表示
print(p)

# --- 7. グラフをファイルに保存 ---
file_name <- "investment_simulation_final_labeled.png"

ggsave(
  filename = file_name,
  plot = p,
  width = 11,
  height = 6.5,
  dpi = 300
)

print(paste("グラフを", file_name, "として保存しました。"))