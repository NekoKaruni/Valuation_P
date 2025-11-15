library(tidyverse)
df <- read_csv("Valuation_results.csv")
df


#円に戻す
df$DCF_value <- df$DCF_value *1e6 
df$RIM_value <- df$RIM_value *1e6
df$DDM_value <- df$DDM_value *1e6

#億
df$DCF_value <- df$DCF_value /1e8 
df$RIM_value <- df$RIM_value /1e8
df$DDM_value <- df$DDM_value /1e8
df$NP_latest <- df$NP_latest /1e8



df_plot <- df[, c("df", "DCF_value", "RIM_value", "DDM_value", "NP_latest")]
df_plot$df <- as.character(df_plot$df)
# wide → long 形式に変換
df_long <- pivot_longer(df_plot, cols = c("DCF_value", "RIM_value", "DDM_value", "NP_latest"),
                        names_to = "モデル", values_to = "時価総額")

# モデル名を整形（末尾の "_value" を削除）
df_long$モデル <- sub("_value|_latest", "", df_long$モデル)




df_long$モデル <- factor(df_long$モデル, levels = c("DCF", "RIM", "DDM", "NP"))
# グラフ作成

#2. 各企業の最大時価総額モデルを判定
df_long <- df_long %>%
  group_by(df) %>%
  mutate(最大モデル = ifelse(時価総額 == max(時価総額), TRUE, FALSE)) %>%
  ungroup()

# 3. 企業ごとのモデル別時価総額グラフ
p1 <- ggplot(df_long, aes(x = df, y = 時価総額, fill = モデル)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "企業ごとのモデル別時価総額",
       x = "企業", y = "時価総額（億円）") +
  theme_minimal()

print(p1)

# 4. モデル別平均時価総額
model_summary <- df_long %>%
  group_by(モデル) %>%
  summarise(
    平均 = mean(時価総額),
    中央値 = median(時価総額),
    最大 = max(時価総額),
    最小 = min(時価総額),
    SD = sd(時価総額)
  )

print(model_summary)

# 5. モデル別分布（箱ひげ図）
p2 <- ggplot(df_long, aes(x = モデル, y = 時価総額, fill = モデル)) +
  geom_boxplot() +
  labs(title = "モデル別時価総額の分布",
       x = "モデル", y = "時価総額（億円）") +
  theme_minimal()

print(p2)

# p2 はモデル別分布の箱ひげ図
ggsave("モデル別時価総額分布.png", plot = p2,
       width = 6, height = 5, dpi = 600)



# 6. 優位モデルの企業数を集計
dominant_model <- df_long %>%
  filter(最大モデル) %>%
  group_by(モデル) %>%
  summarise(企業数 = n())

print(dominant_model)
