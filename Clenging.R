library(tidyverse)
library(readr)

# ① CSV ファイルが入っているフォルダのパス
folder <- "Q:\Machine"   # フォルダパスを指定（例: "./csv_data/"）

# ② フォルダ内の CSV ファイル一覧を取得
files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)

# ③ 1ファイルを処理する関数を定義
process_csv <- function(file) {
  df <- read_csv(file, locale = locale(encoding = "SHIFT_JIS")) %>%
    filter(!if_all(everything(), is.na))  # 全部NAの行削除
  
  # 列名を整理（年度表記を西暦に揃える）
  names(df) <- str_replace(names(df), "年度|年|/3|３月期", "")
  
  df_long <- df %>%
    pivot_longer(
      cols = matches("^[0-9]{4}"),   # 年度列を抽出
      names_to = "年度",
      values_to = "金額（百万円）"
    )
  
  df_final <- df_long %>%
    mutate(
      No = row_number(),
      ファイル = basename(file)      # どのCSVか分かるように追加（任意）
    ) %>%
    select(ファイル, No, 項目 = 1, `金額（百万円）`, 年度)
  
  return(df_final)
}

# ④ すべての CSV を処理して結合
all_data <- map_dfr(files, process_csv)

# ⑤ 結果を確認
print(all_data)

# ⑥ 必要なら CSV に保存
write_csv(all_data, "all_data_long.csv")
