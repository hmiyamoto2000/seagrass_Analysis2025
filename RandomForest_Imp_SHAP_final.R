# --- 初期設定とディレクトリ作成 ---
dir.create("./Result_AA_RF_XG", showWarnings = TRUE, recursive = TRUE, mode = 0777)

# --- ライブラリ読み込み ---
library(randomForest)
library(iml)
library(data.table)
library(ggplot2)
library(reshape2)

# --- データ読み込み ---
FCPC.train <- read.csv("seagrass.csv")
train.x <- FCPC.train[, 2:22]
train.y <- as.factor(FCPC.train[, 1])

# --- ランダムフォレストモデルチューニング & 学習 ---
set.seed(131)
model.rf <- tuneRF(train.x, train.y, doBest = TRUE)

# --- 学習モデルの保存 ---
sink('./Result_AA_RF_XG/randomforest_model_rf.txt')
print(model.rf)
sink()

# --- 予測と混同行列 (訓練データに対して) ---
pred_t <- predict(model.rf, train.x)
rf_pred_t <- table(train.y, pred_t)
write.csv(rf_pred_t, "./Result_AA_RF_XG/randomForest_pred1.csv")

# --- 重要度割合の計算と保存 ---
importance_ratio <- model.rf$importance / sum(model.rf$importance)
write.csv(importance_ratio, "./Result_AA_RF_XG/randomForest_raw.csv")

# --- モデル再学習（明示的にtrain.yとtrain.xを含むデータフレームで） ---
train_df <- data.frame(train.y = train.y, train.x)

set.seed(22)
model <- randomForest(train.y ~ ., data = train_df, mtry = 4, importance = TRUE, proximity = TRUE)

# --- 変数重要度プロットを画像保存 ---
png("./Result_AA_RF_XG/randomforest_tree_var.png", res = 125, width = 750, height = 750)
varImpPlot(model)
dev.off()

# --- Gini重要度をCSV出力 ---
write.csv(importance(model, type = 2), "./Result_AA_RF_XG/randomForest_pred_importance_Gini_model.csv")

# --- SHAP解析の準備 ---
predictor <- Predictor$new(model, data = train.x, y = train.y)

# --- SHAP値の計算（全サンプル） ---
X <- train.x
n_samples <- nrow(X)

shap_values_list <- lapply(1:n_samples, function(i) {
  shap <- Shapley$new(predictor, x.interest = X[i, ])
  shap$results$phi
})

# --- SHAP平均絶対値の計算 ---
shap_matrix <- do.call(rbind, shap_values_list)
mean_abs_shap <- colMeans(abs(shap_matrix))
shap_dt <- data.table(Feature = colnames(X), MeanAbsSHAP = mean_abs_shap)

# --- Gini重要度との比較 ---
imp <- importance(model, type = 2)
imp_dt <- data.table(Feature = rownames(imp), Importance = imp[, 1])

df <- merge(imp_dt, shap_dt, by = "Feature")
df_long <- melt(df, id.vars = "Feature", variable.name = "Method", value.name = "Importance")

# --- グラフ出力（SHAP vs Gini） ---
ggplot(df_long, aes(x = reorder(Feature, Importance), y = Importance, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  coord_flip() +
  labs(
    title = "Feature Importance: Random Forest (Gini) vs Mean Absolute SHAP",
    x = "Feature", y = "Importance"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black", size = 0.8),
    axis.ticks = element_line(color = "black", size = 0.8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# --- グラフ保存（オプション） ---
ggsave("./Result_AA_RF_XG/FeatureImportance_SHAP_vs_Gini.png", width = 10, height = 8)


#

# グラフ描画（SHAP vs Gini） クリアな軸 & 最小限の余白
ggplot(df_long, aes(x = reorder(Feature, Importance), y = Importance, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  coord_flip() +
  scale_x_discrete(expand = c(0, 0)) +             # 特徴量軸の余白をなくす
  scale_y_continuous(expand = c(0, 0)) +           # Importance軸の余白をなくす
  labs(
    title = "Feature Importance: Gini vs SHAP (iml)",
    x = "Feature",
    y = "Importance"
  ) +
  scale_fill_manual(values = c("Importance" = "skyblue", "MeanAbsSHAP" = "darkorange")) +
  theme_minimal() +
  theme(
    axis.line.x = element_line(color = "black", size = 0.8),  # 横軸線
    axis.line.y = element_line(color = "black", size = 0.8),  # 縦軸線
    axis.ticks = element_line(color = "black", size = 0.8),
    panel.grid.major = element_blank(),                      # グリッドを最小限に
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # タイトル中央・太字
    legend.position = "top",
    legend.title = element_blank()
  )

ggsave("./Result_AA_RF_XG/SHAP_vs_Gini_CleanAxes.png", width = 10, height = 8, dpi = 300)

#名前変更する場合
unique(df_long$Method)

# 1. カテゴリ名を統一（ここが重要）
df_long$Method <- factor(df_long$Method,
                         levels = c("Importance", "MeanAbsSHAP"),
                         labels = c("Gini", "SHAP"))

ggplot(df_long, aes(x = reorder(Feature, Importance), y = Importance, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  coord_flip() +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = "Feature Importance: Gini vs SHAP (iml)",
    x = "Feature",
    y = "Importance"
  ) +
  scale_fill_manual(values = c("Gini" = "skyblue", "SHAP" = "darkorange")) +
  theme_minimal() +
  theme(
    axis.line.x = element_line(color = "black", size = 0.8),
    axis.line.y = element_line(color = "black", size = 0.8),
    axis.ticks = element_line(color = "black", size = 0.8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.position = "top",
    legend.title = element_blank()
  )

ggsave("./Result_AA_RF_XG/SHAP_vs_Gini_CleanAxes.png", width = 10, height = 8, dpi = 300)

# --- SHAP値の平均絶対値をCSV出力 ---
write.csv(shap_dt, "./Result_AA_RF_XG/SHAP_importance_mean_abs.csv", row.names = FALSE)

# --- Gini重要度も改めて明示的にCSV出力（既にあるが再確認用） ---
write.csv(imp_dt, "./Result_AA_RF_XG/Gini_importance.csv", row.names = FALSE)

# --- Gini + SHAP を統合したデータ（df）もCSV出力 ---
write.csv(df, "./Result_AA_RF_XG/FeatureImportance_Gini_vs_SHAP.csv", row.names = FALSE)

# --- df_long（グラフ用のロング形式）もCSV出力（オプション） ---
write.csv(df_long, "./Result_AA_RF_XG/FeatureImportance_LongFormat.csv", row.names = FALSE)


# --- Gini のみの棒グラフ（横向き） ---
ggplot(imp_dt, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = "Feature Importance: Random Forest (Gini)",
    x = "Feature",
    y = "MeanDecreaseGini"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black", size = 0.8),
    axis.ticks = element_line(color = "black", size = 0.8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

ggsave("./Result_AA_RF_XG/Gini_only.png", width = 9, height = 7, dpi = 300)


# --- SHAP のみの棒グラフ（横向き） ---
ggplot(shap_dt, aes(x = reorder(Feature, MeanAbsSHAP), y = MeanAbsSHAP)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  coord_flip() +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = "Feature Importance: SHAP (Mean Absolute)",
    x = "Feature",
    y = "Mean Absolute SHAP"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black", size = 0.8),
    axis.ticks = element_line(color = "black", size = 0.8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

ggsave("./Result_AA_RF_XG/SHAP_only.png", width = 9, height = 7, dpi = 300)


#本研究では、Random Forest による Gini-based feature selection、XGBoost および LightGBM による Gain-based feature selection、そして SHAP による SHAP-based feature selection を適用し、重要な特徴量（因子）を抽出した。
#なお、SHAP-based feature selection については、Random Forest、XGBoost、LightGBM の各モデルを用いて SHAP 値をそれぞれ算出し、モデルごとの特徴量の寄与度を比較した。
#
#In this study, we applied Gini-based feature selection (Random Forest), Gain-based feature selection (XGBoost and LightGBM), and SHAP-based feature selection to identify key predictors.
#SHAP values were computed separately for models trained with Random Forest, XGBoost, and LightGBM to evaluate feature contributions across different algorithms.



