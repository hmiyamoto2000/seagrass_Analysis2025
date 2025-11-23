dir.create("./Result_AA_RF_XG", showWarnings = TRUE, recursive = FALSE, mode = "0777")

library(lightgbm)
library(iml)
library(data.table)
library(ggplot2)

data=read.csv("seagrass.csv")

dt <- as.data.table(data)
dt[, Species := as.integer(as.factor(Species)) - 1]
X <- dt[, -"Species", with = FALSE]
y <- dt$Species
dtrain <- lgb.Dataset(data = as.matrix(X), label = y)

# ---- LightGBM モデル訓練複数の場合 ----
params <- list(
  objective = "multiclass",
  num_class = 2,  #ここが複数になる
  metric = "multi_logloss"
)

# ---- LightGBM モデル訓練　二群の場合 ----
params <- list(
  objective = "binary",
  metric = "binary_logloss",
  learning_rate = 0.1,
  num_leaves = 15
)

model <- lgb.train(params = params, data = dtrain, nrounds = 100)

# ---- SHAP用 Predictor（クラス0の予測確率を返す） ----
predict_class0 <- function(model, newdata) {
  preds <- predict(model, as.matrix(newdata))
  matrix(preds, ncol = 3, byrow = TRUE)[, 1]
}
predictor_class0 <- Predictor$new(
  model = model,
  data = X,
  y = y,
  predict.function = predict_class0,
  type = "response"
)

# ---- 複数点に対して SHAP 値を取得し平均絶対値を計算 ---
dim(X) #132 21

shap_list <- lapply(1:132, function(i) {
  shap_i <- Shapley$new(predictor_class0, x.interest = X[i, ])
  shap_i$results$observation <- i
  shap_i$results
})
shap_dt <- rbindlist(shap_list)

# 特徴量ごとの平均 |SHAP| 値を計算
shap_summary <- shap_dt[, .(SHAP = mean(abs(phi))), by = feature]
setnames(shap_summary, "feature", "Feature")

# ---- LightGBMの Gain による重要度取得 ----
imp <- lgb.importance(model, percentage = TRUE)
imp <- imp[, .(Feature, Gain)]

# ---- SHAP と Gain をマージ ----
merged <- merge(imp, shap_summary, by = "Feature")

# ---- ggplotで可視化 ----SHAPが点で表される場合
ggplot(merged, aes(x = reorder(Feature, Gain))) +
  geom_bar(aes(y = Gain, fill = "Gain"), stat = "identity", position = "dodge") +
  geom_point(aes(y = SHAP, color = "SHAP"), size = 3) +
  coord_flip() +
  labs(title = "Feature Importance: Gain vs SHAP",
       x = "Feature", y = "Importance",
       fill = "Type", color = "Type") +
  scale_fill_manual(values = c("Gain" = "skyblue")) +
  scale_color_manual(values = c("SHAP" = "darkorange")) +
  theme_minimal()


#ImpとSHAPの双方の棒グラフを表現する場合
library(lightgbm)
library(iml)
library(data.table)
library(ggplot2)

#上記のコマンドを実施した前提
# ---- SHAP要約：平均絶対値を特徴量ごとに ----
shap_summary <- shap_dt[, .(Importance = mean(abs(phi))), by = feature]
shap_summary[, Type := "SHAP"]
setnames(shap_summary, "feature", "Feature")

# ---- Gainベース重要度取得 ----
gain_summary <- lgb.importance(model, percentage = TRUE)[, .(Feature, Gain)]
gain_summary[, Importance := Gain]
gain_summary[, Type := "Gain"]
gain_summary <- gain_summary[, .(Feature, Importance, Type)]

# ---- 両方結合（long format） ----
importance_all <- rbind(gain_summary, shap_summary)

# ---- ggplot棒グラフ（並列棒グラフ） ----
ggplot(importance_all, aes(x = reorder(Feature, Importance), y = Importance, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  coord_flip() +
  labs(title = "Feature Importance: Gain vs SHAP",
       x = "Feature", y = "Importance") +
  scale_fill_manual(values = c("Gain" = "skyblue", "SHAP" = "darkorange")) +
  theme_minimal()


#X軸と横軸をクリアにする
ggplot(importance_all, aes(x = reorder(Feature, Importance), y = Importance, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  coord_flip() +
  scale_x_discrete(expand = c(0,0)) +             # X軸（特徴量名軸）の余白をなくす
  scale_y_continuous(expand = c(0,0)) +           # Y軸（Importance軸）の余白をなくす
  labs(title = "Feature Importance: Gain vs SHAP (iml)",
       x = "Feature", y = "Importance") +
  scale_fill_manual(values = c("Gain" = "skyblue", "SHAP" = "darkorange")) +
  theme_minimal() +
  theme(
    axis.line.x = element_line(color = "black", size = 0.8),  # 横軸線
    axis.line.y = element_line(color = "black", size = 0.8),  # 縦軸線
    axis.ticks = element_line(color = "black", size = 0.8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )



# --- 出力ディレクトリ確認 ---
dir.create("./Result_AA_RF_XG", showWarnings = FALSE, recursive = TRUE)

# --- SHAP平均絶対値のCSV出力 ---
fwrite(shap_summary[, .(Feature, SHAP = Importance)], "./Result_AA_RF_XG/SHAP_importance_mean_abs.csv")

# --- Gain重要度のCSV出力 ---
fwrite(gain_summary[, .(Feature, Gain = Importance)], "./Result_AA_RF_XG/LightGBM_importance_gain.csv")

# --- SHAPとGainの統合データ（ワイド形式）を出力 ---
fwrite(merged, "./Result_AA_RF_XG/FeatureImportance_Gain_vs_SHAP.csv")

# --- SHAPとGainのロング形式（棒グラフ用）を出力 ---
fwrite(importance_all, "./Result_AA_RF_XG/FeatureImportance_LongFormat.csv")



# --- LightGBM: Gain の単独グラフ ---
ggplot(gain_summary, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "LightGBM Feature Importance (Gain)",
       x = "Feature", y = "Gain") +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )
ggsave("./Result_AA_RF_XG/LightGBM_Gain_only.png", width = 9, height = 7, dpi = 300)


# --- LightGBM: SHAP の単独グラフ ---
ggplot(shap_summary, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  coord_flip() +
  labs(title = "LightGBM Feature Importance (SHAP)",
       x = "Feature", y = "Mean Absolute SHAP") +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )
ggsave("./Result_AA_RF_XG/LightGBM_SHAP_only.png", width = 9, height = 7, dpi = 300)



#PDF Gain
pdf("./Result_AA_RF_XG/LightGBM_Gain_only.pdf", width = 9, height = 7)
ggplot(gain_summary, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "LightGBM Feature Importance (Gain)",
       x = "Feature", y = "Gain") +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )
dev.off()

#PDF SHAP

pdf("./Result_AA_RF_XG/LightGBM_SHAP_only.pdf", width = 9, height = 7)
ggplot(shap_summary, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  coord_flip() +
  labs(title = "LightGBM Feature Importance (SHAP)",
       x = "Feature", y = "Mean Absolute SHAP") +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )
dev.off()

#Gain vs SHAP
pdf("./Result_AA_RF_XG/LightGBM_Gain_vs_SHAP.pdf", width = 10, height = 8)
ggplot(importance_all, aes(x = reorder(Feature, Importance), y = Importance, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  coord_flip() +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(title = "Feature Importance: Gain vs SHAP (iml)",
       x = "Feature", y = "Importance") +
  scale_fill_manual(values = c("Gain" = "skyblue", "SHAP" = "darkorange")) +
  theme_minimal() +
  theme(
    axis.line.x = element_line(color = "black", size = 0.8),
    axis.line.y = element_line(color = "black", size = 0.8),
    axis.ticks = element_line(color = "black", size = 0.8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
dev.off()
