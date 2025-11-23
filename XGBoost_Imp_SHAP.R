dir.create("./Result_AA_RF_XG", showWarnings = TRUE, recursive = FALSE, mode = "0777")

options(na.action='na.pass') # for missing value
library(xgboost)
FCPC.train <- read.csv("seagrass.csv") # tibble::glimpse() 
FCPC.test <- read.csv("seagrass.csv") ## please use the filename "RF_XG_raw_data.csv", which the test group of pig dataset is summarized as Comp_ThB, to classify into 8 groups
dim(FCPC.train)
train.x <- FCPC.train[, 2:22] #dim(FCPC.train)[1] 36 24
x <- rbind(train.x,FCPC.test[,-1]) 

y_0 <- c(FCPC.train$Species)
y_1 <- as.factor(y_0)
y <- as.integer(y_1)-1 #caution the order of list as 2,3,0,1,6,7,4,5 
#2,CW_control(Chicken_Con);3,CW_test(Chicken_Comp);0,FE_control(Fish_Con);1,FE_test(Fish_Comp);6,PK_control(Pig_Con);7,PK_test(Pig_Comp_ThB);4,KC_control(Cattel_Con);5,KC_test(Cattle_ThB)

x <- as.matrix(x)
trind <- 1:length(y) 
teind <- (nrow(train.x)+1):nrow(x) 
set.seed(131) #fixed seed

#多クラス分類の場合
param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss", 
              "num_class" = 2 # class (Fish_Con; Fish_Comp; Chicken_Con; Chicken_Comp; Pig_Con; Pig_Comp_ThB; Cattle_Before; Cattle_After)
)

#２値化の場合
param <- list(
  objective = "binary:logistic",
  eval_metric = "logloss"
)


k<-round(1+log2(nrow(train.x)))
cv.nround <- 100 #search
bst.cv <- xgb.cv(param=param, data = x[trind,], label = y,  nfold = k, nrounds=cv.nround)

set.seed(131)
nround <- 40 # or 40 based on the above bst.cv 

bst <- xgboost(param=param, data = x[trind,], label = y, nrounds=nround)
pred <- predict(bst,x[teind,]) 
pred <- matrix(pred,2,length(pred)/2)　# class (Fish_Con; Fish_Comp; Chicken_Con; Chicken_Comp; Pig_Con; Pig_Comp_ThB; Cattle_Con; Cattle_ThB)　　
pred <- t(pred)
colnames(pred)<-c("Non_seagrass","Seagrass") 
#2,CW_control(Chicken_Con);3,CW_test(Chicken_Comp);0,FE_control(Fish_Con);1,FE_test(Fish_Comp);6,PK_control(Pig_Con);7,PK_test(Pig_Comp_ThB);4,KC_control(Cattel_Con);5,KC_test(Cattle_ThB)

#shap_values <- shap.score.rank(xgb_model = bst,X_train = x[trind,],shap_approx = FALSE)



head(pred,2) #8 group/class

# preparation of test dataset
test.x <- FCPC.test[, 2:22] 
x_t <- rbind(test.x,FCPC.test[,-1]) 

y_0_t <- c(FCPC.test$Species)
y_1_t <- as.factor(y_0_t)
y_t <- as.integer(y_1_t)-1

x_t <- as.matrix(x_t)
trind_t <- 1:length(y_t) 
teind_t <- (nrow(test.x)+1):nrow(x_t) 

#複数の場合
param <- list("objective" = "multi:softmax", # modify the function "multi:softmax"
              "eval_metric" = "mlogloss", 
              "num_class" = 2 #class_No.
)

set.seed(131)
nround <- 40 # or 27 based on the above bst.cv
bst_t <- xgboost(param=param, data = x_t[trind_t,], label = y, nrounds=nround)
pred_t <- predict(bst_t,x_t[teind_t,])


#2 groups (Group_2 in Non_seagrass and seagrass)
x_1_f <- FCPC.test[,1]
for(i in 1:length(pred_t)){
  if(pred_t[i]==0) {pred_t[i]="Control"}
  else if(pred_t[i]==1) {pred_t[i]="Test"}#
}


#8 groups (Group_8 in Fig.S11)
x_1_f <- FCPC.test[,1]
for(i in 1:length(pred_t)){
  if(pred_t[i]==0) {pred_t[i]="Control"}
  else if(pred_t[i]==1) {pred_t[i]="Test"}
  else if(pred_t[i]==2) {pred_t[i]="Control"}
  else if(pred_t[i]==3) {pred_t[i]="Test"}
  else if(pred_t[i]==4) {pred_t[i]="Control"}
  else if(pred_t[i]==5) {pred_t[i]="Test"}
  else if(pred_t[i]==6) {pred_t[i]="Control"}
  else if(pred_t[i]==7) {pred_t[i]="Test"} #Pig_Comp_ThB
}

table(x_1_f,pred_t) # Please check 100% accuracy rate

sink('./Result_AA_RF_XG/XGboost_pre_x_1_f.txt', append = TRUE)
print (table(x_1_f,pred_t))
sink()

write.csv(table(x_1_f,pred_t),"./Result_AA_RF_XG/XGboost_pre_x_1_f.csv")

imp<-xgb.importance(names(y_1),model=bst_t)
print(imp)
xgb.plot.importance(imp) 

#marginが大きい場合
par("mar"=c(5,5,5,5))
xgb.plot.importance(imp) 

pdf ("./Result_AA_RF_XG/XGBoostgraph.pdf") 
xgb.plot.importance(imp) 
dev.off()

write.csv(print(imp),"./Result_AA_RF_XG/XGBoostgraph_raw.csv")

#SHAP
library(iml)

X <- train.x

dtrain <- xgb.DMatrix(data = as.matrix(X), label = y)
model <- xgb.train(param, dtrain, nrounds = 20, verbose = 0)

predict_function <- function(model, newdata) {
  pred <- predict(model, as.matrix(newdata))
  pred_matrix <- matrix(pred, ncol = 3, byrow = TRUE)
  return(pred_matrix[,1])  # クラス0の確率を返す例
}
predictor <- Predictor$new(model, data = X, y = y, predict.fun = predict_function)

# XGBoostのImportanceを取得 (Gain)
importance_matrix <- xgb.importance(feature_names = colnames(X), model = model)
imp_dt <- data.table(Feature = importance_matrix$Feature, Importance = importance_matrix$Gain)

# 3) SHAP値計算（単一サンプルごとに計算し平均を取る）
n_samples <- 132
shap_values_list <- lapply(1:n_samples, function(i) {
  shap <- Shapley$new(predictor, x.interest = X[i, ])
  shap$results$phi
})

shap_matrix <- do.call(rbind, shap_values_list)
mean_abs_shap <- colMeans(abs(shap_matrix))

shap_dt <- data.table(Feature = colnames(X), Importance = mean_abs_shap)


# 4) データ結合＆整形
df <- merge(imp_dt, shap_dt, by = "Feature", suffixes = c("_Gain", "_MeanAbsSHAP"))
df_long <- melt(df, id.vars = "Feature", variable.name = "Method", value.name = "Importance")


#データ排出
# --- 保存先ディレクトリ作成（なければ） ---
#dir.create("./Result_AA_RF_XG", showWarnings = FALSE, recursive = TRUE)

# --- Gainベース重要度のCSV出力 ---
fwrite(imp_dt, "./Result_AA_RF_XG/XGBoost_importance_gain.csv")

# --- SHAP平均絶対値のCSV出力 ---
fwrite(shap_dt, "./Result_AA_RF_XG/SHAP_importance_mean_abs.csv")

# --- 統合ワイド形式（Gain + SHAP）CSV出力 ---
fwrite(df, "./Result_AA_RF_XG/FeatureImportance_XGB_Gain_vs_SHAP.csv")

# --- ロング形式（グラフ用）CSV出力 ---
fwrite(df_long, "./Result_AA_RF_XG/FeatureImportance_XGB_LongFormat.csv")

# 5) グラフ描画（軸線クリア＆0位置にY軸）
ggplot(df_long, aes(x = reorder(Feature, Importance), y = Importance, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  coord_flip() +
  scale_x_discrete(expand = c(0, 0)) +     # X軸の余白なし（特徴量軸）
  scale_y_continuous(expand = c(0, 0)) +  # Y軸の余白なし（Importance軸）
  labs(title = "Feature Importance: XGBoost Gain vs Mean Absolute SHAP (iml)",
       x = "Feature", y = "Importance") +
  theme_minimal() +
  theme(
    axis.line.x = element_line(color = "black", size = 0.8),
    axis.line.y = element_line(color = "black", size = 0.8),
    axis.ticks = element_line(color = "black", size = 0.8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


# 5) グラフ描画（軸線クリア＆0位置にY軸）色違い
ggplot(df_long, aes(x = reorder(Feature, Importance), y = Importance, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  coord_flip() +
  scale_x_discrete(expand = c(0, 0)) +     # X軸の余白なし（特徴量軸）
  scale_y_continuous(expand = c(0, 0)) +           # Y軸（Importance軸）の余白をなくす
labs(title = "Feature Importance: XGBoost Gain vs Mean Absolute SHAP (iml)",
     x = "Feature", y = "Importance") +
scale_fill_manual(values = c("Importance_Gain" = "skyblue", "Importance_MeanAbsSHAP" = "darkorange")) +
theme_minimal() +
theme(
  axis.line.x = element_line(color = "black", size = 0.8),  # 横軸線
  axis.line.y = element_line(color = "black", size = 0.8),  # 縦軸線
  axis.ticks = element_line(color = "black", size = 0.8),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
)






#名前を変更する場合

library(dplyr)

# 1. カテゴリ名を統一（ここが重要）
df_long <- df_long %>%
  mutate(Method = case_when(
    Method == "Importance_Gain" ~ "Gain",
    Method == "Importance_MeanAbsSHAP"   ~ "SHAP",
    TRUE                     ~ Method
  ))

# 2. グラフ描画
ggplot(df_long, aes(x = reorder(Feature, Importance), y = Importance, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  coord_flip() +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Feature Importance: XGBoost Gain vs Importance_MeanAbsSHAP (iml)",
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

# --- XGBoost: Gain の単独グラフ ---
ggplot(imp_dt, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "XGBoost Feature Importance (Gain)",
       x = "Feature", y = "Gain") +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )
ggsave("./Result_AA_RF_XG/XGBoost_Gain_only.png", width = 9, height = 7, dpi = 300)


# --- XGBoost: SHAP の単独グラフ ---
ggplot(shap_dt, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  coord_flip() +
  labs(title = "XGBoost Feature Importance (SHAP)",
       x = "Feature", y = "Mean Absolute SHAP") +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )
ggsave("./Result_AA_RF_XG/XGBoost_SHAP_only.png", width = 9, height = 7, dpi = 300)


# PDFファイルを開く
pdf("./Result_AA_RF_XG/XGBoost_FeatureImportance.pdf", width = 9, height = 7)

# 1) Gain vs SHAP 並列棒グラフ（長い形式）
ggplot(df_long, aes(x = reorder(Feature, Importance), y = Importance, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  coord_flip() +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Feature Importance: XGBoost Gain vs Mean Absolute SHAP (iml)",
       x = "Feature", y = "Importance") +
  theme_minimal() +
  theme(
    axis.line.x = element_line(color = "black", size = 0.8),
    axis.line.y = element_line(color = "black", size = 0.8),
    axis.ticks = element_line(color = "black", size = 0.8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
print(last_plot())  # 明示的にプロット出力

# 2) Gain の単独棒グラフ
ggplot(imp_dt, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "XGBoost Feature Importance (Gain)",
       x = "Feature", y = "Gain") +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )
print(last_plot())

# 3) SHAP の単独棒グラフ
ggplot(shap_dt, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  coord_flip() +
  labs(title = "XGBoost Feature Importance (SHAP)",
       x = "Feature", y = "Mean Absolute SHAP") +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )
print(last_plot())

# PDFファイルを閉じる
dev.off()
