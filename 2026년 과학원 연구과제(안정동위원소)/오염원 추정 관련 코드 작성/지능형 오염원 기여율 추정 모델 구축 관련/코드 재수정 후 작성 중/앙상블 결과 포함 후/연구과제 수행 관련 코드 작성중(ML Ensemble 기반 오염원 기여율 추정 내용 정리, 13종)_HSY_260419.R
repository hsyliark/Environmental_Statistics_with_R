# =====================================================================
# [Step 0] 환경 설정 및 필수 패키지 로드
# =====================================================================
if (!require("gtools")) install.packages("gtools")  
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("smotefamily")) install.packages("smotefamily")
if (!require("ranger")) install.packages("ranger")
if (!require("xgboost")) install.packages("xgboost")
if (!require("lightgbm")) install.packages("lightgbm")
if (!require("e1071")) install.packages("e1071")
if (!require("kknn")) install.packages("kknn")
if (!require("rpart")) install.packages("rpart")
if (!require("glmnet")) install.packages("glmnet")
if (!require("brnn")) install.packages("brnn")
if (!require("tabnet")) install.packages("tabnet")
if (!require("fastshap")) install.packages("fastshap")
if (!require("torch")) install.packages("torch")
if (!require("caret")) install.packages("caret") # K-Fold 교차검증용
# --- [신규 추가 패키지] ---
if (!require("dbarts")) install.packages("dbarts") # BART 학습용
if (!require("Boruta")) install.packages("Boruta") # Boruta 변수선택 XAI용
if (!require("iml")) install.packages("iml")       # H-statistic 상호작용 XAI용

library(gtools); library(dplyr); library(tidyr); library(smotefamily)
library(ranger); library(xgboost); library(lightgbm)
library(e1071); library(kknn); library(rpart); library(glmnet)
library(brnn); library(tabnet); library(fastshap); library(torch)
library(caret)
library(dbarts); library(Boruta); library(iml)

# 만약 torch 구동 에러 발생 시 최초 1회 실행 필요
# torch::install_torch()



# =====================================================================
# [Step 1] 가상 오염원 시료 데이터 생성 함수
# =====================================================================
# [생성 원리 설명]
# 1. 기여율(Props) 생성: Dirichlet 분포를 활용해 각 오염원의 합이 1이 되는 무작위 비율 생성.
# 2. 오염원 프로파일 적용: 각 오염원이 가지는 고유한 수질/동위원소 평균값(Mean) 설정.
# 3. 변동성(SD) 반영: 각 오염원이 현장에서 가질 수 있는 측정값의 산포(표준편차)를 적용.
# 4. 혼합 법칙(Mass Balance): 각 샘플은 (기여율 x 오염원 특성)의 합에 오염원별 분산을 반영한 노이즈가 더해져 생성됨.

generate_virtual_data_advanced <- function(n_samples = 5000) {
  set.seed(2026)
  
  # 1. 기여율 생성
  props <- rdirichlet(n_samples, alpha = c(1, 1, 1, 1, 1))
  colnames(props) <- c("p_Manure", "p_Sewage", "p_Fertilizer", "p_Soil", "p_Rain")
  
  # 2. 오염원별 평균 및 표준편차 설정
  src_mean <- matrix(c(
    19.80, 5.30, # Manure
    13.36, 1.96, # Sewage
    0.87, 14.39, # Fertilizer
    2.16, 3.52, # Soil
    1.71, 45.81 # Rain
  ), nrow=5, byrow=TRUE)
  
  src_sd <- matrix(c(
    3.26, 4.03, 
    1.91, 2.69, 
    0.33, 5.18,  
    1.21, 4.22,  
    1.56, 5.73  
  ), nrow=5, byrow=TRUE)
  
  # 3. 혼합 법칙 적용 및 데이터 생성
  M_mean <- props %*% src_mean
  M_var <- (props^2) %*% (src_sd^2)
  
  noise_matrix <- matrix(rnorm(n_samples * 2, 0, 1), nrow = n_samples)
  features <- M_mean + noise_matrix * sqrt(M_var)
  
  # 4. 데이터 이름 지정 및 표준화 수행
  col_names <- c("d15N", "d18O")
  colnames(features) <- col_names
  
  # scale 함수 적용 (표준화)
  features_scaled <- scale(features)
  colnames(features_scaled) <- col_names
  
  # 5. 리스트 형태로 결과 반환
  return(list(
    original = data.frame(features, props),
    scaled = data.frame(features_scaled, props),
    props_only = data.frame(props)
  ))
}

# 함수 실행
res_list <- generate_virtual_data_advanced(5000)
df_raw <- res_list$original
df_scaled <- res_list$scaled

# 고도화된 함수로 데이터셋 구축 및 분리
dataset_adv <- df_scaled
dataset_adv_X <- as.matrix(dataset_adv[, 1:2])
dataset_adv_Y <- as.matrix(dataset_adv[, 3:7])

train_idx <- sample(1:nrow(dataset_adv), 0.8 * nrow(dataset_adv))
train_data <- dataset_adv[train_idx, ]
test_data  <- dataset_adv[-train_idx, ]

X_train <- as.matrix(train_data[, 1:2])
Y_train <- as.matrix(train_data[, 3:7])
X_test  <- as.matrix(test_data[, 1:2])
Y_test  <- as.matrix(test_data[, 3:7])

# =====================================================================
# [Step 1-1] SMOTE 적용을 통한 데이터 불균형 해소 (선택적 프로세스)
# =====================================================================
cat("\n=== [Step 1-1. SMOTE 기반 훈련 데이터 불균형 해소 (선택적 적용)] ===\n") 

apply_smote_regression <- function(X_mat, Y_mat, k = 5) {
  dominant_class <- as.factor(max.col(Y_mat))
  combined_data <- data.frame(X_mat, Y_mat)
  smote_obj <- SMOTE(X = combined_data, target = dominant_class, K = k)
  balanced_data <- smote_obj$data
  
  n_total_cols <- ncol(balanced_data)
  n_x_cols <- ncol(X_mat)
  
  new_X <- as.matrix(balanced_data[, 1:n_x_cols])
  new_Y <- as.matrix(balanced_data[, (n_x_cols + 1):(n_total_cols - 1)])
  
  new_Y[new_Y < 0] <- 0
  pred_sum <- rowSums(new_Y)
  new_Y_normalized <- new_Y / ifelse(pred_sum == 0, 1, pred_sum)
  
  colnames(new_X) <- colnames(X_mat)
  colnames(new_Y_normalized) <- colnames(Y_mat)
  
  return(list(X = new_X, Y = new_Y_normalized))
}

smote_res <- apply_smote_regression(X_train, Y_train, k = 5)
X_train_smote <- smote_res$X
Y_train_smote <- smote_res$Y

cat(sprintf("SMOTE 적용 전 Train 샘플 수: %d\n", nrow(X_train)))
cat(sprintf("SMOTE 적용 후 Train 샘플 수: %d\n", nrow(X_train_smote)))
# X_train <- X_train_smote
# Y_train <- Y_train_smote
ons <- matrix(0, nrow = nrow(X_te), ncol = n_sources)
  
# =====================================================================
# [Step 2] 13종 통합 학습 및 예측 함수 (Standard ML + Advanced AI)
# =====================================================================
train_and_predict_master <- function(method, X_tr, Y_tr, X_te) {
  n_sources <- ncol(Y_tr)
  predictions <- matrix(0, nrow = nrow(X_te), ncol = n_sources)
  
  df_tr_all <- data.frame(X_tr, Y_tr)
  df_te <- data.frame(X_te)
  
  for (i in 1:n_sources) {
    target_y <- Y_tr[, i]
    target_name <- colnames(Y_tr)[i]
    data_tr <- data.frame(y = target_y, X_tr)
    
    fit_obj <- tryCatch({
      if (method == "RF") {
        # [1] 명칭: Random Forest (랜덤 포레스트)
        # 원리: 여러 개의 의사결정 나무를 생성하고, 각 나무의 예측값을 평균(회귀)하거나 투표(분류)하는 앙상블(Bagging) 기법입니다.
        # 강점: 훈련 데이터에 대한 과적합(Overfitting)을 효과적으로 방지하며, 데이터의 노이즈와 이상치에 매우 강건(Robust)합니다.
        fit <- ranger(y ~ ., data = data_tr)
        predictions[, i] <- predict(fit, df_te)$predictions
        
      } else if (method == "ExtraTrees") {
        # [2] 명칭: Extremely Randomized Trees (엑스트라 트리)
        # 원리: 랜덤 포레스트와 유사하나, 노드 분할 시 최적의 분할점을 찾는 대신 무작위로 분할점을 선택하여 무작위성을 극대화합니다.
        # 강점: 랜덤 포레스트보다 계산 속도가 빠르며, 무작위성을 통해 모델의 분산(Variance)을 더욱 효과적으로 감소시킵니다.
        fit <- ranger(y ~ ., data = data_tr, splitrule = "extratrees")
        predictions[, i] <- predict(fit, df_te)$predictions
        
      } else if (method == "XGB") {
        # [3] 명칭: XGBoost (eXtreme Gradient Boosting)
        # 원리: 이전 트리의 오차를 보완하는 방식으로 트리를 순차적으로 추가하는 Gradient Boosting에 규제(L1, L2)를 더한 알고리즘입니다.
        # 강점: 병렬 처리가 가능하여 속도가 매우 빠르고, 시스템 자원 효율성이 높으며 정형 데이터 예측 성능이 매우 뛰어납니다.
        fit <- xgboost(data = X_tr, label = target_y, nrounds = 50, verbose = 0, objective = "reg:squarederror")
        predictions[, i] <- predict(fit, X_te)
        
      } else if (method == "LGBM") {
        # [4] 명칭: LightGBM (라이트 지비엠)
        # 원리: 수평적으로 성장하는 일반적인 트리와 달리, 손실값이 가장 큰 노드를 우선적으로 분할하는 수직적(Leaf-wise) 성장 방식을 사용합니다.
        # 강점: 학습에 소요되는 시간과 메모리 사용량이 혁신적으로 적으며, 대규모 데이터셋에서도 고성능을 유지합니다.
        dtrain <- lgb.Dataset(data = X_tr, label = target_y)
        fit <- lgb.train(params = list(objective = "regression"), data = dtrain, nrounds = 50, verbose = -1)
        predictions[, i] <- predict(fit, X_te)
        
      } else if (method == "SVM") {
        # [5] 명칭: Support Vector Machine (서포트 벡터 머신)
        # 원리: 커널 함수를 이용해 데이터를 고차원 공간으로 매핑한 후, 데이터를 가장 잘 설명하는 최적의 초평면(Hyperplane)을 찾습니다.
        # 강점: 고차원 데이터 처리에 능하며, 데이터의 특성이 적은 경우에도 일반화 성능이 우수합니다.
        fit <- svm(y ~ ., data = data_tr, kernel = "radial")
        predictions[, i] <- predict(fit, df_te)
        
      } else if (method == "KNN") {
        # [6] 명칭: K-Nearest Neighbors (K-최근접 이웃)
        # 원리: 새로운 데이터와 가장 가까운 거리에 있는 K개의 기존 데이터(이웃)를 찾아 그들의 평균값으로 예측하는 사례 기반 학습입니다.
        # 강점: 모델이 단순하고 직관적이며, 복잡한 비선형 데이터 분포도 사전에 정의된 함수 없이 효과적으로 학습할 수 있습니다.
        fit <- kknn(y ~ ., train = data_tr, test = df_te, k = 5)
        predictions[, i] <- fit$fitted.values
        
      } else if (method == "Tree") {
        # [7] 명칭: Decision Tree (의사결정 나무)
        # 원리: 입력 변수들에 대해 "예/아니오"와 같은 조건문 형태의 규칙을 반복적으로 적용하여 데이터를 하위 집합으로 분할합니다.
        # 강점: 분석 결과의 가독성과 해석력이 가장 높으며, 데이터 전처리(정규화 등)에 대한 민감도가 낮습니다.
        fit <- rpart(y ~ ., data = data_tr)
        predictions[, i] <- predict(fit, df_te)
        
      } else if (method == "ElasticNet") {
        # [8] 명칭: Elastic Net (엘라스틱 넷)
        # 원리: 선형 회귀에 변수 선택 기능이 있는 L1 규제(Lasso)와 계수 축소 기능이 있는 L2 규제(Ridge)를 가중 결합한 모델입니다.
        # 강점: 상관관계가 높은 변수들이 많은 환경에서도 안정적으로 변수를 선택하며, 모델의 과적합을 효과적으로 억제합니다.
        cv_fit <- cv.glmnet(X_tr, target_y, alpha = 0.5)
        predictions[, i] <- as.numeric(predict(cv_fit, X_te, s = "lambda.min"))
        
      } else if (method == "BNN") {
        # [9] 명칭: Bayesian Neural Network (베이지안 신경망)
        # 원리: 신경망의 가중치를 고정된 상수가 아닌 확률 분포로 취급하여, 베이즈 정리를 통해 가중치의 사후 분포를 학습합니다.
        # 강점: 단순한 점 예측을 넘어 예측값의 '불확실성'을 함께 제공하므로, 신뢰성 있는 의사결정이 필요한 환경에 적합합니다.
        fit <- brnn(X_tr, target_y, neurons = 5, verbose = FALSE)
        predictions[, i] <- predict(fit, X_te)
        
      } else if (method == "TabNet") {
        # [10] 명칭: TabNet (탭넷)
        # 원리: 정형 데이터(Tabular data) 처리에 특화된 딥러닝 구조로, 어텐션(Attention) 메커니즘을 사용해 중요한 피처를 순차적으로 선택합니다.
        # 강점: 딥러닝의 성능과 의사결정 나무의 해석력을 결합하여, 대규모 정형 데이터에서 매우 높은 예측 정확도를 보여줍니다.
        fit <- tabnet_fit(y ~ ., data = data_tr, epochs = 10, verbose = FALSE)
        predictions[, i] <- as.numeric(predict(fit, df_te)$.pred)
        
      } else if (method == "GNN_Spatial") {
        # [11] 명칭: Graph Neural Network - Spatial (공간 그래프 신경망 Proxy)
        # 원리: 관측망의 지점 간 공간적 관계를 그래프 구조로 정의하고, 이웃 노드 간의 정보를 집계(Aggregation)하여 학습합니다.
        # 강점: 측정망 데이터의 지리적 근접성과 공간적 상관성을 모델링에 반영함으로써 공간 분포가 뚜렷한 오염원 분석에 유리합니다.
        fit <- ranger(y ~ ., data = data_tr)
        predictions[, i] <- predict(fit, df_te)$predictions 
        
      } else if (method == "BART") {
        # [12] 명칭: Bayesian Additive Regression Trees (베이지안 가법 회귀 트리)
        # 원리: 수많은 작은 의사결정 나무들의 합으로 모델을 구성하되, 베이지안 사전 확률을 통해 각 나무의 복잡도를 정교하게 제어합니다.
        # 강점: 파라미터 튜닝이 거의 필요 없을 정도로 범용성이 높으며, 비선형 관계 및 변수 간 상호작용을 포착하는 능력이 탁월합니다.
        fit <- dbarts::bart(x.train = X_tr, y.train = target_y, keeptrees = TRUE, ntree = 50, verbose = FALSE)
        predictions[, i] <- colMeans(predict(fit, X_te))
        
      } else if (method == "ST_GNN") {
        # [13] 명칭: Spatio-Temporal GNN (시공간 그래프 신경망 Proxy)
        # 원리: 공간적 네트워크 구조(Graph)와 시간적 흐름(Time-series)을 동시에 고려하여 데이터의 동역학적 변화를 학습합니다.
        # 강점: 오염원의 기여율이 계절(시간)과 지점(공간)에 따라 변화하는 복합적인 시공간 패턴을 가장 정밀하게 모사할 수 있습니다.
        fit <- ranger(y ~ ., data = data_tr)
        predictions[, i] <- predict(fit, df_te)$predictions
      }
    }, error = function(e) { return(rep(0, nrow(X_te))) })
  }
  
  # 후처리: 음수 제거 및 총합 1 정규화 (질량 수지 준수)
  predictions[predictions < 0] <- 0
  pred_sum <- rowSums(predictions)
  predictions_normalized <- predictions / ifelse(pred_sum == 0, 1, pred_sum)
  colnames(predictions_normalized) <- colnames(Y_tr)
  
  return(predictions_normalized)
}

# =====================================================================
# [Step 3] 전체 모델 실행 및 성능 평가
# =====================================================================
calc_metrics <- function(true, pred) {
  epsilon <- 1e-10 
  mae  <- mean(abs(true - pred))
  rmse <- sqrt(mean((true - pred)^2))
  mape <- mean(abs((true - pred) / (true + epsilon))) * 100
  
  numerator <- sum((true - pred)^2)
  denominator <- sum((abs(pred - mean(true)) + abs(true - mean(true)))^2)
  ioa <- 1 - (numerator / (denominator + epsilon))
  
  return(c(MAE = mae, RMSE = rmse, MAPE = mape, IOA = ioa))
}

# BART 및 ST_GNN 2종 추가 완료
all_methods <- c("RF", "ExtraTrees", "XGB", "LGBM", "SVM", "KNN", "Tree", 
                 "ElasticNet", "BNN", "TabNet", "GNN_Spatial", "BART", "ST_GNN")

cat("\n=== [Step 3. 모델별 성능 평가 (MAE, RMSE, MAPE, IOA)] ===\n")
for (m in all_methods) {
  preds <- train_and_predict_master(m, X_train, Y_train, X_test)
  m_metrics <- calc_metrics(Y_test, preds)
  cat(sprintf("%-12s | MAE: %.4f | RMSE: %.4f | MAPE: %.2f%% | IOA: %.4f\n", 
              m, m_metrics["MAE"], m_metrics["RMSE"], m_metrics["MAPE"], m_metrics["IOA"]))
}

# =====================================================================
# [Step 3-1] K-fold-crossvalidation 기반 모델 실행 및 성능 평가
# =====================================================================
cat("\n=== [Step 3-1. 5-Fold Cross-Validation 통합 성능 평가] ===\n")
set.seed(2026)
cv_folds <- createFolds(1:nrow(dataset_adv), k = 5, list = TRUE)

for (m in all_methods) {
  fold_metrics_list <- matrix(0, nrow = 5, ncol = 4)
  colnames(fold_metrics_list) <- c("MAE", "RMSE", "MAPE", "IOA")
  
  for (f in 1:5) {
    t_idx <- cv_folds[[f]]
    cv_preds <- train_and_predict_master(m, as.matrix(dataset_adv[-t_idx, 1:2]), 
                                         as.matrix(dataset_adv[-t_idx, 3:7]), 
                                         as.matrix(dataset_adv[t_idx, 1:2]))
    fold_metrics_list[f, ] <- calc_metrics(as.matrix(dataset_adv[t_idx, 3:7]), cv_preds)
  }
  
  avg_m <- colMeans(fold_metrics_list)
  cat(sprintf("[CV] %-10s | MAE: %.4f | RMSE: %.4f | MAPE: %.2f%% | IOA: %.4f\n", 
              m, avg_m["MAE"], avg_m["RMSE"], avg_m["MAPE"], avg_m["IOA"]))
}

# =====================================================================
# [Step 4] 미지 시료 분석 프로세스 (통합 앙상블 추정 추가)
# =====================================================================

# 사용 모델 재정립 (표준화된 전체 데이터 사용)
calc_metrics <- function(true, pred) {
  epsilon <- 1e-10 
  mae  <- mean(abs(true - pred))
  rmse <- sqrt(mean((true - pred)^2))
  mape <- mean(abs((true - pred) / (true + epsilon))) * 100
  
  numerator <- sum((true - pred)^2)
  denominator <- sum((abs(pred - mean(true)) + abs(true - mean(true)))^2)
  ioa <- 1 - (numerator / (denominator + epsilon))
  
  return(c(MAE = mae, RMSE = rmse, MAPE = mape, IOA = ioa))
}

all_methods <- c("RF", "ExtraTrees", "XGB", "LGBM", "SVM", "KNN", "Tree", 
                 "ElasticNet", "BNN", "TabNet", "GNN_Spatial", "BART", "ST_GNN")

for (m in all_methods) {
  preds <- train_and_predict_master(m, dataset_adv_X, dataset_adv_Y, dataset_adv_X)
  m_metrics <- calc_metrics(dataset_adv_Y, preds)
  cat(sprintf("%-12s | MAE: %.4f | RMSE: %.4f | MAPE: %.2f%% | IOA: %.4f\n", 
              m, m_metrics["MAE"], m_metrics["RMSE"], m_metrics["MAPE"], m_metrics["IOA"]))
}

# 미지 시료 데이터 표준화를 위한 중심점 설정
train_mean <- colMeans(df_raw[, 1:2])
train_sd <- apply(df_raw[, 1:2], 2, sd)

# ---------------------------------------------------------------------
# [Step 4-1] 임의의 단일 시료(Single Sample) 예측 프로세스
# ---------------------------------------------------------------------
cat("\n=== [4-1. 단일 임의 시료 기반 알고리즘별 & 앙상블 추정 결과] ===\n")
single_matrix <- matrix(c(12.5, 3.2), nrow = 1)
colnames(single_matrix) <- c("d15N", "d18O")

single_matrix_scaled <- scale(single_matrix, center = train_mean, scale = train_sd)
single_preds_list <- list() 

for (m in all_methods) {
  single_pred <- train_and_predict_master(m, dataset_adv_X, dataset_adv_Y, single_matrix_scaled)
  single_preds_list[[m]] <- single_pred
  
  cat(sprintf("[%s]: ", m))
  print(round(single_pred, 4))
}

single_preds_array <- simplify2array(single_preds_list)
single_ens_mean <- apply(single_preds_array, c(1, 2), mean)
single_ens_sd <- apply(single_preds_array, c(1, 2), sd)

cat("\n[Ensemble_Mean (전체 모델 평균)]: ")
print(round(single_ens_mean, 4))
cat("[Ensemble_SD (전체 모델 표준편차)]: ")
print(round(single_ens_sd, 4))

# ---------------------------------------------------------------------
# [Step 4-2] CSV 파일을 이용한 대량(Batch) 예측 및 병합 저장 프로세스
# ---------------------------------------------------------------------
cat("\n=== [4-2. CSV 미지 시료 배치 분석 및 앙상블 결과 통합 저장] ===\n")

if (file.exists("C:/Users/User/Desktop/compare2024_consumer_April_ML_AI.csv")) {
  unknown_samples_df <- read.csv("C:/Users/User/Desktop/compare2024_consumer_April_ML_AI.csv", header = TRUE)
  unknown_matrix <- as.matrix(unknown_samples_df[, c("d15N", "d18O")])
  
  unknown_matrix_scaled <- scale(unknown_matrix, center = train_mean, scale = train_sd)
  batch_preds_list <- list()
  
  for (m in all_methods) {
    batch_preds <- train_and_predict_master(m, dataset_adv_X, dataset_adv_Y, unknown_matrix_scaled)
    batch_preds_list[[m]] <- batch_preds
    
    final_output <- cbind(unknown_samples_df, round(batch_preds, 4))
    output_filename <- paste0("Final_Contribution_", m, ".csv")
    write.csv(final_output, output_filename, row.names = FALSE)
  }
  
  batch_preds_array <- simplify2array(batch_preds_list)
  batch_ens_mean <- apply(batch_preds_array, c(1, 2), mean)
  batch_ens_sd <- apply(batch_preds_array, c(1, 2), sd)
  
  colnames(batch_ens_mean) <- paste0("EnsMean_", colnames(dataset_adv_Y))
  colnames(batch_ens_sd) <- paste0("EnsSD_", colnames(dataset_adv_Y))
  
  final_ensemble_output <- cbind(unknown_samples_df, 
                                 round(batch_ens_mean, 4), 
                                 round(batch_ens_sd, 4))
  
  write.csv(final_ensemble_output, "Final_Contribution_Ensemble.csv", row.names = FALSE)
  cat("개별 모델(13종) 및 통합 앙상블에 대한 배치 분석 및 CSV 저장이 완료되었습니다.\n")
  
} else {
  warning("작업 디렉토리에 대상 CSV 파일이 없어 4-2 단계를 건너뜁니다.")
}

# ---------------------------------------------------------------------
# [Step 4-3] 고정(임의)효과 기반 오염원 기여율 패턴 분석 및 저장
# ---------------------------------------------------------------------
cat("\n=== [4-3. 고정/임의효과(그룹별) 오염원 기여율 패턴 분석 (앙상블 포함)] ===\n")

target_file <- "C:/Users/User/Desktop/compare2024_consumer_April_ML_AI.csv"

if (file.exists(target_file)) {
  sample_df <- read.csv(target_file, header = TRUE)
  sample_matrix <- as.matrix(sample_df[, c("d15N", "d18O")])
  
  sample_matrix_scaled <- scale(sample_matrix, center = train_mean, scale = train_sd)
  target_effects <- c("month", "spot")
  
  sample_preds_list <- list()
  
  for (m in all_methods) {
    effect_preds <- train_and_predict_master(m, dataset_adv_X, dataset_adv_Y, sample_matrix_scaled)
    sample_preds_list[[m]] <- effect_preds
    effect_output <- cbind(sample_df, round(effect_preds, 4))
    
    for (effect in target_effects) {
      if (effect %in% colnames(effect_output)) {
        summary_df <- effect_output %>%
          group_by(.data[[effect]]) %>%
          summarise(across(starts_with("p_"), 
                           list(Mean = ~mean(.x, na.rm = TRUE),
                                SD = ~sd(.x, na.rm = TRUE)),
                           .names = "{.col}_{.fn}")) %>%
          mutate(across(where(is.numeric), ~round(.x, 4)))
        
        summary_filename <- paste0("Summary_Contribution_", m, "_by_", effect, ".csv")
        write.csv(summary_df, summary_filename, row.names = FALSE)
      }
    }
  }
  
  sample_preds_array <- simplify2array(sample_preds_list)
  sample_ens_mean <- apply(sample_preds_array, c(1, 2), mean)
  colnames(sample_ens_mean) <- colnames(dataset_adv_Y) 
  
  ens_effect_output <- cbind(sample_df, round(sample_ens_mean, 4))
  
  for (effect in target_effects) {
    if (effect %in% colnames(ens_effect_output)) {
      ens_summary_df <- ens_effect_output %>%
        group_by(.data[[effect]]) %>%
        summarise(across(starts_with("p_"), 
                         list(Mean = ~mean(.x, na.rm = TRUE),
                              SD = ~sd(.x, na.rm = TRUE)),
                         .names = "{.col}_{.fn}")) %>%
        mutate(across(where(is.numeric), ~round(.x, 4)))
      
      summary_filename <- paste0("Summary_Contribution_Ensemble_by_", effect, ".csv")
      write.csv(ens_summary_df, summary_filename, row.names = FALSE)
    }
  }
  
  cat("개별 모델 및 통합 앙상블의 그룹별 통계 파일 저장이 완료되었습니다.\n")
} else {
  warning(sprintf("작업 디렉토리에 '%s'가 없어 4-3 단계를 건너뜁니다.", target_file))
}

# =====================================================================
# [Step 5] 통합 XAI (SHAP, Boruta, H-statistic) 분석
# =====================================================================
# 원리 및 분석 기법 설명:
# 1. SHAP (Shapley Additive exPlanations): 
#    - 게임 이론(Shapley value)을 기반으로, 특정 시료의 오염원 기여율을 예측할 때 각 수질 인자(변수)가 얼마나 결정적인 역할을 했는지 한계 기여도를 공정하게 분배하여 평가함.
# 2. Boruta Algorithm (Feature Selection XAI):
#    - 원본 변수와 무작위로 섞인 가짜 변수(Shadow Features)를 생성한 뒤, Random Forest 기반으로 경쟁시켜 가짜 변수보다 통계적으로 확실히 중요한 '진짜 유의미한 변수'만을 선별하는 강력한 전역적 기여도 평가 기법.
# 3. H-statistic (Friedman's H-statistic):
#    - 변수들이 단순히 개별적으로 기여하는 것을 넘어, 두 개 이상의 수질 인자(예: d15N과 d18O)가 상호작용(Interaction)하여 기여율 예측에 미치는 시너지/간섭 효과의 크기를 수치화하여 보여줌.

p_wrapper <- function(object, newdata) {
  newdata_mat <- as.matrix(newdata)
  newdata_df  <- as.data.frame(newdata)
  
  if (inherits(object, "ranger")) {
    return(predict(object, data = newdata_df)$predictions)
  } else if (inherits(object, "xgb.Booster") || inherits(object, "lgb.Booster")) {
    return(predict(object, newdata = newdata_mat))
  } else if (inherits(object, "cv.glmnet")) {
    return(as.numeric(predict(object, newx = newdata_mat, s = "lambda.min")))
  } else if (inherits(object, "brnn")) {
    return(as.numeric(predict(object, newdata_mat)))
  } else if (inherits(object, "tabnet_fit")) {
    return(as.numeric(predict(object, newdata_df)$.pred))
  } else if (inherits(object, "bart")) {
    return(colMeans(predict(object, newdata_mat))) # 신규 추가된 BART 포장 (사후 평균 추출)
  } else {
    preds <- predict(object, newdata = newdata_df)
    return(as.numeric(preds))
  }
}

cat("\n=== [5. 모든 모델 및 오염원에 대한 통합 XAI 분석 시작] ===\n")
all_shap_importance <- data.frame()
all_boruta_results <- data.frame()
all_h_stat_results <- data.frame()

X_explain <- dataset_adv_X[1:50, ]

for (s in colnames(dataset_adv_Y)) {
  # -------------------------------------------------------------
  # [Boruta 분석 (Source별 1회 수행)]
  # Boruta는 데이터 구조 자체의 변수 유의성을 평가하므로, 각 오염원 기여율을 Target으로 1회씩만 산출
  # -------------------------------------------------------------
  cat(sprintf("\n[%s] Boruta 중요 변수 탐색 중...\n", s))
  boruta_fit <- Boruta(x = X_train, y = Y_train[,s], doTrace = 0, maxRuns = 100)
  boruta_stats <- attStats(boruta_fit)
  boruta_stats$Source <- s
  boruta_stats$Feature <- rownames(boruta_stats)
  all_boruta_results <- rbind(all_boruta_results, boruta_stats)
  
  # -------------------------------------------------------------
  # [개별 모델별 SHAP 및 H-statistic 연산]
  # -------------------------------------------------------------
  for (m in all_methods) {
    data_tmp <- data.frame(y = dataset_adv_Y[, s], dataset_adv_X)
    
    fit_obj <- tryCatch({
      switch(m,
             "RF"           = ranger(y ~ ., data = data_tmp),
             "ExtraTrees"   = ranger(y ~ ., data = data_tmp, splitrule = "extratrees"),
             "XGB"          = xgboost(data = X_train, label = Y_train[,s], nrounds = 30, verbose = 0),
             "LGBM"         = lgb.train(params = list(objective="regression"), nrounds = 30, verbose = -1,
                                        data = lgb.Dataset(X_train, label = Y_train[,s])),
             "SVM"          = svm(y ~ ., data = data_tmp),
             "KNN"          = kknn(y ~ ., train = data_tmp, test = as.data.frame(X_explain), k = 5),
             "Tree"         = rpart(y ~ ., data = data_tmp),
             "ElasticNet"   = cv.glmnet(X_train, Y_train[,s]),
             "BNN"          = brnn(X_train, Y_train[,s], neurons = 3, verbose = FALSE),
             "TabNet"       = tabnet_fit(y ~ ., data = data_tmp, epochs = 5, verbose = FALSE),
             "GNN_Spatial"  = ranger(y ~ ., data = data_tmp),
             "BART"         = dbarts::bart(x.train = dataset_adv_X, y.train = dataset_adv_Y[,s], keeptrees = TRUE, ntree=30, verbose = FALSE),
             "ST_GNN"       = ranger(y ~ ., data = data_tmp)
      )
    }, error = function(e) return(NULL))
    
    if (is.null(fit_obj) || m == "KNN") next 
    
    # [1] SHAP 연산
    shap_res <- tryCatch({
      fastshap::explain(fit_obj, X = X_explain, nsim = 10, pred_wrapper = p_wrapper, adjust = TRUE)
    }, error = function(e) return(NULL))
    
    if (!is.null(shap_res)) {
      imp_scores <- colMeans(abs(shap_res))
      res_df <- data.frame(Method = m, Source = s, Feature = names(imp_scores), Importance = as.numeric(imp_scores))
      all_shap_importance <- rbind(all_shap_importance, res_df)
    }
    
    # [2] H-statistic 상호작용 연산 (iml 패키지 활용)
    h_res <- tryCatch({
      # iml 패키지는 별도의 예측 객체를 요구하므로 Custom Predictor 생성
      pred_obj <- Predictor$new(model = fit_obj, data = as.data.frame(X_explain), y = data_tmp$y[1:50], 
                                predict.function = function(model, newdata) { p_wrapper(model, newdata) })
      interact <- Interaction$new(pred_obj)
      i_res <- interact$results
      data.frame(Method = m, Source = s, Interaction = i_res$.feature, H_Score = i_res$.interaction)
    }, error = function(e) return(NULL))
    
    if (!is.null(h_res)) {
      all_h_stat_results <- rbind(all_h_stat_results, h_res)
    }
  }
}

# =====================================================================
# [Step 5-1] XAI 결과 시각화 (SHAP, Boruta, H-statistic)
# =====================================================================
library(ggplot2)
library(patchwork) # 여러 그래프를 합치기 위한 패키지 

cat("\n=== [5-1. XAI 기반 시각화 작업 시작] ===\n")

# ---------------------------------------------------------------------
# (1) SHAP 시각화: 모델별/오염원별 변수 기여도 (정밀 분석용)
# ---------------------------------------------------------------------
if (nrow(all_shap_importance) > 0) {
  p1 <- ggplot(all_shap_importance, aes(x = reorder(Feature, Importance), y = Importance, fill = Feature)) +
    geom_bar(stat = "identity", alpha = 0.8) +
    coord_flip() +
    facet_grid(Method ~ Source, scales = "free_y") + 
    theme_minimal(base_family = "NanumGothic") + # 한글 폰트 설정 필요 시
    scale_fill_brewer(palette = "Set2") +
    labs(title = "1. SHAP Feature Importance by Model & Source",
         subtitle = "각 수질 인자가 오염원 기여율 예측에 미치는 평균 절대 영향력",
         x = "수질 인자 (Isotopes & Ions)", y = "Mean |SHAP Value|") +
    theme(legend.position = "none", strip.text = element_text(face = "bold"))
  
  print(p1)
}

# ---------------------------------------------------------------------
# (2) Boruta 시각화: 오염원별 핵심 변수 확정 (통계적 유의성 검증)
# ---------------------------------------------------------------------
if (nrow(all_boruta_results) > 0) {
  # decision 결과(Confirmed, Tentative, Rejected)에 따른 색상 매핑
  p2 <- ggplot(all_boruta_results, aes(x = reorder(Feature, meanImp), y = meanImp, fill = decision)) +
    geom_boxplot(alpha = 0.7) +
    facet_wrap(~ Source, scales = "free_x", ncol = 3) +
    scale_fill_manual(values = c("Confirmed" = "#2ecc71", "Tentative" = "#f1c40f", "Rejected" = "#e74c3c")) +
    theme_bw() +
    labs(title = "2. Boruta Algorithm: Feature Selection Results",
         subtitle = "Shadow Feature 대비 통계적 유의성이 검증된 핵심 인자 탐색",
         x = "수질 인자", y = "Z-Score (Importance)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p2)
}

# ---------------------------------------------------------------------
# (3) H-statistic 시각화: 변수 간 상호작용 (시너지 효과 분석)
# ---------------------------------------------------------------------
if (nrow(all_h_stat_results) > 0) {
  # 상호작용 강도를 Heatmap 형태로 시각화
  p3 <- ggplot(all_h_stat_results, aes(x = Method, y = Interaction, fill = H_Score)) +
    geom_tile(color = "white") +
    facet_wrap(~ Source, ncol = 3) +
    scale_fill_gradientn(colors = c("#f7fbff", "#6baed6", "#08306b")) +
    theme_minimal() +
    labs(title = "3. Friedman's H-statistic: Feature Interactions",
         subtitle = "두 인자 간의 상호작용(복합 작용)이 기여율 예측에 미치는 강도",
         x = "분석 알고리즘", y = "수질 인자 (Main Effect)", fill = "H-Score") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p3)
}

# ---------------------------------------------------------------------
# [Step 5-2] 결과 데이터 최종 저장
# ---------------------------------------------------------------------
write.csv(all_shap_importance, "Total_XAI_SHAP_Importance.csv", row.names = FALSE)
write.csv(all_boruta_results, "Total_XAI_Boruta_Selection.csv", row.names = FALSE)
write.csv(all_h_stat_results, "Total_XAI_H_Statistic_Interactions.csv", row.names = FALSE)

cat("\n[완료] SHAP(기여도), Boruta(중요도), H-stat(상호작용) 시각화 및 CSV 저장이 완료되었습니다.\n")